{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module QCL (Value, evalQCL) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonMap
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Foldable
import Data.Functor
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sortBy, unfoldr)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Merge.Strict as MM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid (First (..))
import Data.Ord (Down (..), comparing)
import Data.Scientific (fromFloatDigits)
import Data.Semigroup (Max (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Read as TR
import Data.Vector (fromList)
import qualified Text.Earley as E

data Positioned a = Positioned {pBegin :: (Int, Int), pEnd :: (Int, Int), pValue :: a}
  deriving (Functor, Eq, Ord)

instance (Show a) => Show (Positioned a) where
  showsPrec prec Positioned {..} = showsPrec prec pValue

type PositionedText = Positioned Text

type PositionedExpr = Positioned Expr

type PositionedTupleExpr = Positioned TupleExpr

type PositionedRowExpr = Positioned RowExpr

data Expr
  = Number Double
  | Boolean Bool
  | Plus PositionedExpr PositionedExpr
  | Minus PositionedExpr PositionedExpr
  | Mul PositionedExpr PositionedExpr
  | Div PositionedExpr PositionedExpr
  | Mod PositionedExpr PositionedExpr
  | IDiv PositionedExpr PositionedExpr
  | UnaryPlus PositionedExpr
  | UnaryMinus PositionedExpr
  | Not PositionedExpr
  | Lt PositionedExpr PositionedExpr
  | Le PositionedExpr PositionedExpr
  | Gt PositionedExpr PositionedExpr
  | Ge PositionedExpr PositionedExpr
  | Eq PositionedExpr PositionedExpr
  | Neq PositionedExpr PositionedExpr
  | And PositionedExpr PositionedExpr
  | Or PositionedExpr PositionedExpr
  | Tuple [PositionedTupleExpr]
  | TupleUpdate PositionedExpr (Positioned [PositionedTupleExpr])
  | AbstractTuple (Positioned ()) (Positioned [PositionedTupleExpr])
  | EvalAbstract PositionedExpr (Positioned ())
  | List [PositionedExpr]
  | Var PositionedText VariableLookupScope
  | Member PositionedExpr PositionedText
  deriving (Show)

data VariableLookupScope
  = VariableLookupCurrentScope
  | VariableLookupAllScopes
  deriving (Show)

data RowAttribute = RowFinal | RowPrivate
  deriving (Show)

data TupleExpr
  = Row PositionedText PositionedRowExpr
  | Assertion PositionedExpr
  deriving (Show)

data RowExpr
  = ValuedRow (Maybe (Positioned RowAttribute)) PositionedExpr
  | DeleteRow
  | AbstractRow
  deriving (Show)

tokenize :: Text -> [PositionedText]
tokenize = concatMap tokenizeLine . zip [1 ..] . T.lines
  where
    tokenizeLine :: (Int, Text) -> [PositionedText]
    tokenizeLine (lineno, line) = unfoldr recognizeOne (1, line)
      where
        recognizeOne :: (Int, Text) -> Maybe (PositionedText, (Int, Text))
        recognizeOne (colno, t) =
          case T.uncons t of
            Nothing -> Nothing
            Just ('#', _) -> Nothing
            Just (h, r)
              | isSpace h -> recognizeOne (1 + colno, r)
              | isAlpha h ->
                  let (tok, remaining) = T.span isAlphaNum t
                      newcolno = colno + T.length tok
                   in Just (Positioned (lineno, colno) (lineno, newcolno) tok, (newcolno, remaining))
              | isDigit h ->
                  case TR.double t of
                    Right (_, remaining) ->
                      let len = T.length t - T.length remaining
                          newcolno = colno + len
                       in Just (Positioned (lineno, colno) (lineno, newcolno) (T.take len t), (newcolno, remaining))
                    Left _ -> error "unexpected error while tokenizing"
              | Just op <- find (`T.isPrefixOf` t) multiCharPunct ->
                  let newcolno = colno + T.length op in Just (Positioned (lineno, colno) (lineno, newcolno) op, (newcolno, T.drop (T.length op) t))
              | otherwise ->
                  let newcolno = colno + 1 in Just (Positioned (lineno, colno) (lineno, newcolno) (T.singleton h), (newcolno, r))

multiCharPunct :: [Text]
multiCharPunct = ["&&", "||", "==", "!=", "<=", ">=", "//=", "//", "+=", "-=", "*=", "/=", "%="]

reservedWords :: [Text]
reservedWords = ["true", "false", "assert", "delete", "private", "final", "abstract", "eval"]

identifier :: PositionedText -> Maybe PositionedText
identifier pt = do
  let t = pValue pt
  (h, r) <- T.uncons t
  if isAlpha h && T.all isAlphaNum r && t `notElem` reservedWords then Just pt else Nothing

between :: (Applicative m) => m (Positioned bra) -> m (Positioned ket) -> m b -> m (Positioned b)
between bra ket ps = do
  b <- bra
  vs <- ps
  k <- ket
  pure (fromPosition2 b k vs)

sepEndBy :: E.Prod r e t a -> E.Prod r e t b -> E.Grammar r (E.Prod r e t [a])
sepEndBy p sep = mdo
  rule <-
    E.rule $
      pure [] <|> do
        first <- p
        subsequent <- optional (sep *> rule)
        pure (first : fromMaybe [] subsequent)
  pure rule

(<$$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
a <$$ fgb = fmap (a <$) fgb

infixl 4 <$$

lit :: Text -> E.Prod r Text PositionedText (Positioned ())
lit t = () <$$ E.satisfy (\(Positioned {pValue = tok}) -> tok == t) E.<?> T.snoc (T.cons '"' t) '"'

withPosition2 :: (Positioned a -> Positioned b -> c) -> Positioned a -> Positioned b -> Positioned c
withPosition2 f p1 p2 =
  let begin = min (pBegin p1) (pBegin p2)
      end = max (pEnd p1) (pEnd p2)
   in Positioned begin end (f p1 p2)

-- | Copy positioning information from two other positioned objects into an object.
fromPosition2 :: Positioned a -> Positioned b -> c -> Positioned c
fromPosition2 p1 p2 val = withPosition2 (\_ _ -> val) p1 p2

-- | Apply a positioned function call.
fromPositionApp :: Positioned (Positioned a -> b) -> Positioned a -> Positioned b
fromPositionApp pf pa = fromPosition2 pf pa (pValue pf pa)

expr :: E.Grammar r (E.Prod r Text PositionedText PositionedExpr)
expr = mdo
  let val = orExpr

  orExpr :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $
      let bin = do
            t1 <- orExpr
            _ <- lit "||"
            t2 <- andExpr
            pure (withPosition2 Or t1 t2)
       in bin <|> andExpr

  andExpr :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $
      let bin = do
            t1 <- andExpr
            _ <- lit "&&"
            t2 <- eqExpr
            pure (withPosition2 And t1 t2)
       in bin <|> eqExpr

  eqExpr :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $
      let bin = do
            t1 <- eqExpr
            op <- Eq <$ lit "==" <|> Neq <$ lit "!="
            t2 <- compExpr
            pure (withPosition2 op t1 t2)
       in bin <|> compExpr

  compExpr :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $
      let bin = do
            t1 <- compExpr
            op <- Lt <$ lit "<" <|> Le <$ lit "<=" <|> Gt <$ lit ">" <|> Ge <$ lit ">="
            t2 <- addExpr
            pure (withPosition2 op t1 t2)
       in bin <|> addExpr

  addExpr :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $
      let bin = do
            t1 <- addExpr
            op <- Plus <$ lit "+" <|> Minus <$ lit "-"
            t2 <- mulExpr
            pure (withPosition2 op t1 t2)
       in bin <|> mulExpr

  mulExpr :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $
      let bin = do
            t1 <- mulExpr
            op <- Mul <$ lit "*" <|> Div <$ lit "/" <|> Mod <$ lit "%" <|> IDiv <$ lit "//"
            t2 <- unaryExpr
            pure (withPosition2 op t1 t2)
       in bin <|> unaryExpr

  unaryExpr :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $
      let un = do
            op <- UnaryMinus <$$ lit "-" <|> UnaryPlus <$$ lit "+" <|> Not <$$ lit "!"
            t <- unaryExpr
            pure (fromPositionApp op t)
       in un <|> bool <|> number <|> memberOrEvalOrUpdateExpr

  let bool = (Boolean True <$$ lit "true") <|> (Boolean False <$$ lit "false")

  let number =
        let readNumber :: PositionedText -> Maybe (Positioned Double)
            readNumber pt = case TR.double (pValue pt) of
              Right (n, "") -> Just (pt $> n)
              _ -> Nothing
         in (Number <$>) <$> E.terminal readNumber E.<?> "number literal"

  memberOrEvalOrUpdateExpr :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $
      let memberOrEval = do
            obj <- memberOrEvalOrUpdateExpr
            _ <- lit "."
            memOrEval <- (Left <$> (E.terminal identifier E.<?> "identifier")) <|> (Right <$> lit "eval")
            pure $ case memOrEval of
              Left mem -> withPosition2 Member obj mem
              Right ev -> fromPosition2 obj ev (EvalAbstract obj ev)
          update = do
            obj <- memberOrEvalOrUpdateExpr
            updates <- between (lit "{") (lit "}") tupleContents
            pure (withPosition2 TupleUpdate obj updates)
       in memberOrEval <|> update <|> var <|> parenthesized <|> tuple <|> list <|> abstractTuple

  parenthesized :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $ pValue <$> between (lit "(") (lit ")") val

  let var = (\t -> Var t VariableLookupAllScopes <$ t) <$> E.terminal identifier E.<?> "identifier"

  let abstractTuple = do
        kw <- lit "abstract"
        tp <- between (lit "{") (lit "}") tupleContents
        pure (withPosition2 AbstractTuple kw tp)

  let tuple = (Tuple <$>) <$> between (lit "{") (lit "}") tupleContents

  tupleContents <- tupleItem `sepEndBy` lit ","

  let tupleItem = tupleRow <|> tupleAssertion

  let tupleRow = tupleDeleteRow <|> tupleAbstractRow <|> tupleValueRow

  let tupleValueRow = do
        attr <- optional (RowFinal <$$ lit "final" <|> RowPrivate <$$ lit "private")
        k <- E.terminal identifier E.<?> "identifier"
        let assignedValue = const <$> (lit "=" *> val)
        let updateSyntaxSugar =
              -- Syntax sugar to perform a tuple update without the = token. Due to
              -- ApplicativeDo desugaring limitations we cannot use the identifier
              -- directly so we have to parse into a function that accepts it.
              ( \updates parsedIdentifier ->
                  withPosition2
                    TupleUpdate
                    (Var parsedIdentifier VariableLookupCurrentScope <$ parsedIdentifier)
                    updates
              )
                <$> between (lit "{") (lit "}") tupleContents
        let compoundUpdatesSugar =
              asum
                ( ( \(op, ct) ->
                      ( \e parsedIdentifier ->
                          withPosition2
                            ct
                            (Var parsedIdentifier VariableLookupCurrentScope <$ parsedIdentifier)
                            e
                      )
                        <$> (lit op *> val)
                  )
                    <$> [("+=", Plus), ("-=", Minus), ("*=", Mul), ("/=", Div), ("//=", IDiv), ("%=", Mod)]
                )
        vf <- assignedValue <|> updateSyntaxSugar <|> compoundUpdatesSugar
        pure (let v = vf k in fromPosition2 k v (Row k (ValuedRow attr v <$ v)))

  let tupleAbstractRow = do
        abstract <- lit "abstract"
        k <- E.terminal identifier E.<?> "identifier"
        pure (fromPosition2 abstract k (Row k (AbstractRow <$ abstract)))

  let tupleDeleteRow = do
        delete <- lit "delete"
        k <- E.terminal identifier E.<?> "identifier"
        pure (fromPosition2 delete k (Row k (DeleteRow <$ delete)))

  let tupleAssertion = do
        l <- lit "assert"
        v <- parenthesized
        pure (fromPosition2 l v (Assertion v))

  let list = (List <$>) <$> between (lit "[") (lit "]") listContents

  listContents <- val `sepEndBy` lit ","

  pure val

parser :: E.Parser Text [PositionedText] PositionedExpr
parser = E.parser expr

data ParseErrorLocation = EOF | Loc (Int, Int)

data ParseError = ParseError ParseErrorLocation TLB.Builder

parseQCL :: Text -> Either TL.Text PositionedExpr
parseQCL t = case E.fullParses parser (tokenize t) of
  (results, E.Report {..}) -> case results of
    [x] -> Right x
    [] -> Left $
      printError $ case (expected, unconsumed) of
        ([], []) -> error "internal error: no parse result but no expected/unconsumed"
        (_ : _, []) -> ParseError EOF ("unexpected EOF; expecting " <> TLB.fromText (T.intercalate ", " expected))
        ([], Positioned b _ tok : _) -> ParseError (Loc b) ("expecting EOF; found \"" <> TLB.fromText tok <> "\"")
        (_ : _, Positioned b _ tok : _) -> ParseError (Loc b) ("expecting " <> TLB.fromText (T.intercalate ", " expected) <> ", found \"" <> TLB.fromText tok <> "\"")
    (p1 : p2 : _) -> error $ "internal error: ambiguous grammar: found " ++ show p1 ++ " and " ++ show p2
  where
    tl = T.lines t
    printError :: ParseError -> TL.Text
    printError (ParseError pel msg) = TLB.toLazyText ("parse error:\n" <> errorContext)
      where
        errorContext =
          case pel of
            EOF -> "    " <> msg <> "\n"
            Loc (l, c) ->
              let lineno = T.pack (show l)
                  linenoSpace = TLB.fromText (T.replicate (T.length lineno) " ")
                  gutter = " | "
                  gutterInitial = linenoSpace <> " |\n"
                  gutterEmpty = linenoSpace <> gutter
                  gutterLine = TLB.fromText lineno <> gutter
                  origLine = tl !! (l - 1)
                  spaces = TLB.fromText (T.replicate (c - 1) " ")
               in gutterInitial <> gutterLine <> TLB.fromText origLine <> "\n" <> gutterEmpty <> spaces <> "^ " <> msg <> "\n"

data Value
  = NumberValue Double
  | BooleanValue Bool
  | TupleValue TupleValue
  | AbstractTupleValue AbstractTupleValue
  | ListValue [Value]
  deriving (Show)

type TupleValue = M.Map Text TupleValueRow

data TupleValueRow
  = TupleValueRow
  { tprDefitionLoc :: PositionedText,
    tprValue :: Value,
    tprAttr :: Maybe (Positioned RowAttribute)
  }
  deriving (Show)

data AbstractTupleValueRow
  = AbstractTupleValueRow
  { atprDefinitionLoc :: PositionedText,
    atprValue :: PositionedRowExpr,
    atprType :: PolyType, -- This needs to be a polytype in case it is another abstract tuple.
    atprEvalOrder :: Int,
    atprCapturedEnvironment :: EvalEnvironment
  }
  deriving (Show)

data AbstractTupleValueAssertion
  = AbstractTupleValueAssertion
  { atpaExpr :: PositionedExpr,
    atpaEvalOrder :: Int,
    atpaCapturedEnvironment :: EvalEnvironment
  }
  deriving (Show)

data AbstractTupleValue -- TODO consider splitting this data type into two, one for type checking, the other for evaluation
  = AbstractTupleRows
  { atvFields :: M.Map Text AbstractTupleValueRow,
    atvAssertions :: [AbstractTupleValueAssertion],
    atvAbstractness :: Positioned (),
    atvNextTyVar :: Int, -- This is likely incorrect when one abstract tuple contains a field marked as abstract from another.

    -- | The type for the @AbstractTupleValue@ needs to be separately stored
    -- due to possible universal quantification.
    atvType :: PolyType,
    -- | The type substitutions need to be stored while type-checking the abstract tuple.
    atvTypeSubst :: TypeSubstitution
  }
  deriving (Show)

instance Aeson.ToJSON Value where
  toJSON (NumberValue n) = Aeson.Number (fromFloatDigits n)
  toJSON (BooleanValue b) = Aeson.Bool b
  toJSON (ListValue l) = Aeson.Array (fromList (Aeson.toJSON <$> l))
  toJSON (TupleValue tv) = Aeson.Object (AesonMap.fromMapText (ML.mapMaybe inner tv))
    where
      inner TupleValueRow {tprAttr = Just Positioned {pValue = RowPrivate}} = Nothing
      inner TupleValueRow {tprValue = v} = Just (Aeson.toJSON v)
  toJSON (AbstractTupleValue {}) = Aeson.Null

-- | In abstract tuples, where evaluation is delayed, we perform type checking
-- instead. These are the types in this language.
data Type
  = -- | The type for numbers or booleans.
    TypeNumberOrBoolean
  | -- | The type for lists. Since lists can be introduced and passed around but not manipulated, they are essentially a foreign type.
    TypeList
  | -- | The type for tuples. Each field has its own type.
    TypeTuple (M.Map Text PolyType)
  | -- | A type variable numbered as an integer. TODO add provenance
    TypeVar Int
  deriving (Show)

valueToType :: Value -> PolyType
valueToType = \case
  NumberValue {} -> PolyType mempty TypeNumberOrBoolean
  BooleanValue {} -> PolyType mempty TypeNumberOrBoolean
  ListValue {} -> PolyType mempty TypeList
  TupleValue tv -> PolyType mempty (TypeTuple (valueToType . tprValue <$> tv))
  AbstractTupleValue atv -> atvType atv

-- | A polytype is a (qualified) type with a number of quantifiers (foralls) in
-- front of it. For example the abstract tuple with one abstract field "in" and
-- one field "ret" equal to "in" has type "forall x. {in: x, ret: x}" where the
-- forall is represented by having the universally quantified "x" in the set.
-- This set is only generated by the "generalize" typing rule.
--
-- Furthermore, polytypes can only appear at the top level.
data PolyType = PolyType IntSet.IntSet Type
  deriving (Show)

-- | A type environment is a map from term variables (i.e. field labels) to its
-- scheme. An abstract field is equivalent to introducing a variable in a lambda
-- expression; a non-abstract field is equivalent to introducing a variable in a
-- "let" binding.
newtype TypeEnv = TypeEnv {getTypeEnv :: M.Map Text PolyType}
  deriving (Show)

-- | A substitution is a map from type variables to its monotype.
newtype TypeSubstitution = TypeSubstitution {getTypeSubstitution :: IntMap.IntMap Type}
  deriving (Show)

-- | Type substitution can be composed.
instance Semigroup TypeSubstitution where
  TypeSubstitution s1 <> TypeSubstitution s2 =
    TypeSubstitution ((applySubstToType (TypeSubstitution s1) <$> s2) `IntMap.union` s1)

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution mempty

applySubstToType :: TypeSubstitution -> Type -> Type
applySubstToType sub ty = case ty of
  TypeVar n ->
    case IntMap.lookup n (getTypeSubstitution sub) of
      Nothing -> ty
      Just t -> t
  TypeTuple fieldTypes -> TypeTuple (applySubstToPolyType sub <$> fieldTypes)
  TypeNumberOrBoolean -> TypeNumberOrBoolean
  TypeList -> TypeList

applySubstToPolyType :: TypeSubstitution -> PolyType -> PolyType
applySubstToPolyType (TypeSubstitution s) (PolyType vars t) =
  let adjustedSubstitution = TypeSubstitution (IntMap.withoutKeys s vars)
   in PolyType vars (applySubstToType adjustedSubstitution t)

applySubstToTypeEnv :: TypeSubstitution -> TypeEnv -> TypeEnv
applySubstToTypeEnv s (TypeEnv env) = TypeEnv (applySubstToPolyType s <$> env)

data EvalError
  = -- | A type error is encountered, with the expression, the actual value and the expected type.
    TypeError PositionedExpr Value TypeErrorDetail
  | -- | A tuple definition has a duplicate label.
    DuplicateLabelError PositionedText PositionedText
  | -- | Assertion failed. Variables inside the assertions are reported.
    AssertionFailed PositionedExpr (M.Map (Down (Positioned ())) Value)
  | -- | A member reference refers to a non-existing member.
    NonExistentLabelError TupleValue PositionedText
  | -- | A variable reference is attempted but is not inside a tuple.
    TopLevelVariableError PositionedText
  | -- | A variable does not exist.
    NonExistentVariableError PositionedText
  | -- | A variable does not exist in current scope.
    NonExistentVariableInCurrentScopeError PositionedText
  | -- | A variable reference that could refer to at least two definitions.
    AmbiguousVariableError PositionedText PositionedText PositionedText
  | -- | Attempt to override a tuple row that is marked final.
    FinalRowOverrideError PositionedText PositionedText (Positioned ())
  | PrivateRowAccessError PositionedText PositionedText (Positioned ())
  | -- | Making a field abstract when not inside an abstract tuple.
    AbstractInNonAbstractTuple (Positioned ())
  | -- | An abstract field is never overridden in an abstract tuple.
    AbstractFieldNotOverridden (Positioned ())
  | -- | A non-abstract field is overridden in an abstract tuple.
    NonAbstractFieldOverriddenInAbstractTuple PositionedText PositionedText PositionedRowExpr (Positioned ())
  | -- | An abstract field is deleted.
    AbstractRowDeleteError (Positioned ()) (Positioned ())
  | -- | Unimplemented.
    UnimplementedError
  | OccursCheck Type Type
  | TypeUnifyError Type Type
  deriving (Show)

data TypeErrorDetail = ExpectNumberOrBoolean | ExpectTuple | ExpectAbstractTuple -- TODO
  deriving (Show)

newtype InsideTupleEnv = InsideTupleEnv
  { eCurrentTuple :: TupleValue
  }

instance Show InsideTupleEnv where
  showsPrec prec InsideTupleEnv {..} = showsPrec prec (M.keys eCurrentTuple)

-- | The evaluation environment is a series of environments that represents the
-- nesting structure of the original code.
type EvalEnvironment = [InsideTupleEnv]

type Eval = ReaderT EvalEnvironment (Either EvalError)

type TypeCheck = StateT Int (Either EvalError)

newTypeVar :: TypeCheck Type
newTypeVar = do
  s <- get
  put (s + 1)
  pure (TypeVar s)

typeInferForNumberOrBoolean1 :: TypeEnv -> Positioned Expr -> TypeCheck (TypeSubstitution, Type)
typeInferForNumberOrBoolean1 tyenv a = do
  (subst1, t1) <- typeInfer tyenv a
  subst2 <- typeUnify t1 TypeNumberOrBoolean
  pure (subst2 <> subst1, TypeNumberOrBoolean)

typeInferForNumberOrBoolean2 :: TypeEnv -> Positioned Expr -> Positioned Expr -> TypeCheck (TypeSubstitution, Type)
typeInferForNumberOrBoolean2 tyenv a b = do
  (subst1, t1) <- typeInfer tyenv a
  (subst2, t2) <- typeInfer (applySubstToTypeEnv subst1 tyenv) b
  subst3 <- typeUnify t1 TypeNumberOrBoolean
  subst4 <- typeUnify t2 TypeNumberOrBoolean
  pure (subst4 <> subst3 <> subst2 <> subst1, TypeNumberOrBoolean)

typeInferForLogicOp :: TypeEnv -> Positioned Expr -> Positioned Expr -> TypeCheck (TypeSubstitution, Type)
typeInferForLogicOp =
  -- The left hand side is always evaluated into a number or boolean. The right
  -- hand side need not be a number or boolean. The result is either the left
  -- hand side (number or boolean) or the type of right hand side. Our type
  -- system cannot express this fact. TODO
  typeInferForNumberOrBoolean2

typeInfer :: TypeEnv -> Positioned Expr -> TypeCheck (TypeSubstitution, Type)
typeInfer tyenv pexpr =
  case pValue pexpr of
    Number _ -> pure (mempty, TypeNumberOrBoolean)
    Boolean _ -> pure (mempty, TypeNumberOrBoolean)
    Plus a b -> typeInferForNumberOrBoolean2 tyenv a b
    Minus a b -> typeInferForNumberOrBoolean2 tyenv a b
    Mul a b -> typeInferForNumberOrBoolean2 tyenv a b
    Div a b -> typeInferForNumberOrBoolean2 tyenv a b
    Mod a b -> typeInferForNumberOrBoolean2 tyenv a b
    IDiv a b -> typeInferForNumberOrBoolean2 tyenv a b
    UnaryPlus a -> typeInferForNumberOrBoolean1 tyenv a
    UnaryMinus a -> typeInferForNumberOrBoolean1 tyenv a
    Not a -> typeInferForNumberOrBoolean1 tyenv a
    Lt a b -> typeInferForNumberOrBoolean2 tyenv a b
    Le a b -> typeInferForNumberOrBoolean2 tyenv a b
    Gt a b -> typeInferForNumberOrBoolean2 tyenv a b
    Ge a b -> typeInferForNumberOrBoolean2 tyenv a b
    Eq a b -> typeInferForNumberOrBoolean2 tyenv a b
    Neq a b -> typeInferForNumberOrBoolean2 tyenv a b
    And a b -> typeInferForLogicOp tyenv a b
    Or a b -> typeInferForLogicOp tyenv a b
    List pes -> do
      -- Although we know the final type, this may still produce additional substitutions.
      allSubst <- foldlM (\subst e -> (<> subst) . fst <$> typeInfer tyenv e) mempty pes
      pure (allSubst, TypeList)
    _ -> undefined -- TODO

typeUnify :: Type -> Type -> TypeCheck TypeSubstitution -- TODO clarify which one is expected and which is actual
typeUnify t1 t2 = case (t1, t2) of
  (TypeNumberOrBoolean, TypeNumberOrBoolean) -> pure mempty
  (TypeList, TypeList) -> pure mempty
  (TypeVar v, _) -> bindVariable v t2
  (_, TypeVar v) -> bindVariable v t1
  (TypeTuple tuple1, TypeTuple tuple2) -> unifyTuple tuple1 tuple2
  _ -> lift $ Left $ TypeUnifyError t1 t2
  where
    bindVariable :: Int -> Type -> TypeCheck TypeSubstitution
    bindVariable v t
      | TypeVar u <- t, v == u = pure mempty
      | v `IntSet.member` freeTypeVariablesFromType t = lift $ Left $ OccursCheck (TypeVar v) t -- TODO add detail
      | otherwise = pure (TypeSubstitution (IntMap.singleton v t))
    unifyTuple :: M.Map Text PolyType -> M.Map Text PolyType -> TypeCheck TypeSubstitution
    unifyTuple tt1 tt2 = fold <$> MM.mergeA MM.dropMissing MM.dropMissing (MM.zipWithAMatched (const polyTypeUnify)) tt1 tt2

polyTypeUnify :: PolyType -> PolyType -> TypeCheck TypeSubstitution
polyTypeUnify (PolyType vars1 t1) (PolyType vars2 t2) =
  if IntSet.null vars1 && IntSet.null vars2 then typeUnify t1 t2 else lift (Left UnimplementedError)

-- | @instantiateType@ implements the similarly named typing rule that converts
-- a polytype into a type by instantiating universally quantified type variables
-- into fresh type variables.
instantiateType :: PolyType -> TypeCheck Type
instantiateType (PolyType vars ty) = do
  subst <- traverse (\_ -> newTypeVar) (IntMap.fromSet (const ()) vars)
  pure (applySubstToType (TypeSubstitution subst) ty)

-- | @generalize@ implements the similarly named typing rule that converts a
-- type into a polytype by finding free variables in a type that's not a free
-- variable in the type environment and adding them to universal quantification.
generalize :: TypeEnv -> Type -> PolyType
generalize env ty = PolyType (freeTypeVariablesFromType ty `IntSet.difference` freeTypeVariablesFromTypeEnv env) ty

freeTypeVariablesFromType :: Type -> IntSet.IntSet
freeTypeVariablesFromType ty = case ty of
  TypeVar n -> IntSet.singleton n
  TypeNumberOrBoolean -> mempty
  TypeList -> mempty
  TypeTuple tupleType -> foldMap freeTypeVariablesFromPolyType tupleType

freeTypeVariablesFromPolyType :: PolyType -> IntSet.IntSet
freeTypeVariablesFromPolyType (PolyType vars ty) = freeTypeVariablesFromType ty `IntSet.difference` vars

freeTypeVariablesFromTypeEnv :: TypeEnv -> IntSet.IntSet
freeTypeVariablesFromTypeEnv (TypeEnv env) = foldMap freeTypeVariablesFromPolyType env

evalNumeric :: PositionedExpr -> Eval Double
evalNumeric pexpr = do
  val <- evalExpr pexpr
  case val of
    NumberValue n -> pure n
    BooleanValue True -> pure 1
    BooleanValue False -> pure 0
    _ -> lift (Left (TypeError pexpr val ExpectNumberOrBoolean))

evalBoolean :: PositionedExpr -> Eval (Bool, Value)
evalBoolean pexpr = do
  val <- evalExpr pexpr
  case val of
    NumberValue n -> pure (n /= 0, val)
    BooleanValue b -> pure (b, val)
    _ -> lift (Left (TypeError pexpr val ExpectNumberOrBoolean))

evalNumericFunc1 :: (Double -> Double) -> PositionedExpr -> Eval Value
evalNumericFunc1 f a = (NumberValue . f) <$> evalNumeric a

evalNumericFunc2 :: (Double -> Double -> Double) -> PositionedExpr -> PositionedExpr -> Eval Value
evalNumericFunc2 f a b = NumberValue <$> liftA2 f (evalNumeric a) (evalNumeric b)

evalNumericBoolFunc1 :: (Double -> Bool) -> PositionedExpr -> Eval Value
evalNumericBoolFunc1 f a = (BooleanValue . f) <$> (evalNumeric a)

evalNumericBoolFunc2 :: (Double -> Double -> Bool) -> PositionedExpr -> PositionedExpr -> Eval Value
evalNumericBoolFunc2 f a b = BooleanValue <$> liftA2 f (evalNumeric a) (evalNumeric b)

evalExpr :: PositionedExpr -> Eval Value
evalExpr pexpr =
  case pValue pexpr of
    Number d -> pure (NumberValue d)
    Boolean b -> pure (BooleanValue b)
    Plus a b -> evalNumericFunc2 (+) a b
    Minus a b -> evalNumericFunc2 (-) a b
    Mul a b -> evalNumericFunc2 (*) a b
    Div a b -> evalNumericFunc2 (/) a b
    IDiv a b -> evalNumericFunc2 (\x y -> fromIntegral (floor (x / y) :: Int)) a b
    Mod a b -> evalNumericFunc2 (\x y -> let q = fromIntegral (floor (x / y) :: Int) in x - y * q) a b
    UnaryPlus a -> evalNumericFunc1 id a
    UnaryMinus a -> evalNumericFunc1 negate a
    Lt a b -> evalNumericBoolFunc2 (<) a b
    Le a b -> evalNumericBoolFunc2 (<=) a b
    Gt a b -> evalNumericBoolFunc2 (>) a b
    Ge a b -> evalNumericBoolFunc2 (>=) a b
    Eq a b -> evalNumericBoolFunc2 (==) a b
    Neq a b -> evalNumericBoolFunc2 (/=) a b
    And a b -> do
      (leftBool, leftVal) <- evalBoolean a
      if leftBool then evalExpr b else pure leftVal
    Or a b -> do
      (leftBool, leftVal) <- evalBoolean a
      if leftBool then pure leftVal else evalExpr b
    Not a -> evalNumericBoolFunc1 (== 0) a
    List pes -> ListValue <$> traverse evalExpr pes
    Tuple tupleExprs -> evalTupleExprs M.empty tupleExprs
    AbstractTuple abstractness p -> AbstractTupleValue <$> intoAbstractTupleValueFromExpr abstractness (pValue p)
    TupleUpdate origVal updates -> do
      orig <- evalExpr origVal
      case orig of
        TupleValue t -> evalTupleExprs t (pValue updates)
        AbstractTupleValue t -> AbstractTupleValue <$> intoAbstractTupleValueFromExprUpdate t (pValue updates)
        _ -> lift $ Left (TypeError origVal orig ExpectTuple)
    Member tuple label -> do
      tupleVal <- evalExpr tuple
      case tupleVal of
        TupleValue t -> case M.lookup (pValue label) t of
          Nothing -> lift $ Left (NonExistentLabelError t label)
          Just (TupleValueRow {tprDefitionLoc = prevDef, tprAttr = (Just privatePos@Positioned {pValue = RowPrivate})}) ->
            lift $ Left (PrivateRowAccessError label prevDef (void privatePos))
          Just (TupleValueRow {..}) -> pure tprValue
        _ -> lift $ Left (TypeError tuple tupleVal ExpectTuple)
    Var var scope -> lookupVariable var scope
    EvalAbstract tempExpr _ -> do
      tem <- evalExpr tempExpr
      case tem of
        AbstractTupleValue atv -> evalAbstractTuple atv
        _ -> lift $ Left (TypeError tempExpr tem ExpectAbstractTuple)

lookupVariable :: PositionedText -> VariableLookupScope -> Eval Value
lookupVariable p scope = ReaderT $ \case
  [] -> Left (TopLevelVariableError p)
  InsideTupleEnv {..} : outer ->
    case (M.lookup (pValue p) eCurrentTuple, scope) of
      (Nothing, VariableLookupCurrentScope) -> Left (NonExistentVariableInCurrentScopeError p)
      (Nothing, VariableLookupAllScopes) ->
        case runReaderT (lookupVariable p scope) outer of
          Left (TopLevelVariableError _) -> Left (NonExistentVariableError p)
          r -> r
      (Just TupleValueRow {..}, VariableLookupCurrentScope) -> pure tprValue
      (Just TupleValueRow {..}, VariableLookupAllScopes) ->
        case lookupVariableDefOnce outer of
          Just secondDef -> Left (AmbiguousVariableError p tprDefitionLoc secondDef)
          Nothing -> pure tprValue
  where
    lookupVariableDefOnce :: EvalEnvironment -> Maybe PositionedText
    lookupVariableDefOnce =
      getFirst
        . foldMap
          (\InsideTupleEnv {..} -> First (tprDefitionLoc <$> M.lookup (pValue p) eCurrentTuple))

evalTupleExprs :: TupleValue -> [PositionedTupleExpr] -> Eval Value
evalTupleExprs initial updates = do
  newEnv <- foldlM evalTupleExpr (InsideTupleEnv initial) updates
  pure (TupleValue (eCurrentTuple newEnv))
  where
    evalTupleExpr :: InsideTupleEnv -> PositionedTupleExpr -> Eval InsideTupleEnv
    evalTupleExpr ite@InsideTupleEnv {..} tp = withReaderT (ite :) $
      case pValue tp of
        Row label valExpr -> do
          val <- case pValue valExpr of
            ValuedRow attr rowValue -> Just . (attr,) <$> evalExpr rowValue
            DeleteRow -> pure Nothing
            AbstractRow -> lift $ Left (AbstractInNonAbstractTuple (void valExpr))
          let alterer :: Maybe TupleValueRow -> Eval (Maybe TupleValueRow)
              alterer orig =
                case (orig, val) of
                  (Just TupleValueRow {tprDefitionLoc = prevDef, tprAttr = Just pAttr}, _) ->
                    case pValue pAttr of
                      RowFinal -> lift $ Left (FinalRowOverrideError label prevDef (void pAttr))
                      RowPrivate -> lift $ Left (PrivateRowAccessError label prevDef (void pAttr))
                  (_, Nothing) ->
                    pure Nothing
                  (Just TupleValueRow {tprDefitionLoc = prevDef}, Just _)
                    | M.notMember (pValue label) initial ->
                        lift $ Left (DuplicateLabelError label prevDef)
                  (_, Just (attr, v)) ->
                    let newValue =
                          TupleValueRow
                            { tprDefitionLoc = label,
                              tprValue = v,
                              tprAttr = attr
                            }
                     in pure (Just newValue)
          InsideTupleEnv <$> M.alterF alterer (pValue label) eCurrentTuple
        Assertion assertion -> do
          evalAssertion assertion
          pure ite

evalAssertion :: PositionedExpr -> Eval ()
evalAssertion assertion = do
  val <- evalExpr assertion
  case val of
    BooleanValue True -> pure ()
    BooleanValue False -> do
      collectedVars <- collectVariables assertion
      lift $ Left (AssertionFailed assertion collectedVars)
    NumberValue n
      | n /= 0 -> pure ()
      | otherwise -> do
          collectedVars <- collectVariables assertion
          lift $ Left (AssertionFailed assertion collectedVars)
    _ -> lift $ Left (TypeError assertion val ExpectNumberOrBoolean)

collectVariables :: PositionedExpr -> Eval (M.Map (Down (Positioned ())) Value)
collectVariables pexpr =
  case pValue pexpr of
    Number _ -> pure mempty
    Boolean _ -> pure mempty
    Plus a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    Minus a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    Mul a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    Div a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    IDiv a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    Mod a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    UnaryPlus a -> collectVariables a
    UnaryMinus a -> collectVariables a
    Lt a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    Le a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    Gt a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    Ge a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    Eq a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    Neq a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    And a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    Or a b -> liftA2 (<>) (collectVariables a) (collectVariables b)
    Not a -> collectVariables a
    List _ -> pure mempty
    Tuple _ -> pure mempty
    AbstractTuple _ _ -> pure mempty
    TupleUpdate _ _ -> pure mempty
    Member tuple label -> do
      collected <- collectVariables tuple
      val <- evalExpr pexpr
      pure (M.insert (Down (void label)) val collected)
    Var var scope -> do
      val <- lookupVariable var scope
      pure (M.singleton (Down (void var)) val)
    EvalAbstract _ ev -> do
      val <- evalExpr pexpr
      pure (M.singleton (Down ev) val)

evalAbstractTuple :: AbstractTupleValue -> Eval Value
evalAbstractTuple (AbstractTupleRows fs assertions _ _ _ _) = do
  newEnv <- foldlM evalAbstractTupleField (InsideTupleEnv mempty) orderedFieldsAndAssertions
  pure (TupleValue (eCurrentTuple newEnv))
  where
    evalAbstractTupleField :: InsideTupleEnv -> Either AbstractTupleValueRow AbstractTupleValueAssertion -> Eval InsideTupleEnv
    evalAbstractTupleField it@InsideTupleEnv {..} (Left AbstractTupleValueRow {..}) =
      let newEnv = it : atprCapturedEnvironment
       in withReaderT (const newEnv) $
            case pValue atprValue of
              DeleteRow -> pure it
              AbstractRow -> lift $ Left (AbstractFieldNotOverridden (void atprValue))
              ValuedRow attr pexpr -> do
                val <- evalExpr pexpr
                pure
                  InsideTupleEnv
                    { eCurrentTuple =
                        M.insert
                          (pValue atprDefinitionLoc)
                          ( TupleValueRow
                              { tprDefitionLoc = atprDefinitionLoc,
                                tprValue = val,
                                tprAttr = attr
                              }
                          )
                          eCurrentTuple
                    }
    evalAbstractTupleField it (Right AbstractTupleValueAssertion {..}) =
      let newEnv = it : atpaCapturedEnvironment
       in withReaderT (const newEnv) (evalAssertion atpaExpr) >> pure it

    -- Reorder the fields and assertions according to the evaluation order.
    orderedFieldsAndAssertions :: [Either AbstractTupleValueRow AbstractTupleValueAssertion]
    orderedFieldsAndAssertions = sortBy (comparing (either atprEvalOrder atpaEvalOrder)) ((Left <$> M.elems fs) ++ (Right <$> assertions))

intoAbstractTupleValueFromExpr :: Positioned () -> [PositionedTupleExpr] -> Eval AbstractTupleValue
intoAbstractTupleValueFromExpr abstractness = intoAbstractTupleValueFromExprUpdate (AbstractTupleRows mempty mempty abstractness 0 (PolyType mempty (TypeTuple mempty)) mempty)

intoAbstractTupleValueFromExprUpdate :: AbstractTupleValue -> [PositionedTupleExpr] -> Eval AbstractTupleValue
intoAbstractTupleValueFromExprUpdate initial texprs =
  foldlM processTupleExpr initial (zip [initialEvaluationOrder ..] texprs)
  where
    -- TODO update atvType using generalize

    initialEvaluationOrder :: Int
    initialEvaluationOrder = 1 + (max 0 (getMax (foldMap (Max . atprEvalOrder) (atvFields initial))))

    getCurrentTypeEnv :: AbstractTupleValue -> EvalEnvironment -> TypeEnv
    getCurrentTypeEnv AbstractTupleRows {..} evalEnv =
      let current = atprType <$> atvFields
          -- Intentionally lazy due to inner tuples replacing outer ones.
          outside = foldMap (\(InsideTupleEnv ite) -> valueToType . tprValue <$> ite) evalEnv
       in TypeEnv (current <> outside)

    processTupleExpr :: AbstractTupleValue -> (Int, PositionedTupleExpr) -> Eval AbstractTupleValue
    processTupleExpr atv@AbstractTupleRows {..} (order, Positioned {pValue = Assertion a}) = do
      capturedEnvironment <- ask
      (subst, nextTyVar) <- lift $ (`runStateT` atvNextTyVar) $ do
        let tyEnv = getCurrentTypeEnv atv capturedEnvironment
        (subst1, ty) <- typeInfer tyEnv a
        subst2 <- typeUnify TypeNumberOrBoolean ty
        pure (subst2 <> subst1)
      pure
        atv
          { atvAssertions =
              AbstractTupleValueAssertion
                { atpaExpr = a,
                  atpaEvalOrder = order,
                  atpaCapturedEnvironment = capturedEnvironment
                }
                : atvAssertions,
            atvNextTyVar = nextTyVar,
            atvTypeSubst = subst <> atvTypeSubst
          }
    processTupleExpr atv@AbstractTupleRows {atvFields = fs} (order, Positioned {pValue = Row label rowExpr}) = do
      capturedEnvironment <- ask
      let newValueWithType :: PolyType -> AbstractTupleValueRow
          newValueWithType pt =
            AbstractTupleValueRow
              { atprDefinitionLoc = label,
                atprValue = rowExpr,
                atprEvalOrder = order,
                atprCapturedEnvironment = capturedEnvironment,
                atprType = pt
              }
          orig = M.lookup (pValue label) fs
      ((newSubst, newValue), nextTyVar) <- lift $ (`runStateT` atvNextTyVar atv) $ do
        case orig of
          Just AbstractTupleValueRow {atprDefinitionLoc = prevDef, atprValue = Positioned {pValue = ValuedRow (Just pAttr) _}} ->
            case pValue pAttr of
              RowFinal -> lift $ Left (FinalRowOverrideError label prevDef (void pAttr))
              RowPrivate -> lift $ Left (PrivateRowAccessError label prevDef (void pAttr))
          Just AbstractTupleValueRow {atprDefinitionLoc = prevLabel}
            | M.notMember (pValue label) (atvFields initial) ->
                lift $ Left (DuplicateLabelError label prevLabel)
          Just AbstractTupleValueRow {atprDefinitionLoc = prevLabel, atprType = origType, atprEvalOrder = oldOrder, atprValue = abstractness@Positioned {pValue = AbstractRow}} ->
            case pValue rowExpr of
              DeleteRow -> lift $ Left (AbstractRowDeleteError (void abstractness) (void rowExpr))
              AbstractRow -> lift $ Left (DuplicateLabelError label prevLabel)
              ValuedRow _ pexpr -> do
                let tyEnv = getCurrentTypeEnv atv capturedEnvironment
                t1 <- instantiateType origType
                (subst, t2) <- typeInfer tyEnv pexpr
                newSubst <- typeUnify (applySubstToType subst t1) t2
                pure (newSubst, Just ((newValueWithType (PolyType mempty t2)) {atprEvalOrder = oldOrder})) -- TODO
          Just AbstractTupleValueRow {atprValue = pRow, atprDefinitionLoc = origDef} ->
            lift $ Left (NonAbstractFieldOverriddenInAbstractTuple label origDef pRow (atvAbstractness initial))
          Nothing ->
            case pValue rowExpr of
              DeleteRow -> pure (mempty, Nothing)
              AbstractRow -> do
                tv <- newTypeVar
                pure (mempty, Just (newValueWithType (PolyType mempty tv)))
              ValuedRow _ pexpr -> do
                let tyEnv = getCurrentTypeEnv atv capturedEnvironment
                (subst, ty) <- typeInfer tyEnv pexpr
                pure (subst, Just (newValueWithType (PolyType mempty ty)))
      pure
        atv
          { atvNextTyVar = nextTyVar,
            atvTypeSubst = newSubst <> atvTypeSubst atv,
            atvFields = case newValue of
              Nothing -> fs
              Just v -> M.insert (pValue label) v fs
          }

evalQCL :: Text -> Either TL.Text Value
evalQCL text = do
  parsed <- parseQCL text
  case runReaderT (evalExpr parsed) [] of
    Right v -> pure v
    Left e -> Left (printError e)
  where
    textLines = T.lines text
    printError :: EvalError -> TL.Text
    printError ee = TLB.toLazyText ("error:\n    " <> msg)
      where
        msg :: TLB.Builder
        msg = case ee of
          TypeError pe v detail ->
            "unexpected type for expression\n" <> explainContext pe ("expecting " <> explainExpectedType detail <> ", found " <> explainActualType v)
          DuplicateLabelError l1 l2 ->
            "duplicate field label " <> escapedText l1 <> " in tuple\n" <> explainContext l1 "this definition" <> explainContext l2 "earlier definition"
          AssertionFailed pe vars ->
            "assertion failed\n" <> explainContext pe "evaluates to false" <> ML.foldMapWithKey explainVariable vars
          NonExistentLabelError tp l ->
            "label " <> escapedText l <> " does not exist in tuple\n" <> explainContext l (explainTuple tp)
          TopLevelVariableError l ->
            "variable reference " <> escapedText l <> " must be inside a tuple\n" <> explainContext l "top-level variable reference"
          NonExistentVariableError l ->
            "variable reference " <> escapedText l <> " does not exist\n" <> explainContext l "undefined variable reference"
          NonExistentVariableInCurrentScopeError l ->
            "variable reference " <> escapedText l <> " does not exist in current tuple\n" <> explainContext l "undefined variable reference"
          AmbiguousVariableError var l1 l2 ->
            "variable reference " <> escapedText l1 <> " is ambiguous\n" <> explainContext var "variable reference used here" <> explainContext l1 "possible referent" <> explainContext l2 "another possible referent"
          FinalRowOverrideError override def final ->
            "field marked as final cannot be overridden\n" <> explainContext override "override here" <> explainContext def "defined here" <> explainContext final "marked as final here"
          PrivateRowAccessError label def privatePos ->
            "field marked as private cannot be accessed outside its enclosing tuple\n" <> explainContext label "access of private field here" <> explainContext def "defined here" <> explainContext privatePos "marked as private here"
          AbstractInNonAbstractTuple abstract ->
            "abstract field cannot be used in non-abstract tuples\n" <> explainContext abstract "abstract field"
          AbstractFieldNotOverridden abstract ->
            "abstract field never overridden upon evaluation\n" <> explainContext abstract "abstract field"
          NonAbstractFieldOverriddenInAbstractTuple label origLabel origRowExpr abstract ->
            "non-abstract field cannot be overridden in an abstract tuple\n"
              <> explainPossiblyEvalFirst
              <> explainContext label "override of field here"
              <> explainContext origLabel "previous value defined here"
              <> explainContext abstract "tuple marked as abstract here"
            where
              explainPossiblyEvalFirst = case origRowExpr of
                Positioned {pValue = ValuedRow (Just _) _} -> mempty
                _ -> "        (consider first evaluating the abstract tuple into a tuple,\n        and then overriding this field)\n"
          AbstractRowDeleteError abstractness deletion ->
            "abstract fields cannot be deleted\n"
              <> explainContext abstractness "abstract field defined here"
              <> explainContext deletion "delete here"
          UnimplementedError -> "this is unimplemented\n"
          OccursCheck t1 t2 -> "cannot unify type " <> explainType t1 <> " with " <> explainType t2 <> " because the latter contains the former"
          TypeUnifyError t1 t2 -> "cannot unify type " <> explainType t1 <> " with " <> explainType t2
        escapedText (Positioned {pValue = t}) = TLB.fromString (show t)
        explainExpectedType detail = case detail of
          ExpectNumberOrBoolean -> "number or boolean"
          ExpectTuple -> "tuple"
          ExpectAbstractTuple -> "abstract tuple"
        explainActualType v = case v of
          NumberValue {} -> "number"
          BooleanValue {} -> "boolean"
          TupleValue {} -> "tuple"
          AbstractTupleValue {} -> "abstract tuple"
          ListValue {} -> "list"
        explainType ty = case ty of
          TypeNumberOrBoolean -> "number or boolean"
          TypeList -> "list"
          TypeVar n -> "t" <> TLB.fromString (show n)
          TypeTuple tup -> "{" <> ML.foldMapWithKey (\field (PolyType vars t) -> TLB.fromText field <> ": " <> explainForall vars <> explainType t <> ", ") tup <> "}"
        explainForall vars = if IntSet.null vars then mempty else "forall " <> IntSet.foldr (\v r -> explainType (TypeVar v) <> " " <> r) "" vars <> ". "
        explainTuple tp = case M.toList tp of
          [] -> "tuple is empty"
          [(a, _)] -> "tuple has sole label " <> TLB.fromString (show a)
          v ->
            let shortened5 = map (T.pack . show . fst) (take 5 v)
                shortened4 = take 4 shortened5
             in "tuple has labels " <> TLB.fromText (T.intercalate ", " shortened4) <> (if length shortened5 == 5 then ", ..." else mempty)
        explainVariable :: Down (Positioned a) -> Value -> TLB.Builder
        explainVariable (Down name) val = explainContext name ("this has value " <> TLB.fromLazyText (decodeUtf8 (Aeson.encode val)))
        explainContext :: Positioned a -> TLB.Builder -> TLB.Builder
        explainContext Positioned {pBegin = (bl, bc), pEnd = (el, ec)} explanation =
          let contextLines = drop (bl - 1) (take el (zip [(1 :: Int) ..] textLines))
              contextLinesWithColumns =
                zip
                  contextLines
                  ( ( \(lineno, line) ->
                        ( if lineno == bl then bc else 1,
                          if lineno == el then ec else T.length line + 1
                        )
                    )
                      <$> contextLines
                  )
              gutter = " | "
              lastLineno = T.pack (show el)
              gutterEmpty = TLB.fromText (T.replicate (T.length lastLineno) " ") <> gutter
              gutterInitial = TLB.fromText (T.replicate (T.length lastLineno) " ") <> " |\n"
              makeGutter n =
                let ns = T.pack (show n)
                 in TLB.fromText (T.replicate (T.length lastLineno - T.length ns) " ") <> TLB.fromText ns <> gutter
           in gutterInitial
                <> foldMap
                  ( \((lineno, line), (columnBegin, columnEnd)) ->
                      makeGutter lineno
                        <> TLB.fromText line
                        <> "\n"
                        <> gutterEmpty
                        <> TLB.fromText (T.replicate (columnBegin - 1) " ")
                        <> TLB.fromText (T.replicate (columnEnd - columnBegin) "^")
                        <> (if lineno == el then " " <> explanation else mempty)
                        <> "\n"
                  )
                  contextLinesWithColumns
