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
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonMap
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Foldable
import Data.Functor
import Data.List (sortBy, unfoldr)
import qualified Data.Map.Strict as M
import Data.Monoid (First (..))
import Data.Ord (Down (..), comparing)
import Data.Scientific (fromFloatDigits)
import Data.Semigroup (Max (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.Read as TR
import Data.Vector (fromList)
import qualified Text.Earley as E

data Positioned a = Positioned {pBegin :: (Int, Int), pEnd :: (Int, Int), pValue :: a}
  deriving (Show, Functor, Eq, Ord)

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
  | AbstractTuple (Positioned [PositionedTupleExpr])
  | EvalAbstract PositionedExpr (Positioned ())
  | List [PositionedExpr]
  | Var PositionedText
  | Member PositionedExpr PositionedText
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
multiCharPunct = ["&&", "||", "==", "!=", "<=", ">=", "//"]

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
sepEndBy p sep = mfix (\rule -> E.rule $ (:) <$> p <*> ((sep *> rule) <|> pure []) <|> pure [])

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

  bool :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $ (Boolean True <$$ lit "true") <|> (Boolean False <$$ lit "false")

  number :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $
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

  var :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $ (\t -> Var t <$ t) <$> E.terminal identifier E.<?> "identifier"

  abstractTuple :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $ do
      kw <- lit "abstract"
      tp <- between (lit "{") (lit "}") tupleContents
      pure (withPosition2 (const AbstractTuple) kw tp)

  tuple :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $ (Tuple <$>) <$> between (lit "{") (lit "}") tupleContents

  tupleContents <- tupleItem `sepEndBy` lit ","

  tupleItem <- E.rule $ tupleRow <|> tupleAssertion

  tupleRow :: E.Prod r Text PositionedText PositionedTupleExpr <-
    E.rule $
      tupleDeleteRow
        <|> tupleAbstractRow
        <|> do
          attr <- pure Nothing <|> (Just <$> (RowFinal <$$ lit "final")) <|> (Just <$> (RowPrivate <$$ lit "private"))
          k <- E.terminal identifier E.<?> "identifier"
          let assignedValue = const <$> (lit "=" *> val)
          let updateSyntaxSugar =
                -- Syntax sugar to perform a tuple update without the = token. Due to
                -- ApplicativeDo desugaring limitations we cannot use the identifier
                -- directly so we have to parse into a function that accepts it.
                ( \updates parsedIdentifier ->
                    withPosition2
                      TupleUpdate
                      (Var parsedIdentifier <$ parsedIdentifier)
                      updates
                )
                  <$> between (lit "{") (lit "}") tupleContents
          vf <- assignedValue <|> updateSyntaxSugar
          pure (let v = vf k in fromPosition2 k v (Row k (ValuedRow attr v <$ v)))

  tupleAbstractRow :: E.Prod r Text PositionedText PositionedTupleExpr <- E.rule $ do
    abstract <- lit "abstract"
    k <- E.terminal identifier E.<?> "identifier"
    pure (fromPosition2 abstract k (Row k (AbstractRow <$ abstract)))

  tupleDeleteRow :: E.Prod r Text PositionedText PositionedTupleExpr <- E.rule $ do
    abstract <- lit "delete"
    k <- E.terminal identifier E.<?> "identifier"
    pure (fromPosition2 abstract k (Row k (DeleteRow <$ abstract)))

  tupleAssertion <- E.rule $ do
    l <- lit "assert"
    v <- parenthesized
    pure (fromPosition2 l v (Assertion v))

  list :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $ (List <$>) <$> between (lit "[") (lit "]") listContents

  listContents <- val `sepEndBy` lit ","

  pure val

parser :: E.Parser Text [PositionedText] PositionedExpr
parser = E.parser expr

data ParseErrorLocation = EOF | Loc (Int, Int)

data ParseError = ParseError ParseErrorLocation Text

parseQCL :: Text -> Either Text PositionedExpr
parseQCL t = case E.fullParses parser (tokenize t) of
  (results, E.Report {..}) -> case results of
    [x] -> Right x
    [] -> Left $
      printError $ case (expected, unconsumed) of
        ([], []) -> error "internal error: no parse result but no expected/unconsumed"
        (_ : _, []) -> ParseError EOF ("unexpected EOF; expecting " <> T.intercalate ", " expected)
        ([], Positioned b _ tok : _) -> ParseError (Loc b) ("expecting EOF; found \"" <> tok <> "\"")
        (_ : _, Positioned b _ tok : _) -> ParseError (Loc b) ("expecting " <> T.intercalate ", " expected <> ", found \"" <> tok <> "\"")
    (p1 : p2 : _) -> error $ "internal error: ambiguous grammar: found " ++ show p1 ++ " and " ++ show p2
  where
    tl = T.lines t
    printError (ParseError pel msg) =
      "parse error:\n" <> errorContext
      where
        errorContext =
          case pel of
            EOF -> "    " <> msg <> "\n"
            Loc (l, c) ->
              let lineno = T.pack (show l)
                  linenoSpace = T.replicate (T.length lineno) " "
                  gutter = " | "
                  gutterEmpty = linenoSpace <> gutter
                  gutterLine = lineno <> gutter
                  origLine = tl !! (l - 1)
                  spaces = T.replicate (c - 1) " "
               in gutterEmpty <> "\n" <> gutterLine <> origLine <> "\n" <> gutterEmpty <> spaces <> "^ " <> msg <> "\n"

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
    tprFinality :: TupleValueRowFinality
  }
  deriving (Show)

data AbstractTupleValueRow
  = AbstractTupleValueRow
  { atprDefinitionLoc :: PositionedText,
    atprValue :: PositionedRowExpr,
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

newtype TupleValueRowFinality = TupleValueRowFinality (Maybe (Positioned ()))
  deriving (Show)

data AbstractTupleValue
  = AbstractTupleRows
  { atvFields :: M.Map Text AbstractTupleValueRow,
    atvAssertions :: [AbstractTupleValueAssertion]
  }
  deriving (Show)

instance Aeson.ToJSON Value where
  toJSON (NumberValue n) = Aeson.Number (fromFloatDigits n)
  toJSON (BooleanValue b) = Aeson.Bool b
  toJSON (ListValue l) = Aeson.Array (fromList (Aeson.toJSON <$> l))
  toJSON (TupleValue tv) = Aeson.Object (AesonMap.fromMapText (Aeson.toJSON . tprValue <$> tv))
  toJSON (AbstractTupleValue {}) = Aeson.Null -- NOTE: Maybe let's not have this?

data EvalError
  = TypeError PositionedExpr TypeErrorDetail
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
  | -- | A variable reference that could refer to at least two definitions.
    AmbiguousVariableError PositionedText PositionedText PositionedText
  | -- | Attempt to override a tuple row that is marked final.
    FinalRowOverrideError PositionedText PositionedText (Positioned ())
  | -- | Making a field abstract when not inside an abstract tuple.
    AbstractInNonAbstractTuple (Positioned ())
  | AbstractFieldNotOverridden (Positioned ())
  | PrivateFieldInAbstractTuple (Positioned ())
  deriving (Show)

data TypeErrorDetail = ExpectBoolean | ExpectNumberOrBoolean | ExpectTuple | ExpectAbstractTuple
  deriving (Show)

data InsideTupleEnv = InsideTupleEnv
  { eCurrentTuple :: TupleValue,
    eCurrentTuplePrivates :: M.Map Text ()
  }
  deriving (Show)

-- | The evaluation environment is a series of environments that represents the
-- nesting structure of the original code.
type EvalEnvironment = [InsideTupleEnv]

type Eval = ReaderT EvalEnvironment (Either EvalError)

evalNumeric :: PositionedExpr -> Eval Double
evalNumeric pexpr = do
  val <- evalExpr pexpr
  case val of
    NumberValue n -> pure n
    BooleanValue True -> pure 1
    BooleanValue False -> pure 0
    _ -> lift (Left (TypeError pexpr ExpectNumberOrBoolean))

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
    And a b -> evalNumericBoolFunc2 (\x y -> (x /= 0) && (y /= 0)) a b
    Or a b -> evalNumericBoolFunc2 (\x y -> (x /= 0) || (y /= 0)) a b
    Not a -> evalNumericBoolFunc1 (== 0) a
    List pes -> ListValue <$> traverse evalExpr pes
    Tuple tupleExprs -> evalTupleExprs M.empty tupleExprs
    AbstractTuple p -> do
      capturedEnvironment <- ask
      lift $ (AbstractTupleValue <$> intoAbstractTupleValueFromExpr (pValue p) capturedEnvironment)
    TupleUpdate origVal updates -> do
      orig <- evalExpr origVal
      case orig of
        TupleValue t -> evalTupleExprs t (pValue updates)
        AbstractTupleValue t -> do
          capturedEnvironment <- ask
          lift $ (AbstractTupleValue <$> intoAbstractTupleValueFromExprUpdate t (pValue updates) capturedEnvironment)
        _ -> lift $ Left (TypeError origVal ExpectTuple)
    Member tuple label -> do
      tupleVal <- evalExpr tuple
      case tupleVal of
        TupleValue t -> case M.lookup (pValue label) t of
          Nothing -> lift $ Left (NonExistentLabelError t label)
          Just (TupleValueRow _ v _) -> pure v
        _ -> lift $ Left (TypeError tuple ExpectTuple)
    Var var -> lookupVariable var
    EvalAbstract tempExpr _ -> do
      tem <- evalExpr tempExpr
      case tem of
        AbstractTupleValue atv -> evalAbstractTuple atv
        _ -> lift $ Left (TypeError tempExpr ExpectAbstractTuple)

lookupVariable :: PositionedText -> Eval Value
lookupVariable p = ReaderT $ \env ->
  case env of
    [] -> Left (TopLevelVariableError p)
    InsideTupleEnv {eCurrentTuple = cur} : outer ->
      case M.lookup (pValue p) cur of
        Nothing ->
          case runReaderT (lookupVariable p) outer of
            Left (TopLevelVariableError _) -> Left (NonExistentVariableError p)
            r -> r
        Just (TupleValueRow p1 v _) ->
          case lookupVariableOnce outer of
            Just (p2, _) -> Left (AmbiguousVariableError p p1 p2)
            Nothing -> pure v
  where
    lookupVariableOnce :: EvalEnvironment -> Maybe (PositionedText, Value)
    lookupVariableOnce =
      getFirst
        . foldMap
          (\InsideTupleEnv {eCurrentTuple = cur} -> First ((\(TupleValueRow p2 v _) -> (p2, v)) <$> (M.lookup (pValue p) cur)))

evalTupleExprs :: TupleValue -> [PositionedTupleExpr] -> Eval Value
evalTupleExprs initial updates = do
  newEnv <- foldlM evalTupleExpr (InsideTupleEnv initial mempty) updates
  pure (TupleValue (M.difference (eCurrentTuple newEnv) (eCurrentTuplePrivates newEnv)))
  where
    evalTupleExpr :: InsideTupleEnv -> PositionedTupleExpr -> Eval InsideTupleEnv
    evalTupleExpr ite tp = withReaderT (ite :) $
      case pValue tp of
        Row label valExpr -> do
          val <- case pValue valExpr of
            ValuedRow attr rowValue -> Just . (attr,) <$> evalExpr rowValue
            DeleteRow -> pure Nothing
            AbstractRow -> lift $ Left (AbstractInNonAbstractTuple (void valExpr))
          case ite of
            InsideTupleEnv {eCurrentTuple = e, eCurrentTuplePrivates = privates} -> do
              let alterer :: Maybe TupleValueRow -> Eval (Maybe TupleValueRow)
                  alterer orig =
                    case (orig, val) of
                      (Just (TupleValueRow prevDef _ (TupleValueRowFinality (Just finalPos))), _) ->
                        lift $ Left (FinalRowOverrideError label prevDef finalPos)
                      (_, Nothing) ->
                        pure Nothing
                      (Just (TupleValueRow prevLabel _ _), Just _)
                        | M.notMember (pValue label) initial ->
                            lift $ Left (DuplicateLabelError label prevLabel)
                      (_, Just (attr, v)) -> pure (Just (TupleValueRow label v (getRowFinality attr)))
              newe <- M.alterF alterer (pValue label) e
              let newPrivates = case val of
                    Just (Just (Positioned {pValue = RowPrivate}), _) -> M.insert (pValue label) () privates
                    _ -> privates
              pure InsideTupleEnv {eCurrentTuple = newe, eCurrentTuplePrivates = newPrivates}
        Assertion assertion -> do
          evalAssertion assertion
          pure ite

getRowFinality :: Maybe (Positioned RowAttribute) -> TupleValueRowFinality
getRowFinality attr =
  case attr of
    Just (p@Positioned {pValue = RowFinal}) -> TupleValueRowFinality (Just (void p))
    _ -> TupleValueRowFinality Nothing

evalAssertion :: PositionedExpr -> Eval ()
evalAssertion assertion = do
  val <- evalExpr assertion
  case val of
    BooleanValue True -> pure ()
    BooleanValue False -> do
      collectedVars <- collectVariables assertion
      lift $ Left (AssertionFailed assertion collectedVars)
    _ -> lift $ Left (TypeError assertion ExpectBoolean)

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
    AbstractTuple _ -> pure mempty
    TupleUpdate _ _ -> pure mempty
    Member tuple label -> do
      collected <- collectVariables tuple
      val <- evalExpr pexpr
      pure (M.insert (Down (void label)) val collected)
    Var var -> do
      val <- lookupVariable var
      pure (M.singleton (Down (void var)) val)
    EvalAbstract _ ev -> do
      val <- evalExpr pexpr
      pure (M.singleton (Down ev) val)

evalAbstractTuple :: AbstractTupleValue -> Eval Value
-- Let's keep things simple for now. An abstract tuple can be thought of as a
-- function (where the abstract fields are function arguments and the resulting
-- tuple is the function result). However general functions can close over their
-- lexical environment and capture variables; we however only capture the
-- dynamic environment (which would pose issues for nested abstract tuples).
-- Also for simplicity, we do not evaluate or type check expressions that are
-- overridden. The name lookup rules are tremendously simplified: there is but
-- one scope to look up names. Assertions are accumulated and evaluated at the
-- very end, regardless of where they appear.
evalAbstractTuple (AbstractTupleRows fs assertions) = do
  newEnv <- foldlM evalAbstractTupleField (InsideTupleEnv mempty mempty) orderedFieldsAndAssertions
  -- forM_ assertions (\assertion -> withReaderT (const [newEnv]) (evalAssertion assertion))
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
                pure it {eCurrentTuple = M.insert (pValue atprDefinitionLoc) (TupleValueRow atprDefinitionLoc val (getRowFinality attr)) eCurrentTuple}
    evalAbstractTupleField it (Right AbstractTupleValueAssertion {..}) =
      let newEnv = it : atpaCapturedEnvironment
       in withReaderT (const newEnv) (evalAssertion atpaExpr) >> pure it

    -- Reorder the fields and assertions according to the evaluation order.
    orderedFieldsAndAssertions :: [Either AbstractTupleValueRow AbstractTupleValueAssertion]
    orderedFieldsAndAssertions = sortBy (comparing (either atprEvalOrder atpaEvalOrder)) ((Left <$> M.elems fs) ++ (Right <$> assertions))

intoAbstractTupleValueFromExpr :: [PositionedTupleExpr] -> EvalEnvironment -> Either EvalError AbstractTupleValue
intoAbstractTupleValueFromExpr = intoAbstractTupleValueFromExprUpdate (AbstractTupleRows mempty mempty)

intoAbstractTupleValueFromExprUpdate :: AbstractTupleValue -> [PositionedTupleExpr] -> EvalEnvironment -> Either EvalError AbstractTupleValue
intoAbstractTupleValueFromExprUpdate initial texprs capturedEnvironment =
  foldlM processTupleExpr initial (zip [initialEvaluationOrder ..] texprs)
  where
    initialEvaluationOrder :: Int
    initialEvaluationOrder = 1 + (max 0 (getMax (foldMap (\row -> Max (atprEvalOrder row)) (atvFields initial))))

    processTupleExpr :: AbstractTupleValue -> (Int, PositionedTupleExpr) -> Either EvalError AbstractTupleValue
    processTupleExpr atv@AbstractTupleRows {..} (order, Positioned {pValue = Assertion a}) =
      pure
        atv
          { atvAssertions =
              AbstractTupleValueAssertion
                { atpaExpr = a,
                  atpaEvalOrder = order,
                  atpaCapturedEnvironment = capturedEnvironment
                }
                : atvAssertions
          }
    processTupleExpr _ (_, Positioned {pValue = Row _ (Positioned {pValue = ValuedRow (Just (pos@Positioned {pValue = RowPrivate})) _})}) =
      Left (PrivateFieldInAbstractTuple (void pos))
    processTupleExpr atv@AbstractTupleRows {atvFields = fs} (order, Positioned {pValue = Row label rowExpr}) = do
      let newValue =
            AbstractTupleValueRow
              { atprDefinitionLoc = label,
                atprValue = rowExpr,
                atprEvalOrder = order,
                atprCapturedEnvironment = capturedEnvironment
              }
          alterer :: Maybe AbstractTupleValueRow -> Either EvalError (Maybe AbstractTupleValueRow)
          alterer orig = case orig of
            Just
              ( AbstractTupleValueRow
                  { atprDefinitionLoc = prevDef,
                    atprValue = Positioned {pValue = ValuedRow (Just finalPos@Positioned {pValue = RowFinal}) _}
                  }
                ) ->
                Left (FinalRowOverrideError label prevDef (void finalPos))
            Just (AbstractTupleValueRow {atprDefinitionLoc = prevLabel})
              | M.notMember (pValue label) (atvFields initial) ->
                  Left (DuplicateLabelError label prevLabel)
            Just
              ( AbstractTupleValueRow
                  { atprEvalOrder = oldOrder,
                    atprValue = Positioned {pValue = AbstractRow}
                  }
                ) ->
                pure (Just newValue {atprEvalOrder = oldOrder})
            _ ->
              pure (Just newValue)
      newFields <- M.alterF alterer (pValue label) fs
      pure atv {atvFields = newFields}

evalQCL :: Text -> Either Text Value
evalQCL text = do
  parsed <- parseQCL text
  case runReaderT (evalExpr parsed) [] of
    Right v -> pure v
    Left e -> Left (printError e)
  where
    textLines = T.lines text
    printError :: EvalError -> Text
    printError ee =
      "error:\n    " <> msg
      where
        msg = case ee of
          TypeError pe detail ->
            "unexpected type for expression\n" <> explainContext pe ("expecting " <> explainExpectedType detail)
          DuplicateLabelError l1 l2 ->
            "duplicate field label " <> escapedText l1 <> " in tuple\n" <> explainContext l1 "this definition" <> explainContext l2 "earlier definition"
          AssertionFailed pe vars ->
            "assertion failed\n" <> explainContext pe "evaluates to false" <> M.foldMapWithKey explainVariable vars
          NonExistentLabelError tp l ->
            "label " <> escapedText l <> " does not exist in tuple\n" <> explainContext l (explainTuple tp)
          TopLevelVariableError l ->
            "variable reference " <> escapedText l <> " must be inside a tuple\n" <> explainContext l "top-level variable reference"
          NonExistentVariableError l ->
            "variable reference " <> escapedText l <> " does not exist\n" <> explainContext l "undefined variable reference"
          AmbiguousVariableError var l1 l2 ->
            "variable reference " <> escapedText l1 <> " is ambiguous\n" <> explainContext var "variable reference used here" <> explainContext l1 "possible referent" <> explainContext l2 "another possible referent"
          FinalRowOverrideError override def final ->
            "field marked as final cannot be overridden\n" <> explainContext override "override here" <> explainContext def "defined here" <> explainContext final "marked as final here"
          AbstractInNonAbstractTuple abstract ->
            "abstract field cannot be used in non-abstract tuples\n" <> explainContext abstract "abstract field"
          AbstractFieldNotOverridden abstract ->
            "abstract field never overridden upon evaluation\n" <> explainContext abstract "abstract field"
          PrivateFieldInAbstractTuple private ->
            -- This actually could be supported in the future, but it requires
            -- distinguishing between different labels of the same name. Will
            -- require a bit more refactoring in order to implement it.
            "private fields cannot be used in abstract tuples\n" <> explainContext private "private here"
        escapedText (Positioned {pValue = t}) = T.pack (show t)
        explainExpectedType detail = case detail of
          ExpectBoolean -> "boolean"
          ExpectNumberOrBoolean -> "number or boolean"
          ExpectTuple -> "tuple"
          ExpectAbstractTuple -> "abstract tuple"
        explainTuple tp = case M.toList tp of
          [] -> "tuple is empty"
          [(a, _)] -> "tuple has sole label " <> a
          v ->
            let shortened5 = map (T.pack . show . fst) (take 5 v)
                shortened4 = take 4 shortened5
             in "tuple has labels " <> T.intercalate ", " shortened4 <> (if length shortened5 == 5 then ", ..." else mempty)
        explainVariable :: Down (Positioned a) -> Value -> Text
        explainVariable (Down name) val = explainContext name ("this has value " <> (decodeUtf8Lenient (BL.toStrict (Aeson.encode val))))
        explainContext :: Positioned a -> Text -> Text
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
              gutterEmpty = T.replicate (T.length lastLineno) " " <> gutter
              makeGutter n =
                let ns = T.pack (show n)
                 in T.replicate (T.length lastLineno - T.length ns) " " <> ns <> gutter
           in gutterEmpty
                <> "\n"
                <> foldMap'
                  ( \((lineno, line), (columnBegin, columnEnd)) ->
                      makeGutter lineno
                        <> line
                        <> "\n"
                        <> gutterEmpty
                        <> T.replicate (columnBegin - 1) " "
                        <> T.replicate (columnEnd - columnBegin) "^"
                        <> (if lineno == el then " " <> explanation else mempty)
                        <> "\n"
                  )
                  contextLinesWithColumns
