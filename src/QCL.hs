{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module QCL (Positioned (..), Expr (..), TupleExpr (..), RowExpr (..), parseQCL, evalQCL, runExamples) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonMap
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Functor
import Data.List (find, unfoldr)
import qualified Data.Map.Strict as M
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Vector (fromList)
import qualified Text.Earley as E

data Positioned a = Positioned {pBegin :: (Int, Int), pEnd :: (Int, Int), pValue :: a}
  deriving (Show, Functor)

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
  | List [PositionedExpr]
  | Var PositionedText
  | Member PositionedExpr PositionedText
  deriving (Show)

data TupleExpr
  = Row PositionedText PositionedRowExpr
  | Assertion PositionedExpr
  deriving (Show)

data RowExpr
  = ValuedRow PositionedExpr
  | NullRow
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
                       in Just (Positioned (lineno, colno) (lineno, newcolno) (T.take len t), (colno, remaining))
                    Left _ -> error "unexpected error while tokenizing"
              | Just op <- find (`T.isPrefixOf` t) multiCharPunct ->
                  let newcolno = colno + T.length op in Just (Positioned (lineno, colno) (lineno, newcolno) op, (newcolno, T.drop (T.length op) t))
              | otherwise ->
                  let newcolno = colno + 1 in Just (Positioned (lineno, colno) (lineno, newcolno) (T.singleton h), (newcolno, r))

multiCharPunct :: [Text]
multiCharPunct = ["&&", "||", "==", "!=", "<=", ">=", "//"]

reservedWords :: [Text]
reservedWords = ["true", "false", "assert", "null"]

identifier :: PositionedText -> Maybe PositionedText
identifier pt = do
  let t = pValue pt
  (h, r) <- T.uncons t
  if isAlpha h && T.all isAlphaNum r && t `notElem` reservedWords then Just pt else Nothing

between :: Applicative m => m (Positioned bra) -> m (Positioned ket) -> m b -> m (Positioned b)
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
       in un <|> atom

  atom <- E.rule $ bool <|> number <|> memberOrUpdateExpr

  bool :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $ (Boolean True <$$ lit "true") <|> (Boolean False <$$ lit "false")

  number :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $
      let readNumber :: PositionedText -> Maybe (Positioned Double)
          readNumber pt = case TR.double (pValue pt) of
            Right (n, "") -> Just (pt $> n)
            _ -> Nothing
       in (Number <$>) <$> E.terminal readNumber E.<?> "number literal"

  memberOrUpdateExpr :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $
      let member = do
            obj <- memberOrUpdateExpr
            _ <- lit "."
            mem <- E.terminal identifier E.<?> "identifier"
            pure (fromPosition2 obj mem (Member obj mem))
          update = do
            obj <- memberOrUpdateExpr
            updates <- between (lit "{") (lit "}") tupleContents
            pure (fromPosition2 obj updates (TupleUpdate obj updates))
       in member <|> update <|> var <|> surroundExpr

  surroundExpr <- E.rule $ parenthesized <|> tuple <|> list

  parenthesized :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $ pValue <$> between (lit "(") (lit ")") val

  var :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $ (\t -> Var t <$ t) <$> E.terminal identifier E.<?> "identifier"

  tuple :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $ (Tuple <$>) <$> between (lit "{") (lit "}") tupleContents

  tupleContents <- tupleItem `sepEndBy` lit ","

  tupleItem <- E.rule $ tupleRow <|> tupleAssertion

  tupleRow <- E.rule $ do
    k <- E.terminal identifier E.<?> "identifier"
    _ <- lit "="
    v <- tupleRowValue
    pure (fromPosition2 k v (Row k v))

  let valuedRow pe = ValuedRow pe <$ pe
  tupleRowValue <- E.rule $ (NullRow <$$ lit "null") <|> (valuedRow <$> val)

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
        (_ : _, []) -> ParseError EOF ("Unexpected EOF. Expecting " <> T.intercalate ", " expected)
        ([], Positioned b _ tok : _) -> ParseError (Loc b) ("Expecting EOF. Found \"" <> tok <> "\"")
        (_ : _, Positioned b _ tok : _) -> ParseError (Loc b) ("Expecting " <> T.intercalate ", " expected <> ". Found \"" <> tok <> "\"")
    (p1 : p2 : _) -> error $ "internal error: ambiguous grammar: found " ++ show p1 ++ " and " ++ show p2
  where
    tl = T.lines t
    printError (ParseError pel msg) =
      "Error: " <> msg <> "\n\n"
        <> ( case pel of
               EOF -> mempty
               Loc (l, c) ->
                 let column = T.pack (show l) <> " | "
                     origLine = tl !! (l - 1)
                     spaces = T.replicate (T.length column + c - 1) " "
                  in column <> origLine <> "\n" <> spaces <> "^-- here"
           )

data Value
  = NumberValue Double
  | BooleanValue Bool
  | TupleValue TupleValue
  | ListValue [Value]
  deriving (Show)

type TupleValue = M.Map Text (PositionedText, Value)

instance Aeson.ToJSON Value where
  toJSON (NumberValue n) = Aeson.Number (fromFloatDigits n)
  toJSON (BooleanValue b) = Aeson.Bool b
  toJSON (ListValue l) = Aeson.Array (fromList (Aeson.toJSON <$> l))
  toJSON (TupleValue tv) = Aeson.Object (AesonMap.fromMapText (Aeson.toJSON . snd <$> tv))

data EvalError
  = TypeError PositionedExpr TypeErrorDetail
  | -- | A tuple definition has a duplicate label.
    DuplicateLabelError PositionedText PositionedText
  | -- | Assertion failed.
    AssertionFailed PositionedExpr
  | -- | A member reference refers to a non-existing member.
    NonExistentLabelError TupleValue PositionedText
  | -- | A variable reference is attempted but is not inside a tuple.
    TopLevelVariableError PositionedText
  | -- | A variable does not exist.
    NonExistentVariableError PositionedText
  | -- | A variable reference that could refer to at least two definitions.
    AmbiguousVariableError PositionedText PositionedText
  deriving (Show)

data TypeErrorDetail = ExpectBoolean | ExpectNumberOrBoolean | ExpectTuple
  deriving (Show)

data EvalEnvironment
  = TopLevel -- not within a tuple
  | InsideTuple {eCurrentTuple :: TupleValue, eOuterEnvironment :: EvalEnvironment}

type Eval = ExceptT EvalError (State EvalEnvironment)

evalNumeric :: PositionedExpr -> Eval Double
evalNumeric pexpr = do
  val <- evalQCL pexpr
  case val of
    NumberValue n -> pure n
    BooleanValue True -> pure 1
    BooleanValue False -> pure 0
    _ -> throwE (TypeError pexpr ExpectNumberOrBoolean)

evalNumericFunc1 :: (Double -> Double) -> PositionedExpr -> Eval Value
evalNumericFunc1 f a = (NumberValue . f) <$> evalNumeric a

evalNumericFunc2 :: (Double -> Double -> Double) -> PositionedExpr -> PositionedExpr -> Eval Value
evalNumericFunc2 f a b = NumberValue <$> liftA2 f (evalNumeric a) (evalNumeric b)

evalNumericBoolFunc1 :: (Double -> Bool) -> PositionedExpr -> Eval Value
evalNumericBoolFunc1 f a = (BooleanValue . f) <$> (evalNumeric a)

evalNumericBoolFunc2 :: (Double -> Double -> Bool) -> PositionedExpr -> PositionedExpr -> Eval Value
evalNumericBoolFunc2 f a b = BooleanValue <$> liftA2 f (evalNumeric a) (evalNumeric b)

evalQCL :: PositionedExpr -> Eval Value
evalQCL pexpr =
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
    List pes -> ListValue <$> traverse evalQCL pes
    Tuple tupleExprs -> evalTupleExprs M.empty tupleExprs
    TupleUpdate origVal updates -> do
      orig <- evalQCL origVal
      case orig of
        TupleValue t -> evalTupleExprs t (pValue updates)
        _ -> throwE (TypeError origVal ExpectTuple)
    Member tuple label -> do
      tupleVal <- evalQCL tuple
      case tupleVal of
        TupleValue t -> case M.lookup (pValue label) t of
          Nothing -> throwE (NonExistentLabelError t label)
          Just (_, v) -> pure v
        _ -> throwE (TypeError tuple ExpectTuple)
    Var var -> do
      e <- lift get
      except (snd <$> lookupVariable var e)

lookupVariable :: PositionedText -> EvalEnvironment -> Either EvalError (PositionedText, Value)
lookupVariable p env =
  case env of
    TopLevel -> Left (TopLevelVariableError p)
    InsideTuple {eCurrentTuple = cur, eOuterEnvironment = outer} ->
      case (M.lookup (pValue p) cur, lookupVariable p outer) of
        (Nothing, e@(Left _)) -> e
        (Just _, Left (AmbiguousVariableError p2 _)) -> Left (AmbiguousVariableError p p2)
        (Just x, Left _) -> pure x
        (Nothing, Right x) -> pure x
        (Just _, Right (p2, _)) -> Left (AmbiguousVariableError p p2)

evalTupleExprs :: TupleValue -> [PositionedTupleExpr] -> Eval Value
evalTupleExprs initial updates = do
  prevEnv <- lift get
  lift (modify (InsideTuple initial))
  forM_ updates evalTupleExpr
  newEnv <- lift get
  lift (put prevEnv)
  pure (TupleValue (eCurrentTuple newEnv))

evalTupleExpr :: PositionedTupleExpr -> Eval ()
evalTupleExpr tp =
  case pValue tp of
    Row label valExpr -> do
      val <- case pValue valExpr of
        ValuedRow rowValue -> Just <$> evalQCL rowValue
        NullRow -> pure Nothing
      e <- lift get
      newe <-
        M.alterF
          ( \orig -> case (orig, val) of
              -- It is okay to remove something that doesn't exist.
              (Nothing, Nothing) -> pure Nothing
              (Nothing, Just v) -> pure (Just (label, v))
              (Just _, Nothing) -> pure Nothing
              (Just (prevLabel, _), Just _) -> throwE (DuplicateLabelError label prevLabel)
          )
          (pValue label)
          (eCurrentTuple e)
      lift (put e {eCurrentTuple = newe})
    Assertion assertion -> do
      val <- evalQCL assertion
      case val of
        BooleanValue True -> pure ()
        BooleanValue False -> throwE (AssertionFailed assertion)
        _ -> throwE (TypeError assertion ExpectBoolean)

examples :: [Text]
examples =
  [ "true",
    "1.234",
    "1+2+3+4+5",
    "1 + 2 * 3 + 4 < 5",
    "a+b.c.d",
    "+a+!b.c.d",
    "(",
    ")",
    "{",
    "{}",
    "{} =",
    "{} { }",
    "{} = { }",
    "{x = true}",
    "{x = true, }",
    "{x = true,\n y = false\n}",
    "{x = true,\n y = false,\n}",
    "{x = true,\n y = x + 1}",
    "{x = true,\n assert(true), }",
    "{x = 2,\n x = 1}",
    "1 + { a = 2, b = a + 3}.b ",
    "1 + { a = 2, b = a + 3}.b && c",
    "[]",
    "[1, true]",
    "a{x=1} {y=2}",
    "a.b.c",
    "a.b{x=1}",
    "a{x=1}.b",
    "{ x=1, y=2, z=3 } {z = null}",
    "{ x=1, y=2, z=3 } .x",
    "{ x=1, y=2, z=3 } .w",
    "{ x=1, y=2, z=y } ",
    "{ x=1, y=2, z={a=1, b=y} } ",
    "{ x=1, y=2, z={a=1, b=a} } ",
    "{ x=1, y=2, z={x=1, y=x} } ",
    "{ x = 1, y = 2, z = { a = x + 1, b = y + a + 2}}.z.b"
  ]

runExamples :: IO ()
runExamples = forM_ examples $ \s -> do
  TIO.putStrLn s
  case parseQCL s of
    Right parsed -> do
      case evalState (runExceptT (evalQCL parsed)) TopLevel of
        Right value -> do
          putStr "Eval Success: "
          BL.putStrLn (Aeson.encode value)
        Left e -> do
          putStr "Parse Success: " <> print parsed
          putStr "Eval error: " <> print e
    Left e -> TIO.putStrLn e
  putStrLn "========================================"
