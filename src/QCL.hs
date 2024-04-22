{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module QCL (Positioned (..), Expr (..), TupleExpr (..), RowExpr (..), evalQCL, runExamples) where

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
import Data.Foldable
import Data.Functor
import Data.List (unfoldr)
import qualified Data.Map.Strict as M
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as TIO
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
                       in Just (Positioned (lineno, colno) (lineno, newcolno) (T.take len t), (newcolno, remaining))
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
  val <- evalExpr pexpr
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
    TupleUpdate origVal updates -> do
      orig <- evalExpr origVal
      case orig of
        TupleValue t -> evalTupleExprs t (pValue updates)
        _ -> throwE (TypeError origVal ExpectTuple)
    Member tuple label -> do
      tupleVal <- evalExpr tuple
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
      case M.lookup (pValue p) cur of
        Nothing ->
          case lookupVariable p outer of
            Left (TopLevelVariableError _) -> Left (NonExistentVariableError p)
            r -> r
        Just x@(p1, _) ->
          case lookupVariableOnce outer of
            Just (p2, _) -> Left (AmbiguousVariableError p1 p2)
            Nothing -> pure x
  where
    lookupVariableOnce :: EvalEnvironment -> Maybe (PositionedText, Value)
    lookupVariableOnce newEnv =
      case newEnv of
        TopLevel -> Nothing
        InsideTuple {eCurrentTuple = cur, eOuterEnvironment = outer} ->
          case M.lookup (pValue p) cur of
            Nothing -> lookupVariableOnce outer
            Just x -> pure x

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
        ValuedRow rowValue -> Just <$> evalExpr rowValue
        NullRow -> pure Nothing
      env <- lift get
      case env of
        TopLevel -> error "evalTupleExpr cannot be called"
        InsideTuple {eCurrentTuple = e, eOuterEnvironment = outer} -> do
          newe <-
            M.alterF
              ( \orig -> case (orig, val) of
                  (_, Nothing) -> pure Nothing
                  (Nothing, Just v) -> pure (Just (label, v))
                  (Just (prevLabel, _), Just _) -> throwE (DuplicateLabelError label prevLabel)
              )
              (pValue label)
              e
          lift (put InsideTuple {eCurrentTuple = newe, eOuterEnvironment = outer})
    Assertion assertion -> do
      val <- evalExpr assertion
      case val of
        BooleanValue True -> pure ()
        BooleanValue False -> throwE (AssertionFailed assertion)
        _ -> throwE (TypeError assertion ExpectBoolean)

evalQCL :: Text -> Either Text Value
evalQCL text = do
  parsed <- parseQCL text
  case evalState (runExceptT (evalExpr parsed)) TopLevel of
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
            "duplicate label " <> escapedText l1 <> " in tuple\n" <> explainContext l2 "this definition" <> explainContext l1 "earlier definition"
          AssertionFailed pe ->
            "assertion failed\n" <> explainContext pe "evaluates to false"
          NonExistentLabelError tp l ->
            "label " <> escapedText l <> " does not exist in tuple\n" <> explainContext l (explainTuple tp)
          TopLevelVariableError l ->
            "variable reference " <> escapedText l <> " must be inside a tuple\n" <> explainContext l "top-level variable"
          NonExistentVariableError l ->
            "variable reference " <> escapedText l <> " does not exist in this lexical scope\n" <> explainContext l "undefined variable"
          AmbiguousVariableError l1 l2 ->
            "variable reference " <> escapedText l1 <> " is ambiguous\n" <> explainContext l1 "possible reference" <> explainContext l2 "another possible reference"
        escapedText (Positioned {pValue = t}) = T.pack (show t)
        explainExpectedType detail = case detail of
          ExpectBoolean -> "boolean"
          ExpectNumberOrBoolean -> "number or boolean"
          ExpectTuple -> "tuple"
        explainTuple tp = case M.toList tp of
          [] -> "tuple is empty"
          [(a, _)] -> "tuple has sole label " <> a
          v ->
            let shortened5 = map (T.pack . show . fst) (take 5 v)
                shortened4 = take 4 shortened5
             in "tuple has labels " <> T.intercalate ", " shortened4 <> (if length shortened5 == 5 then ", ..." else mempty)
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
           in gutterEmpty <> "\n"
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

examples :: [Text]
examples =
  [ "true",
    "1.234",
    "1+2+3+4+5",
    "17.5 / 5.5",
    "17.5 // 5.5",
    "17.5 % 5.5",
    "1 + 2 * 3 + 4 < 5",
    "a+b.c.d",
    "{}",
    "{} =",
    "{} { }",
    "{x = true}",
    "{x = true, }",
    "{x = true,\n y = false\n}",
    "{x = true,\n y = false,\n}",
    "{x = true,\n y = x + 1}",
    "{x = true,\n assert(true), }",
    "{x = 5, assert(x % 2\n==\n0), }",
    "{x = 2,\n x = 1}",
    "1 + { a = 2, b = a + 3}.b ",
    "1 + { a = 2, b = a + 3}.b && c",
    "1 + { a = 2, b = a + 3}",
    "{\n a = 1,\n b = a + a,\n c = a + b + c\n}.c",
    "[]",
    "[1, true]",
    "a{x=1} {y=+x}",
    "a.b.c",
    "a.b{x=1}",
    "a{x=1}.b",
    "{a = {b = {c=1}}, ret = a.b{x=1}}.ret",
    "{a = {b = {c=1}}, ret = a{x=1}.b}.ret",
    "{ x=1, y=2, z=3 } {z = null}",
    "{ x=1, y=2, z=3 } .x",
    "{ x=1, y=2, z=3 }.wwww",
    "{ x=1, y=2, z=y } ",
    "{ x=1, y=2, z={a=1, b=y} } ",
    "{ x=1, y=2, z={a=1, b=a} } ",
    "{ x=1, y=2, z={x=1, y=x} } ",
    "{ x = 1, y = 2, z = { a = x + 1, b = y + a + 2}}.z.b",
    "{ a=1, b={ a=1, b={ a=1, b={ c=a } } } }"
  ]

runExamples :: IO ()
runExamples = forM_ examples $ \s -> do
  putStrLn "QCL:\n\n```"
  TIO.putStrLn s
  putStrLn "```"
  case evalQCL s of
    Right value -> do
      putStrLn "\nJSON result:\n\n```"
      BL.putStrLn (Aeson.encode value)
      putStrLn "```\n"
    Left e -> do
      putStrLn "\nError message:\n```"
      TIO.putStrLn e
      putStrLn "```\n"
  putStrLn "------------\n"
