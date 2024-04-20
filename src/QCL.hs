{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StrictData #-}

module QCL (Expr (..), TupleExpr (..), parseQCL) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (find, unfoldr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Text.Earley as E

data Positioned a = Positioned Int Int a
  deriving (Show, Functor)

data Expr
  = Number Double
  | Boolean Bool
  | Plus Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | UnaryPlus Expr
  | UnaryMinus Expr
  | Not Expr
  | Lt Expr Expr
  | Le Expr Expr
  | Gt Expr Expr
  | Ge Expr Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Tuple [TupleExpr]
  | TupleUpdate Expr [TupleExpr]
  | List [Expr]
  | Var Text
  | Member Expr Text
  deriving (Show)

data TupleExpr
  = Row Text RowExpr
  | Assertion Expr
  deriving (Show)

data RowExpr
  = ValuedRow Expr
  | NullRow
  deriving (Show)

type PositionedText = Positioned Text

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
                   in Just (Positioned lineno colno tok, (colno + T.length tok, remaining))
              | isDigit h ->
                  case TR.double t of
                    Right (n, remaining) ->
                      let len = T.length t - T.length remaining
                       in Just (Positioned lineno colno (T.take len t), (colno + len, remaining))
                    Left _ -> error "unexpected error while tokenizing"
              | Just op <- find (`T.isPrefixOf` t) multiCharPunct ->
                  Just (Positioned lineno colno op, (colno + T.length op, T.drop (T.length op) t))
              | otherwise -> Just (Positioned lineno colno (T.singleton h), (colno + 1, r))

multiCharPunct :: [Text]
multiCharPunct = ["&&", "||", "==", "!=", "<=", ">="]

reservedWords :: [Text]
reservedWords = ["true", "false", "assert", "null"]

identifier :: PositionedText -> Maybe Text
identifier (Positioned _ _ t) = do
  (h, r) <- T.uncons t
  if isAlpha h && T.all isAlphaNum r && t `notElem` reservedWords then Just t else Nothing

between :: Applicative m => m bra -> m ket -> m a -> m a
between bra ket p = bra *> p <* ket

sepEndBy :: E.Prod r e t a -> E.Prod r e t b -> E.Grammar r (E.Prod r e t [a])
sepEndBy p sep = mfix (\rule -> E.rule $ (:) <$> p <*> ((sep *> rule) <|> pure []) <|> pure [])

lit :: Text -> E.Prod r Text PositionedText ()
lit t = void $ E.satisfy (\(Positioned _ _ tok) -> tok == t) E.<?> T.snoc (T.cons '"' t) '"'

expr :: E.Grammar r (E.Prod r Text PositionedText Expr)
expr = mdo
  val <- E.rule orExpr

  orExpr <-
    E.rule $
      let bin = do
            t1 <- orExpr
            _ <- lit "||"
            t2 <- andExpr
            pure (Or t1 t2)
       in bin <|> andExpr

  andExpr <-
    E.rule $
      let bin = do
            t1 <- andExpr
            _ <- lit "&&"
            t2 <- eqExpr
            pure (And t1 t2)
       in bin <|> eqExpr

  eqExpr <-
    E.rule $
      let bin = do
            t1 <- eqExpr
            op <- Eq <$ lit "==" <|> Neq <$ lit "!="
            t2 <- compExpr
            pure (op t1 t2)
       in bin <|> compExpr

  compExpr <-
    E.rule $
      let bin = do
            t1 <- compExpr
            op <- Lt <$ lit "<" <|> Le <$ lit "<=" <|> Gt <$ lit ">" <|> Ge <$ lit ">="
            t2 <- addExpr
            pure (op t1 t2)
       in bin <|> addExpr

  addExpr <-
    E.rule $
      let bin = do
            t1 <- addExpr
            op <- Plus <$ lit "+" <|> Minus <$ lit "-"
            t2 <- mulExpr
            pure (op t1 t2)
       in bin <|> mulExpr

  mulExpr <-
    E.rule $
      let bin = do
            t1 <- mulExpr
            op <- Mul <$ lit "*" <|> Div <$ lit "/" <|> Mod <$ lit "%"
            t2 <- unaryExpr
            pure (op t1 t2)
       in bin <|> unaryExpr

  unaryExpr <-
    E.rule $
      let un = do
            op <- UnaryMinus <$ lit "-" <|> UnaryPlus <$ lit "+" <|> Not <$ lit "!"
            t <- unaryExpr
            pure (op t)
       in un <|> atom

  atom <- E.rule $ bool <|> number <|> memberOrUpdateExpr

  bool <- E.rule $ (Boolean True <$ lit "true") <|> (Boolean False <$ lit "false")

  number <-
    E.rule $
      let readNumber :: PositionedText -> Maybe Double
          readNumber (Positioned _ _ t) = case TR.double t of
            Right (n, "") -> Just n
            _ -> Nothing
       in Number <$> E.terminal readNumber E.<?> "number literal"

  memberOrUpdateExpr <-
    E.rule $
      let member = do
            obj <- memberOrUpdateExpr
            _ <- lit "."
            mem <- E.terminal identifier E.<?> "identifier"
            pure (Member obj mem)
          update = do
            obj <- memberOrUpdateExpr
            updates <- between (lit "{") (lit "}") tupleContents
            pure (TupleUpdate obj updates)
       in member <|> update <|> var <|> surroundExpr

  surroundExpr <- E.rule $ parenthesized <|> tuple <|> list

  parenthesized <- E.rule $ between (lit "(") (lit ")") val

  var <- E.rule $ Var <$> E.terminal identifier E.<?> "identifier"

  tuple <- E.rule $ Tuple <$> between (lit "{") (lit "}") tupleContents

  tupleContents <- tupleItem `sepEndBy` lit ","

  tupleItem <- E.rule $ tupleRow <|> tupleAssertion

  tupleRow <- E.rule $ do
    k <- E.terminal identifier E.<?> "identifier"
    _ <- lit "="
    v <- tupleRowValue
    pure (Row k v)

  tupleRowValue <- E.rule $ (NullRow <$ lit "null") <|> (ValuedRow <$> val)

  tupleAssertion <- E.rule $ Assertion <$> (lit "assert" *> parenthesized)

  listContents <- val `sepEndBy` lit ","

  list <- E.rule $ List <$> between (lit "[") (lit "]") listContents

  pure val

parser :: E.Parser Text [PositionedText] Expr
parser = E.parser expr

data ParseErrorLocation = EOF | Loc Int Int

data ParseError = ParseError ParseErrorLocation Text

parseQCL :: Text -> Either Text Expr
parseQCL t = case E.fullParses parser (tokenize t) of
  (results, E.Report {..}) -> case results of
    [x] -> Right x
    [] -> Left $
      printError $ case (expected, unconsumed) of
        ([], []) -> error "internal error: no parse result but no expected/unconsumed"
        (_ : _, []) -> ParseError EOF ("Unexpected EOF. Expecting " <> T.intercalate ", " expected)
        ([], Positioned l c t : _) -> ParseError (Loc l c) ("Expecting EOF. Found \"" <> t <> "\"")
        (_ : _, Positioned l c t : _) -> ParseError (Loc l c) ("Expecting " <> T.intercalate ", " expected <> ". Found \"" <> t <> "\"")
    (p1 : p2 : _) -> error $ "internal error: ambiguous grammar: found " ++ show p1 ++ " and " ++ show p2
  where
    tl = T.lines t
    printError (ParseError pel msg) =
      "Error: " <> msg <> "\n\n"
        <> ( case pel of
               EOF -> mempty
               Loc l c ->
                 let column = T.pack (show l) <> " | "
                     origLine = tl !! (l - 1)
                     spaces = T.replicate (T.length column + c - 1) " "
                  in column <> origLine <> "\n" <> spaces <> "^-- here"
           )

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
    "1 + { a = 2, b = a + 3}.b ",
    "1 + { a = 2, b = a + 3}.b && c",
    "[]",
    "[1, true]",
    "a{x=1} {y=2}",
    "a.b.c",
    "a.b{x=1}",
    "a{x=1}.b",
    "{ x=1, y=2, z=3 } {z = null}"
  ]

parseExamples :: IO ()
parseExamples = forM_ examples $ \s -> do
  TIO.putStrLn s
  case parseQCL s of
    Right expr -> putStr "Success: " <> print expr
    Left e -> TIO.putStrLn e
  putStrLn "========================================"
