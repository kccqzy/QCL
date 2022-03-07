{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module QCL (Expr (..), TupleExpr (..), parseQCL) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Text.Earley as E

data Expr
  = Number Double
  | Boolean Bool
  | Plus Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Tuple [TupleExpr]
  | VarRef [Text]
  deriving (Show)

data TupleExpr
  = Row Text Expr
  | Assertion Expr
  deriving (Show)

data PositionedText = PositionedText Int Int Text
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
            Just (h, r)
              | isSpace h -> recognizeOne (1 + colno, r)
              | isAlpha h ->
                  let (tok, remaining) = T.span isAlphaNum t
                   in Just (PositionedText lineno colno tok, (colno + T.length tok, remaining))
              | isDigit h ->
                  case TR.double t of
                    Right (n, remaining) ->
                      let len = T.length t - T.length remaining
                       in Just (PositionedText lineno colno (T.take len t), (colno + len, remaining))
                    Left _ -> error "unexpected error while tokenizing"
              | otherwise -> Just (PositionedText lineno colno (T.singleton h), (colno + 1, r))

readNumber :: PositionedText -> Maybe Double
readNumber (PositionedText _ _ t) = case TR.double t of
  Right (n, "") -> Just n
  _ -> Nothing

reservedWords :: [Text]
reservedWords = ["true", "false", "assert"]

identifier :: PositionedText -> Maybe Text
identifier (PositionedText _ _ t) = do
  (h, r) <- T.uncons t
  if isAlpha h && T.all isAlphaNum r && t `notElem` reservedWords then Just t else Nothing

between :: Applicative m => m bra -> m ket -> m a -> m a
between bra ket p = bra *> p <* ket

sepEndBy :: E.Prod r e t a -> E.Prod r e t b -> E.Grammar r (E.Prod r e t [a])
sepEndBy p sep = mfix (\rule -> E.rule $ (:) <$> p <*> ((sep *> rule) <|> pure []) <|> pure [])

sepBy1 :: E.Prod r e t a -> E.Prod r e t b -> E.Grammar r (E.Prod r e t [a])
sepBy1 p sep = mfix (\rule -> E.rule $ (:) <$> p <*> ((sep *> rule) <|> pure []))

lit :: Text -> E.Prod r Text PositionedText ()
lit t = () <$ E.satisfy (\(PositionedText _ _ tok) -> tok == t) E.<?> (T.snoc (T.cons '"' t) '"')

expr :: E.Grammar r (E.Prod r Text PositionedText Expr)
expr = mdo
  bool <- E.rule $ (Boolean True <$ lit "true") <|> (Boolean False <$ lit "false")
  number <- E.rule $ Number <$> E.terminal readNumber E.<?> "number literal"
  addExpr <- E.rule $ (do t1 <- addExpr; op <- Plus <$ lit "+" <|> Minus <$ lit "-"; t2 <- mulExpr; pure (op t1 t2)) <|> mulExpr
  mulExpr <- E.rule $ (do t1 <- mulExpr; op <- Mul <$ lit "*" <|> Div <$ lit "/"; t2 <- atom; pure (op t1 t2)) <|> atom
  varRefs <- (E.terminal identifier `sepBy1` lit ".")
  var <- E.rule $ VarRef <$> varRefs
  atom <- E.rule $ var <|> bool <|> number <|> between (lit "(") (lit ")") val
  tupleRow <- E.rule $ do
    k <- E.terminal identifier E.<?> "identifier"
    _ <- lit "="
    v <- val
    pure (Row k v)
  tupleAssertion <- E.rule $ Assertion <$> (lit "assert" *> between (lit "(") (lit ")") val)
  tupleContents <- tupleItem `sepEndBy` lit ","
  tupleItem <- E.rule $ tupleRow <|> tupleAssertion
  tuple <- E.rule $ Tuple <$> between (lit "{") (lit "}") tupleContents
  val <- E.rule $ addExpr <|> tuple
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
        ([], PositionedText l c t : _) -> ParseError (Loc l c) ("Expecting EOF. Found \"" <> t <> "\"")
        (_ : _, PositionedText l c t : _) -> ParseError (Loc l c) ("Expecting " <> T.intercalate ", " expected <> ". Found \"" <> t <> "\"")
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
    "1 + 2 * 3 + 4",
    "1+ ",
    "(",
    "{",
    "{}",
    "{} =",
    "{} { }",
    "{} = { }",
    "{x = true}",
    "{x = true, }",
    "{x = true,\n y = false\n}",
    "{x = true,\n y = false,\n}",
    "{x = true,\n y = false,,\n}"
  ]

parseExamples :: IO ()
parseExamples = forM_ examples $ \s -> do
  TIO.putStrLn s
  case parseQCL s of
    Right expr -> putStr "Success: " <> print expr
    Left e -> TIO.putStrLn e
  putStrLn "========================================"
