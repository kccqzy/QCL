{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Functor
import Data.List (find, unfoldr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Text.Earley as E

data Positioned a = Positioned { pBegin :: (Int, Int), pEnd :: (Int, Int), pValue :: a }
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
  | Var Text
  | Member PositionedExpr PositionedText
  deriving (Show)

data TupleExpr
  = Row PositionedText PositionedRowExpr
  | Assertion PositionedExpr
  deriving (Show)

data RowExpr
  = ValuedRow Expr
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
                    Right (n, remaining) ->
                      let len = T.length t - T.length remaining
                          newcolno = colno + len
                       in Just (Positioned (lineno, colno) (lineno, newcolno) (T.take len t), (colno, remaining))
                    Left _ -> error "unexpected error while tokenizing"
              | Just op <- find (`T.isPrefixOf` t) multiCharPunct ->
                  let newcolno = colno + T.length op in Just (Positioned (lineno, colno) (lineno, newcolno) op, (newcolno, T.drop (T.length op) t))
              | otherwise ->
                   let newcolno = colno + 1 in Just (Positioned (lineno, colno) (lineno, newcolno) (T.singleton h), (newcolno, r))

multiCharPunct :: [Text]
multiCharPunct = ["&&", "||", "==", "!=", "<=", ">="]

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
lit t = () <$$ E.satisfy (\(Positioned {pValue=tok}) -> tok == t) E.<?> T.snoc (T.cons '"' t) '"'

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
            op <- Mul <$ lit "*" <|> Div <$ lit "/" <|> Mod <$ lit "%"
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
    E.rule $ (Var <$>) <$> E.terminal identifier E.<?> "identifier"

  tuple :: E.Prod r Text PositionedText PositionedExpr <-
    E.rule $ (Tuple <$>) <$> between (lit "{") (lit "}") tupleContents

  tupleContents <- tupleItem `sepEndBy` lit ","

  tupleItem <- E.rule $ tupleRow <|> tupleAssertion

  tupleRow <- E.rule $ do
    k <- E.terminal identifier E.<?> "identifier"
    _ <- lit "="
    v <- tupleRowValue
    pure (fromPosition2 k v (Row k v))

  tupleRowValue <- E.rule $ (NullRow <$$ lit "null") <|> ((ValuedRow <$>) <$> val)

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
        ([], Positioned b _ t : _) -> ParseError (Loc b) ("Expecting EOF. Found \"" <> t <> "\"")
        (_ : _, Positioned b _ t : _) -> ParseError (Loc b) ("Expecting " <> T.intercalate ", " expected <> ". Found \"" <> t <> "\"")
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
