{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module QCL (parseQCL) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Char (isAlpha, isAlphaNum)
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

readNumber :: Text -> Maybe Double
readNumber t = case TR.double t of
  Right (n, "") -> Just n
  _ -> Nothing

reservedWords :: [Text]
reservedWords = ["true", "false", "assert"]

identifier :: Text -> Maybe Text
identifier t = do
  (h, r) <- T.uncons t
  if isAlpha h && T.all isAlphaNum r && t `notElem` reservedWords then Just t else Nothing

between :: Applicative m => m bra -> m ket -> m a -> m a
between bra ket p = bra *> p <* ket

sepEndBy :: E.Prod r e t a -> E.Prod r e t b -> E.Grammar r (E.Prod r e t [a])
sepEndBy p sep = mfix (\rule -> E.rule $ (:) <$> p <*> ((sep *> rule) <|> pure []) <|> pure [])

sepBy1 :: E.Prod r e t a -> E.Prod r e t b -> E.Grammar r (E.Prod r e t [a])
sepBy1 p sep = mfix (\rule -> E.rule $ (:) <$> p <*> ((sep *> rule) <|> pure []) )

expr :: E.Grammar r (E.Prod r Text Text Expr)
expr = mdo
  bool <- E.rule $ (Boolean True <$ E.namedToken "true") <|> (Boolean False <$ E.namedToken "false")
  number <- E.rule $ Number <$> E.terminal readNumber E.<?> "number literal"
  addExpr <- E.rule $  (do t1 <- addExpr; op <- Plus <$ E.namedToken "+" <|> Minus <$ E.namedToken "-"; t2 <- mulExpr; pure (op t1 t2)) <|> mulExpr
  mulExpr <- E.rule $  (do t1 <- mulExpr; op <- Mul <$ E.namedToken "*" <|> Div <$ E.namedToken "/"; t2 <- atom; pure (op t1 t2)) <|> atom
  varRefs <- (E.terminal identifier `sepBy1` E.namedToken ".")
  var <- E.rule $ VarRef <$> varRefs
  atom <- E.rule $ var <|> bool <|> number <|> between (E.namedToken "(" ) (E.namedToken ")") val
  tupleRow <- E.rule $ do
    k <- E.terminal identifier E.<?> "identifier"
    _ <- E.namedToken "="
    v <- val
    pure (Row k v)
  tupleAssertion <- E.rule $ Assertion <$> (E.namedToken "assert" *> between (E.namedToken "(") (E.namedToken ")") val)
  tupleContents <- tupleItem `sepEndBy` E.namedToken ","
  tupleItem <- E.rule $ tupleRow <|> tupleAssertion
  tuple <- E.rule $ Tuple <$> between (E.namedToken "{") (E.namedToken "}") tupleContents
  val <- E.rule $ addExpr <|> tuple
  pure val

parseQCL :: Text -> ([Expr], E.Report Text [Text])
parseQCL = E.fullParses (E.parser expr) . T.words

examples :: [Text]
examples =
  [ "true",
    "1.234",
    "1 + 2 * 3 + 4",
    "{",
    "{ }",
    "{ } =",
    "{ } { }",
    "{ } = { }",
    "{ x = true }",
    "{ x = true , }",
    "{ x = true , y = false }",
    "{ x = true , y = false , }"
  ]

parseExamples :: IO ()
parseExamples = forM_ examples $ \s -> do
  TIO.putStrLn s
  print (parseQCL s)
  putStrLn "========================================"
