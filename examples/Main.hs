{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable (forM_)
import Data.Text (Text)
import qualified Data.Text.IO.Utf8 as TIO
import QCL

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
    "{private x = true,\n y = x + 1}",
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
    "{ x=1, y=2, z=3 } {z = 4}",
    "{ x=1, y=2, z=3 } {z = null}",
    "{ x=1, y=2, z=3 } .x",
    "{ x=1, y=2, z=3 }.wwww",
    "{ x=1, y=2, z=y } ",
    "{ x=1, y=2, z={a=1, b=y} } ",
    "{ x=1, y=2, z={a=1, b=a} } ",
    "{ x=1, y=2, z={x=1, y=x} } ",
    "{ x = 1, y = 2, z = { a = x + 1, b = y + a + 2}}.z.b",
    "{ a=1, b={ a=1, b={ a=1, b={ c=a } } } }",
    "{ final meaningOfLife = 42 } { meaningOfLife = 43 }",
    "{ final\n  meaningOfLife = 42 } { meaningOfLife = null }"
  ]

main :: IO ()
main = forM_ examples $ \s -> do
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
