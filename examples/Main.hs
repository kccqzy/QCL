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
    "{a = {b = {c=1}}, ret = a.b{x=1}}.ret",
    "{a = {b = {c=1}}, ret = a{x=1}.b}.ret",
    "{ x=1, y=2, z=3 } {z = 4}",
    "{ x=1, y=2, z=3 } {z = 4 + z}",
    "{ x=1, y=2, z=3 } {delete z}",
    "{ x=1, y=2, z=3 } .x",
    "{ x=1, y=2, z=3 }.wwww",
    "{ x=1, y=2, z=y } ",
    "{ x=1, y=2, z={a=1, b=y} } ",
    "{ x=1, y=2, z={a=1, b=a} } ",
    "{ x=1, y=2, z={x=1, y=x} } ",
    "{ a=1, b={ x=2, y=3 } } { b = b { y = y + x + a } }",
    "{ a=1, b={ x=2, y=3 } } { b { y = y + x + a } }",
    "{ x = 1, y = 2, z = { a = x + 1, b = y + a + 2}}.z.b",
    "{ a=1, b=2 } { a=b+1 } { b=a+1 } { a=b+1 } { b=a+1 }",
    "{ final meaningOfLife = 42 } { meaningOfLife = 43 }",
    "{ final\n  meaningOfLife = 42 } { delete meaningOfLife }",
    "{ abstract a }",
    "{}.eval",
    "# Abstract tuples are not eagerly evaluated.\nabstract {\n  abstract a,\n  assert (a % 2 == 0),\n  ret = a / 2,\n}",
    "# Abstract tuple updates are also not eagerly evaluated.\nabstract {\n  abstract a,\n  assert (a % 2 == 0),\n  ret = a / 2,\n} { a = 42 }",
    "abstract {\n  abstract a,\n  assert (a % 2 == 0),\n  ret = a / 2,\n} { a = 42 }\n.eval",
    "{\n\
    \  checkEven = abstract {\n\
    \    abstract a,\n\
    \    assert(a % 2 == 0),\n\
    \    ret = a / 2,\n\
    \  },\n\
    \  e1 = checkEven { a = 100 },\n\
    \} { e1 = e1.eval.ret, delete checkEven }",
    "{\n\
    \  checkEven = abstract {\n\
    \    abstract a,\n\
    \    assert(a % 2 == 0),\n\
    \    ret = a / 2,\n\
    \  },\n\
    \  e1 = checkEven { a = 105 },\n\
    \} { e1 = e1.eval.ret, delete checkEven }",
    "abstract {\n  abstract a,\n  assert (a % 2 == 0),\n  ret = a / 2,\n} { a = 42 } { a = 64 }.eval",
    "# Variables in abstract tuples can refer to the surrounding scope (lexical scope).\n\
    \{a = 1, b = abstract{c = a}.eval, assert (b.c==a)}",
    "# Variables in abstract tuple updates can also refer to the surrounding scope.\n\
    \{a = 1, b = abstract{abstract c}}{b = b{c = a}.eval, assert (b.c==a)}",
    "# Variables in abstract tuple updates refer to the value upon the update, not during evaluation.\n\
    \{a = 1, b = abstract{abstract c}}{b {c = a}, a = a + a, b = b.eval, assert (b.c!=a)}",
    "abstract{\n\
    \  abstract a,\n\
    \  b= abstract{\n\
    \    abstract x,\n\
    \    ret= x*10\n\
    \  },\n\
    \  ret= a+b{x=a*100}.eval.ret\n\
    \} { a = 5 }.eval.ret",
    "{assert(1004==abstract{\n\
    \  abstract a,\n\
    \  b= abstract{\n\
    \    abstract x,\n\
    \    ret= x*10\n\
    \  },\n\
    \  ret= a+b{x=a*100}.eval.ret\n\
    \} { a = 5 }.eval.ret)}",
    "# Mutual reference is not allowed\n\
    \abstract { abstract a, b = a+1 } { a = b+1 } .eval",
    "{a=2, b = abstract { abstract c, assert (c%a == 0) }} { b = b { c = 10 }.eval }",
    "{a=2, b = abstract { abstract c, assert (c%a == 0) }} { b = b { c = 11 }.eval }"
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
