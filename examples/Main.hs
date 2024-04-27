{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable (forM_)
import Data.Text (Text)
import qualified Data.Text.IO.Utf8 as TIO
import NeatInterpolation (text)
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
    [text|
          # Abstract tuples are not automatically evaluated. It is not evaluated here.
          abstract {
            abstract a,
            assert (a % 2 == 0),
            ret = a / 2,
          }
         |],
    [text|
          # Abstract tuple updates are also not automatically evaluated. It is not evaluated here.
          abstract {
            abstract a,
            assert (a % 2 == 0),
            ret = a / 2,
          } { a = 42 }
         |],
    [text|
         # The abstract tuple must be evaluated explicitly using the eval keyword.
         abstract {
           abstract a,
           assert (a % 2 == 0),
           ret = a / 2,
         } { a = 42 }.eval
         |],
    [text|
          {
            checkEven = abstract {
              abstract a,
              assert(a % 2 == 0),
              ret = a / 2,
            },
            e1 = checkEven { a = 100 },
          } { e1 = e1.eval.ret, delete checkEven }
         |],
    [text|
          {
            checkEven = abstract {
              abstract a,
              # This will fail.
              assert(a % 2 == 0),
              ret = a / 2,
            },
            e1 = checkEven { a = 105 },
          } { e1 = e1.eval.ret, delete checkEven }
         |],
    [text|
         abstract {
           abstract a,
           assert (a % 2 == 0),
           ret = a / 2,
         } {
           a = 42
         } {
           a = 64
         }.eval # Evaluation happens only after both tuple updates.
         |],
    [text|
         # Variables in abstract tuples can refer to the surrounding scope (lexical scope).
         { a = 1,
           b = abstract {
             c = a
           }.eval,
           assert (b.c==a)
         }
         |],
    [text|
         # Variables in abstract tuple updates can also refer to the surrounding scope.
         { a = 1,
           b = abstract {
             abstract c
           }
         } {
           b = b {c = a}.eval,
           assert (b.c==a)
         }
         |],
    [text|
          # Variables in abstract tuple updates refer to the value upon the update, not during evaluation.
          { a = 1,
            b = abstract {
              abstract c
            }
          } {
            b {c = a}, # This `a` is 1.
            a = a + a, # `a` becomes 2
            b = b.eval, # Evaluation of `b` uses the `a` whose value is 1.
            assert (b.c!=a)
          }
         |],
    [text|
          abstract{
            abstract a,
            b = abstract{
              abstract x,
              ret = x*10,
            },
            ret = a + b { x = a * 100}.eval.ret
          } { a = 5 }.eval.ret
          |],
    [text|
        # Mutual reference is not allowed
        abstract { abstract a, b = a+1 } { a = b+1 } .eval
         |],
    "{a=2, b = abstract { abstract c, assert (c%a == 0) }} { b = b { c = 10 }.eval }",
    "{a=2, b = abstract { abstract c, assert (c%a == 0) }} { b = b { c = 11 }.eval }",
    [text|
         # This example showcases Church-encoded booleans in an untyped lambda calculus.
         {
           t = abstract { abstract a, abstract b, ret = a },
           f = abstract { abstract a, abstract b, ret = b },

           # Boolean and
           and = abstract { abstract p, abstract q, ret = p { a = q, b = p }},
           trueAndFalse  = and { p = t, q = f }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
           falseAndTrue  = and { p = f, q = t }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
           trueAndTrue   = and { p = t, q = t }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
           falseAndFalse = and { p = f, q = f }.eval.ret.eval.ret{a=true,b=false}.eval.ret,

           # Boolean or
           or = abstract { abstract p, abstract q, ret = p { a = p, b = q }},
           trueOrFalse  = or { p = t, q = f }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
           falseOrTrue  = or { p = f, q = t }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
           trueOrTrue   = or { p = t, q = t }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
           falseOrFalse = or { p = f, q = f }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
         } { delete t, delete f, delete and, delete or }
         |],
    [text|
          # The omega combinator. In lambda calculus, the omega combinator diverges (infinite
          # loop). But in this language, every abstract tuple evaluation must be explicit.
          # Therefore, an infinite loop is impossible: it would take an infinitely long program!
          {
            omega = abstract {
              abstract x,
              ret = x { x = x }
            },
          } {
            omega { x = omega },
            o1 = omega.eval.ret,
            o2 = omega.eval.ret.eval.ret,
            o3 = omega.eval.ret.eval.ret.eval.ret,
            o4 = omega.eval.ret.eval.ret.eval.ret.eval.ret,
            o5 = omega.eval.ret.eval.ret.eval.ret.eval.ret.eval.ret,
            o6 = omega.eval.ret.eval.ret.eval.ret.eval.ret.eval.ret.eval.ret,
            # Ad infitinum.
          }
          |]
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
