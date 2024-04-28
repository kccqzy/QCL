{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable (forM_)
import Data.Text (Text)
import qualified Data.Text.IO.Utf8 as TIO
import Data.Text.Lazy.Encoding (encodeUtf8)
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
    "{x = true,\n y }",
    "{private x = true,\n y = x + 1}",
    "{private x = true, y = x + 1} { x = false }",
    "{private x = true, y = x + 1} { delete x }",
    "{private x = true, y = x + 1}.x",
    "{private x = true} { private x = false}",
    "{x = true,\n assert(true), }",
    "{x = 5, assert(x % 2\n==\n0), }",
    "{x = 2,\n x = 1}",
    "1 + { a = 2, b = a + 3}.b",
    "1 + { a = 2, b = a + 3}",
    "{\n a = 1,\n b = a + a,\n c = a + b + c\n}.c",
    "[]",
    "[1, true]",
    "{a = {b = {c=1}}, ret = a.b{x=1}}.ret",
    "{a = {b = {c=1}}, ret = a{x=1}.b}.ret",
    "{ x=1, y=2, z=3 } {z = 4}",
    "{ x=1, y=2, z=3 } {z += 4}",
    "{ x=1, y=2, z=3 } {z *= z}",
    "{ x=1, y=2, z=3 } {delete z}",
    "{ x=1, y=2, z=3 } .x",
    "{ x=1, y=2, z=3 }.wwww",
    "{ x=1, y=2, z=y }",
    "{ x=1, y=2, z={a=1, b=y} }",
    "{ x=1, y=2, z={a=1, b=a} }",
    "{ x=1, y=2, z={x=1, y=x} }",
    "{ inner = { private x = 1, y = 2 }, result = inner.x }",
    "{ private x = 1, inner = { y = 2 + x } }",
    "# A convenient syntax for nested tuple updates.\n{ a=1, b={ x=2, y=3 } } { b { y = 100 } }",
    [text|
         { z = { irrelevant = 1000 },
           inner = {
             z = { x = 0 }
           } {
             z { x += 1 } # This z updates the field in the current tuple, not the outside one.
           }
         }
         |],
    "{ x = 1, y = 2, z = { a = x + 1, b = y + a + 2}}.z.b",
    "{ a=1, b=2 } { a=b+1 } { b=a+1 } { a=b+1 } { b=a+1 }",
    "{ final meaningOfLife = 42 } { meaningOfLife = 43 }",
    "{ final\n  meaningOfLife = 42 } { delete meaningOfLife }",
    [text|
         {
           # Perl-style boolean operators.
           oneOrTwo    = 1 || 2,
           zeroOrTwo   = 0 || 2,
           oneAndTwo   = 1 && 2,
           zeroAndTwo  = 0 && 2,
           # Substitute for the conditional operator.
           ifTrue      = 1 && 100 || 200,
           ifFalse     = 0 && 100 || 200,
           # Short-circuiting. Even type errors are not found.
           simplyFalse = false && (1+{}),
           simplyTrue  = true  || (1+{}),
         }
         |],
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
            private checkEven = abstract {
              abstract a,
              assert(a % 2 == 0),
              ret = a / 2,
            },
            e1 = checkEven { a = 100 },
          } { e1 = e1.eval.ret }
         |],
    [text|
          {
            private checkEven = abstract {
              abstract a,
              # This will fail.
              assert(a % 2 == 0),
              ret = a / 2,
            },
            e1 = checkEven { a = 105 },
          } { e1 = e1.eval.ret }
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
    [text|
         # Abstract tuples delay evaluation. They allow but do not require abstract fields.
         abstract {
           x = 0,
           y = 0,
           ret = x * y,  # ret is evaluated after the overriding x and y
         } {
           x = 2,
           y = 4,
         }.eval
         |],
    [text|
         abstract {
           x = 2,
           y = 4,
           ret = x * y,
         } {
           # Will not work because the previous value of x does not exist yet.
           x += 1,
         }.eval
         |],
    [text|
         abstract {
           x = 2,
           y = 4,
           ret = x * y,
         }.eval {
           # Works.
           x += 1,
         }
         |],
    [text|
         { x = 100,
           inner = abstract {
             x = 2,
             y = 4,
           } {
             # Still does not work.
             x += 1,
           }.eval
         }
         |],
    [text|
         { x = 100,
           inner = abstract {
             x = 2,
             y = 4,
           } {
             # Works, but refers to the outer x.
             x = x + 1,
           }.eval
         }
         |],
    "{a=2, b = abstract { abstract c, assert (c%a == 0) }} { b = b { c = 10 }.eval }",
    "{a=2, b = abstract { abstract c, assert (c%a == 0) }} { b = b { c = 11 }.eval }",
    "abstract { abstract x, private y = x + x, z = y * y } { x = 10 }.eval",
    "abstract { abstract x, private y = x + x } { x = 10 }.eval.y",
    [text|
         # It is possible to mark an overridden field private. It is orthogonal
         # to the evaluation order.
         abstract {
           abstract x,
           ret = x * x,
         } {
           private x = 10,
         }.eval
         |],
    [text|
         # It is also possible to mark an overridden field final.
         abstract {
           abstract x,
           ret = x * x,
         } {
           final x = 10,
         }.eval
         |],
    [text|
         # This example showcases Church-encoded booleans in an untyped lambda calculus.
         {
           private t = abstract { abstract a, abstract b, ret = a },
           private f = abstract { abstract a, abstract b, ret = b },

           # Boolean and
           private and = abstract { abstract p, abstract q, ret = p { a = q, b = p }},
           trueAndFalse  = and { p = t, q = f }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
           falseAndTrue  = and { p = f, q = t }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
           trueAndTrue   = and { p = t, q = t }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
           falseAndFalse = and { p = f, q = f }.eval.ret.eval.ret{a=true,b=false}.eval.ret,

           # Boolean or
           private or = abstract { abstract p, abstract q, ret = p { a = p, b = q }},
           trueOrFalse  = or { p = t, q = f }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
           falseOrTrue  = or { p = f, q = t }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
           trueOrTrue   = or { p = t, q = t }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
           falseOrFalse = or { p = f, q = f }.eval.ret.eval.ret{a=true,b=false}.eval.ret,
         }
         |],
    [text|
          # The omega combinator. In lambda calculus, the omega combinator diverges (infinite
          # loop). But in this language, every abstract tuple evaluation must be explicit.
          # Therefore, by placing the eval at the right place, it is possible to see each
          # stage of applying the omega combinator.
          {
            omega = abstract {
              abstract x,
              ret = x { x = x } # This program would loop if the eval keyword is here.
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
          |],
    [text|
         {
           # This implements a recursive function call using the well-known trick of
           # having an argument to refer to the recursion and passing the function itself.
           factorial = abstract {
             abstract x,
             abstract rec,
             ret = x == 0 && 1 || x * rec { x = x - 1, rec = rec }.eval.ret
           },
         } {
           final factorial { rec = factorial }
         } {
           f10 = factorial { x = 10 }.eval.ret
         }
         |]
  ]

main :: IO ()
main = forM_ examples $ \s -> do
  putStrLn "\nQCL:\n\n```"
  TIO.putStrLn s
  putStrLn "```"
  case evalQCL s of
    Right value -> do
      putStrLn "\nJSON result:\n\n```"
      BL.putStrLn (Aeson.encode value)
      putStrLn "```\n"
    Left e -> do
      putStrLn "\nError message:\n```"
      BL.putStrLn (encodeUtf8 e)
      putStrLn "```\n"
  putStrLn "------------"
