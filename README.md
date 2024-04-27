# Introduction

QCL is an experimental, dynamically typed "little language" (as defined by
Chapter 6 of *The AWK Programming Language*) that produces JSON output. It is
JSON but more convenient to write, and it allows a little bit of computation
without being Turing complete.

# Motivation

This project mainly exists because I want to play with a natural and intuitive
(to me) syntax. I also want to play with [the Earley
parser](https://en.wikipedia.org/wiki/Earley_parser) which is capable of parsing
context-free languages. I also want to play with making named tuples (aka
dictionaries, JS-style objects) a first-class citizen of the language, such that
almost everything is built on top of it.

I also want to produce extremely helpful and detailed error messages, which
sadly I usually don't get to do too often.

Don't use this in production. This is my playground.

# Demo

This shows real examples. Feel free to run it using the *configlang-examples*
executable.

QCL:

```
true
```

JSON result:

```
true
```

------------

QCL:

```
1.234
```

JSON result:

```
1.234
```

------------

QCL:

```
1+2+3+4+5
```

JSON result:

```
15
```

------------

QCL:

```
17.5 / 5.5
```

JSON result:

```
3.1818181818181817
```

------------

QCL:

```
17.5 // 5.5
```

JSON result:

```
3
```

------------

QCL:

```
17.5 % 5.5
```

JSON result:

```
1
```

------------

QCL:

```
1 + 2 * 3 + 4 < 5
```

JSON result:

```
false
```

------------

QCL:

```
a+b.c.d
```

Error message:
```
error:
    variable reference "a" must be inside a tuple
  |
1 | a+b.c.d
  | ^ top-level variable reference

```

------------

QCL:

```
{}
```

JSON result:

```
{}
```

------------

QCL:

```
{} =
```

Error message:
```
parse error:
  |
1 | {} =
  |    ^ expecting "||", "&&", "!=", "==", ">=", ">", "<=", "<", "-", "+", "//", "%", "/", "*", ".", "{", found "="

```

------------

QCL:

```
{} { }
```

JSON result:

```
{}
```

------------

QCL:

```
{x = true}
```

JSON result:

```
{"x":true}
```

------------

QCL:

```
{x = true, }
```

JSON result:

```
{"x":true}
```

------------

QCL:

```
{x = true,
 y = false
}
```

JSON result:

```
{"x":true,"y":false}
```

------------

QCL:

```
{x = true,
 y = false,
}
```

JSON result:

```
{"x":true,"y":false}
```

------------

QCL:

```
{x = true,
 y = x + 1}
```

JSON result:

```
{"x":true,"y":2}
```

------------

QCL:

```
{private x = true,
 y = x + 1}
```

JSON result:

```
{"y":2}
```

------------

QCL:

```
{x = true,
 assert(true), }
```

JSON result:

```
{"x":true}
```

------------

QCL:

```
{x = 5, assert(x % 2
==
0), }
```

Error message:
```
error:
    assertion failed
  |
1 | {x = 5, assert(x % 2
  |                ^^^^^
2 | ==
  | ^^
3 | 0), }
  | ^ evaluates to false
  |
1 | {x = 5, assert(x % 2
  |                ^ this has value 5

```

------------

QCL:

```
{x = 2,
 x = 1}
```

Error message:
```
error:
    duplicate field label "x" in tuple
  |
2 |  x = 1}
  |  ^ this definition
  |
1 | {x = 2,
  |  ^ earlier definition

```

------------

QCL:

```
1 + { a = 2, b = a + 3}.b
```

JSON result:

```
6
```

------------

QCL:

```
1 + { a = 2, b = a + 3}.b && c
```

Error message:
```
error:
    variable reference "c" must be inside a tuple
  |
1 | 1 + { a = 2, b = a + 3}.b && c
  |                              ^ top-level variable reference

```

------------

QCL:

```
1 + { a = 2, b = a + 3}
```

Error message:
```
error:
    unexpected type for expression
  |
1 | 1 + { a = 2, b = a + 3}
  |     ^^^^^^^^^^^^^^^^^^^ expecting number or boolean

```

------------

QCL:

```
{
 a = 1,
 b = a + a,
 c = a + b + c
}.c
```

Error message:
```
error:
    variable reference "c" does not exist
  |
4 |  c = a + b + c
  |              ^ undefined variable reference

```

------------

QCL:

```
[]
```

JSON result:

```
[]
```

------------

QCL:

```
[1, true]
```

JSON result:

```
[1,true]
```

------------

QCL:

```
{a = {b = {c=1}}, ret = a.b{x=1}}.ret
```

JSON result:

```
{"c":1,"x":1}
```

------------

QCL:

```
{a = {b = {c=1}}, ret = a{x=1}.b}.ret
```

JSON result:

```
{"c":1}
```

------------

QCL:

```
{ x=1, y=2, z=3 } {z = 4}
```

JSON result:

```
{"x":1,"y":2,"z":4}
```

------------

QCL:

```
{ x=1, y=2, z=3 } {z = 4 + z}
```

JSON result:

```
{"x":1,"y":2,"z":7}
```

------------

QCL:

```
{ x=1, y=2, z=3 } {delete z}
```

JSON result:

```
{"x":1,"y":2}
```

------------

QCL:

```
{ x=1, y=2, z=3 } .x
```

JSON result:

```
1
```

------------

QCL:

```
{ x=1, y=2, z=3 }.wwww
```

Error message:
```
error:
    label "wwww" does not exist in tuple
  |
1 | { x=1, y=2, z=3 }.wwww
  |                   ^^^^ tuple has labels "x", "y", "z"

```

------------

QCL:

```
{ x=1, y=2, z=y }
```

JSON result:

```
{"x":1,"y":2,"z":2}
```

------------

QCL:

```
{ x=1, y=2, z={a=1, b=y} }
```

JSON result:

```
{"x":1,"y":2,"z":{"a":1,"b":2}}
```

------------

QCL:

```
{ x=1, y=2, z={a=1, b=a} }
```

JSON result:

```
{"x":1,"y":2,"z":{"a":1,"b":1}}
```

------------

QCL:

```
{ x=1, y=2, z={x=1, y=x} }
```

Error message:
```
error:
    variable reference "x" is ambiguous
  |
1 | { x=1, y=2, z={x=1, y=x} }
  |                       ^ variable reference used here
  |
1 | { x=1, y=2, z={x=1, y=x} }
  |                ^ possible referent
  |
1 | { x=1, y=2, z={x=1, y=x} }
  |   ^ another possible referent

```

------------

QCL:

```
{ a=1, b={ x=2, y=3 } } { b = b { y = y + x + a } }
```

JSON result:

```
{"a":1,"b":{"x":2,"y":6}}
```

------------

QCL:

```
{ a=1, b={ x=2, y=3 } } { b { y = y + x + a } }
```

JSON result:

```
{"a":1,"b":{"x":2,"y":6}}
```

------------

QCL:

```
{ x = 1, y = 2, z = { a = x + 1, b = y + a + 2}}.z.b
```

JSON result:

```
6
```

------------

QCL:

```
{ a=1, b=2 } { a=b+1 } { b=a+1 } { a=b+1 } { b=a+1 }
```

JSON result:

```
{"a":5,"b":6}
```

------------

QCL:

```
{ final meaningOfLife = 42 } { meaningOfLife = 43 }
```

Error message:
```
error:
    field marked as final cannot be overridden
  |
1 | { final meaningOfLife = 42 } { meaningOfLife = 43 }
  |                                ^^^^^^^^^^^^^ override here
  |
1 | { final meaningOfLife = 42 } { meaningOfLife = 43 }
  |         ^^^^^^^^^^^^^ defined here
  |
1 | { final meaningOfLife = 42 } { meaningOfLife = 43 }
  |   ^^^^^ marked as final here

```

------------

QCL:

```
{ final
  meaningOfLife = 42 } { delete meaningOfLife }
```

Error message:
```
error:
    field marked as final cannot be overridden
  |
2 |   meaningOfLife = 42 } { delete meaningOfLife }
  |                                 ^^^^^^^^^^^^^ override here
  |
2 |   meaningOfLife = 42 } { delete meaningOfLife }
  |   ^^^^^^^^^^^^^ defined here
  |
1 | { final
  |   ^^^^^ marked as final here

```

------------

QCL:

```
{ abstract a }
```

Error message:
```
error:
    abstract field cannot be used in non-abstract tuples
  |
1 | { abstract a }
  |   ^^^^^^^^ abstract field

```

------------

QCL:

```
{}.eval
```

Error message:
```
error:
    unexpected type for expression
  |
1 | {}.eval
  | ^^ expecting abstract tuple

```

------------

QCL:

```
# Abstract tuples are not automatically evaluated. It is not evaluated here.
abstract {
  abstract a,
  assert (a % 2 == 0),
  ret = a / 2,
}
```

JSON result:

```
null
```

------------

QCL:

```
# Abstract tuple updates are also not automatically evaluated. It is not evaluated here.
abstract {
  abstract a,
  assert (a % 2 == 0),
  ret = a / 2,
} { a = 42 }
```

JSON result:

```
null
```

------------

QCL:

```
# The abstract tuple must be evaluated explicitly using the eval keyword.
abstract {
  abstract a,
  assert (a % 2 == 0),
  ret = a / 2,
} { a = 42 }.eval
```

JSON result:

```
{"a":42,"ret":21}
```

------------

QCL:

```
{
  checkEven = abstract {
    abstract a,
    assert(a % 2 == 0),
    ret = a / 2,
  },
  e1 = checkEven { a = 100 },
} { e1 = e1.eval.ret, delete checkEven }
```

JSON result:

```
{"e1":50}
```

------------

QCL:

```
{
  checkEven = abstract {
    abstract a,
    # This will fail.
    assert(a % 2 == 0),
    ret = a / 2,
  },
  e1 = checkEven { a = 105 },
} { e1 = e1.eval.ret, delete checkEven }
```

Error message:
```
error:
    assertion failed
  |
5 |     assert(a % 2 == 0),
  |            ^^^^^^^^^^ evaluates to false
  |
5 |     assert(a % 2 == 0),
  |            ^ this has value 105

```

------------

QCL:

```
abstract {
  abstract a,
  assert (a % 2 == 0),
  ret = a / 2,
} {
  a = 42
} {
  a = 64
}.eval # Evaluation happens only after both tuple updates.
```

JSON result:

```
{"a":64,"ret":32}
```

------------

QCL:

```
# Variables in abstract tuples can refer to the surrounding scope (lexical scope).
{ a = 1,
  b = abstract {
    c = a
  }.eval,
  assert (b.c==a)
}
```

JSON result:

```
{"a":1,"b":{"c":1}}
```

------------

QCL:

```
# Variables in abstract tuple updates can also refer to the surrounding scope.
{ a = 1,
  b = abstract {
    abstract c
  }
} {
  b = b {c = a}.eval,
  assert (b.c==a)
}
```

JSON result:

```
{"a":1,"b":{"c":1}}
```

------------

QCL:

```
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
```

JSON result:

```
{"a":2,"b":{"c":1}}
```

------------

QCL:

```
abstract{
  abstract a,
  b = abstract{
    abstract x,
    ret = x*10,
  },
  ret = a + b { x = a * 100}.eval.ret
} { a = 5 }.eval.ret
```

JSON result:

```
5005
```

------------

QCL:

```
# Mutual reference is not allowed
abstract { abstract a, b = a+1 } { a = b+1 } .eval
```

Error message:
```
error:
    variable reference "b" does not exist
  |
2 | abstract { abstract a, b = a+1 } { a = b+1 } .eval
  |                                        ^ undefined variable reference

```

------------

QCL:

```
{a=2, b = abstract { abstract c, assert (c%a == 0) }} { b = b { c = 10 }.eval }
```

JSON result:

```
{"a":2,"b":{"c":10}}
```

------------

QCL:

```
{a=2, b = abstract { abstract c, assert (c%a == 0) }} { b = b { c = 11 }.eval }
```

Error message:
```
error:
    assertion failed
  |
1 | {a=2, b = abstract { abstract c, assert (c%a == 0) }} { b = b { c = 11 }.eval }
  |                                          ^^^^^^^^ evaluates to false
  |
1 | {a=2, b = abstract { abstract c, assert (c%a == 0) }} { b = b { c = 11 }.eval }
  |                                            ^ this has value 2
  |
1 | {a=2, b = abstract { abstract c, assert (c%a == 0) }} { b = b { c = 11 }.eval }
  |                                          ^ this has value 11

```

------------

QCL:

```
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
```

JSON result:

```
{"falseAndFalse":false,"falseAndTrue":false,"falseOrFalse":false,"falseOrTrue":true,"trueAndFalse":false,"trueAndTrue":true,"trueOrFalse":true,"trueOrTrue":true}
```

------------
