# Introduction

QCL is an experimental, dynamically typed "little language" (as defined by
Chapter 6 of *The AWK Programming Language*) that produces JSON output. It is
JSON but more convenient to write, and it allows a little bit of computation.

# Motivation

This project mainly exists because I want to play with a natural and intuitive
(to me) syntax. I also want to play with [the Earley
parser](https://en.wikipedia.org/wiki/Earley_parser) which is capable of parsing
context-free languages (I want my language to be strictly context-free after
lexing). I also want to play with making named tuples (aka dictionaries,
JS-style objects) a first-class citizen of the language, such that almost
everything is built on top of it. The abstract tuple is a powerful feature
whereby the evaluation of the tuple is delayed until such time intended by the
user through the `eval` keyword. It can be thought of as providing a *template*
tuple with values to be filled in later; it can also be thought of as
introducing a lambda function where the values to be filled in are function
arguments. These lambdas are closures and capture their environment.

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
{x = true,
 y }
```

Error message:
```
parse error:
  |
2 |  y }
  |    ^ expecting "%=", "//=", "/=", "*=", "-=", "+=", "{", "=", found "}"

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
{private x = true, y = x + 1} { x = false }
```

Error message:
```
error:
    field marked as private cannot be accessed outside its enclosing tuple
  |
1 | {private x = true, y = x + 1} { x = false }
  |                                 ^ access of private field here
  |
1 | {private x = true, y = x + 1} { x = false }
  |          ^ defined here
  |
1 | {private x = true, y = x + 1} { x = false }
  |  ^^^^^^^ marked as private here

```

------------

QCL:

```
{private x = true, y = x + 1} { delete x }
```

Error message:
```
error:
    field marked as private cannot be accessed outside its enclosing tuple
  |
1 | {private x = true, y = x + 1} { delete x }
  |                                        ^ access of private field here
  |
1 | {private x = true, y = x + 1} { delete x }
  |          ^ defined here
  |
1 | {private x = true, y = x + 1} { delete x }
  |  ^^^^^^^ marked as private here

```

------------

QCL:

```
{private x = true, y = x + 1}.x
```

Error message:
```
error:
    field marked as private cannot be accessed outside its enclosing tuple
  |
1 | {private x = true, y = x + 1}.x
  |                               ^ access of private field here
  |
1 | {private x = true, y = x + 1}.x
  |          ^ defined here
  |
1 | {private x = true, y = x + 1}.x
  |  ^^^^^^^ marked as private here

```

------------

QCL:

```
{private x = true} { private x = false}
```

Error message:
```
error:
    field marked as private cannot be accessed outside its enclosing tuple
  |
1 | {private x = true} { private x = false}
  |                              ^ access of private field here
  |
1 | {private x = true} { private x = false}
  |          ^ defined here
  |
1 | {private x = true} { private x = false}
  |  ^^^^^^^ marked as private here

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
1 + { a = 2, b = a + 3}
```

Error message:
```
error:
    unexpected type for expression
  |
1 | 1 + { a = 2, b = a + 3}
  |     ^^^^^^^^^^^^^^^^^^^ expecting number or boolean, found tuple

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
{ x=1, y=2, z=3 } {z += 4}
```

JSON result:

```
{"x":1,"y":2,"z":7}
```

------------

QCL:

```
{ x=1, y=2, z=3 } {z *= z}
```

JSON result:

```
{"x":1,"y":2,"z":9}
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
{ inner = { private x = 1, y = 2 }, result = inner.x }
```

Error message:
```
error:
    field marked as private cannot be accessed outside its enclosing tuple
  |
1 | { inner = { private x = 1, y = 2 }, result = inner.x }
  |                                                    ^ access of private field here
  |
1 | { inner = { private x = 1, y = 2 }, result = inner.x }
  |                     ^ defined here
  |
1 | { inner = { private x = 1, y = 2 }, result = inner.x }
  |             ^^^^^^^ marked as private here

```

------------

QCL:

```
{ private x = 1, inner = { y = 2 + x } }
```

JSON result:

```
{"inner":{"y":3}}
```

------------

QCL:

```
# A convenient syntax for nested tuple updates.
{ a=1, b={ x=2, y=3 } } { b { y = 100 } }
```

JSON result:

```
{"a":1,"b":{"x":2,"y":100}}
```

------------

QCL:

```
{ z = { irrelevant = 1000 },
  inner = {
    z = { x = 0 }
  } {
    z { x += 1 } # This z updates the field in the current tuple, not the outside one.
  }
}
```

JSON result:

```
{"inner":{"z":{"x":1}},"z":{"irrelevant":1000}}
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
```

JSON result:

```
{"ifFalse":200,"ifTrue":100,"oneAndTwo":2,"oneOrTwo":1,"simplyFalse":false,"simplyTrue":true,"zeroAndTwo":0,"zeroOrTwo":2}
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
  | ^^ expecting abstract tuple, found tuple

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
  private checkEven = abstract {
    abstract a,
    assert(a % 2 == 0),
    ret = a / 2,
  },
  e1 = checkEven { a = 100 },
} { e1 = e1.eval.ret }
```

JSON result:

```
{"e1":50}
```

------------

QCL:

```
{
  private checkEven = abstract {
    abstract a,
    # This will fail.
    assert(a % 2 == 0),
    ret = a / 2,
  },
  e1 = checkEven { a = 105 },
} { e1 = e1.eval.ret }
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
# Abstract tuples delay evaluation. They allow but do not require abstract fields.
abstract {
  x = 0,
  y = 0,
  ret = x * y,  # ret is evaluated after the overriding x and y
} {
  x = 2,
  y = 4,
}.eval
```

JSON result:

```
{"ret":8,"x":2,"y":4}
```

------------

QCL:

```
abstract {
  x = 2,
  y = 4,
  ret = x * y,
} {
  # Will not work because the previous value of x does not exist yet.
  x += 1,
}.eval
```

Error message:
```
error:
    variable reference "x" does not exist in current tuple
  |
7 |   x += 1,
  |   ^ undefined variable reference

```

------------

QCL:

```
abstract {
  x = 2,
  y = 4,
  ret = x * y,
}.eval {
  # Works.
  x += 1,
}
```

JSON result:

```
{"ret":8,"x":3,"y":4}
```

------------

QCL:

```
{ x = 100,
  inner = abstract {
    x = 2,
    y = 4,
  } {
    # Still does not work.
    x += 1,
  }.eval
}
```

Error message:
```
error:
    variable reference "x" does not exist in current tuple
  |
7 |     x += 1,
  |     ^ undefined variable reference

```

------------

QCL:

```
{ x = 100,
  inner = abstract {
    x = 2,
    y = 4,
  } {
    # Works, but refers to the outer x.
    x = x + 1,
  }.eval
}
```

JSON result:

```
{"inner":{"x":101,"y":4},"x":100}
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
abstract { abstract x, private y = x + x, z = y * y } { x = 10 }.eval
```

JSON result:

```
{"x":10,"z":400}
```

------------

QCL:

```
abstract { abstract x, private y = x + x } { x = 10 }.eval.y
```

Error message:
```
error:
    field marked as private cannot be accessed outside its enclosing tuple
  |
1 | abstract { abstract x, private y = x + x } { x = 10 }.eval.y
  |                                                            ^ access of private field here
  |
1 | abstract { abstract x, private y = x + x } { x = 10 }.eval.y
  |                                ^ defined here
  |
1 | abstract { abstract x, private y = x + x } { x = 10 }.eval.y
  |                        ^^^^^^^ marked as private here

```

------------

QCL:

```
# It is possible to mark an overridden field private. It is orthogonal
# to the evaluation order.
abstract {
  abstract x,
  ret = x * x,
} {
  private x = 10,
}.eval
```

JSON result:

```
{"ret":100}
```

------------

QCL:

```
# It is also possible to mark an overridden field final.
abstract {
  abstract x,
  ret = x * x,
} {
  final x = 10,
}.eval
```

JSON result:

```
{"ret":100,"x":10}
```

------------

QCL:

```
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
```

JSON result:

```
{"falseAndFalse":false,"falseAndTrue":false,"falseOrFalse":false,"falseOrTrue":true,"trueAndFalse":false,"trueAndTrue":true,"trueOrFalse":true,"trueOrTrue":true}
```

------------

QCL:

```
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
```

JSON result:

```
{"o1":null,"o2":null,"o3":null,"o4":null,"o5":null,"o6":null,"omega":null}
```

------------

QCL:

```
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
```

JSON result:

```
{"f10":3628800,"factorial":null}
```

------------
