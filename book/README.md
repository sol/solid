# Solid for Haskell programmers

by Simon Hengel

<!--ts-->

* [How to read this book](#how-to-read-this-book)
* [Basic types and operations](#basic-types-and-operations)
   * [Functions, methods, and associated modules](#functions-methods-and-associated-modules)
   * [Using a method as a function](#using-a-method-as-a-function)
   * [Argument order](#argument-order)
   * [Strings and binary data](#strings-and-binary-data)
      * [String interpolation](#string-interpolation)
      * [ByteString literals](#bytestring-literals)
* [Extending Solid](#extending-solid)
   * [Extending Solid with Haskell](#extending-solid-with-haskell)
   * [Extending Solid with C](#extending-solid-with-c)
* [Proposals (not yet implemented, feedback welcome)](#proposals-not-yet-implemented-feedback-welcome)
   * [Method chaining](#method-chaining)



<!--te-->

# How to read this book

This book is for the impatient Haskell programmer.  It gives a no-frills
introduction to Solid and focuses on the "how" rather than the "why".  As a
reader you should be proficient in Haskell. Solid is mostly a superset of
Haskell, and Haskell concepts are not explained in this book.

If you want to experiment with the examples given in this book in a REPL then:

1. Clone the repository
1. Change into the `book` directory
1. Run `make repl`

```
$ git clone https://github.com/sol/solid
$ cd solid/book
$ make repl
```

# Basic types and operations

## Functions, methods, and associated modules

All basic Solid types have an associated module of the same name.  This module
provides operations on values of that type.

The [`String`][string] module provides operations on `String` values, the
[`ByteString`][byte-string] module provides operations on `ByteString` values,
the [`Word8`][word8] module provides operations on `Word8` values, and so on.

All those operations are available both as functions and as methods.

**Example:**

```haskell
name :: String
name = "Joe"
```

```repl
Function syntax:
>>> String.length name
3

Method syntax:
>>> name.length
3
```

> __Implementation note:__
>
> Solid uses the [`OverloadedRecordDot`][overloaded_record_dot] language
> extension to provide method syntax.

## Using a method as a function

A method can be used as a regular function by enclosing it in parentheses.

**Example:**

```repl
>>> (.length) name
3
```

## Argument order

Functions and methods take their arguments in a different order.  The function
variant of an operation takes the subject as the last argument, while the
method variant takes the subject as the first argument.  This allows for easy
currying.

**Example:**

```repl
Method:
>>> name.startsWith "Jo"
True

Method as a function:
>>> (.startsWith) name "Jo"
True

Function:
>>> String.startsWith "Jo" name
True

Currying:
>>> let is_a_joe = String.startsWith "Jo"

>>> is_a_joe name
True

>>> is_a_joe "John Doe"
True
```

## Strings and binary data

### String interpolation

**Example:**

```haskell
say_hey :: String -> IO ()
say_hey name = stdout.writeLine "Hey {name} 👋"
```
```repl
>>> say_hey "Joe"
Hey Joe 👋
```
> __Implementation note:__
>
> String interpolation is implemented via a [GHC
> pre-processor][haskell-pre-processor].

### ByteString literals

Solid and Haskell handle `ByteString` literals differently.  Solid encodes
`ByteString` literals as UTF-8, while Haskell truncates `ByteString` literals
to octets.

**Example:**

```haskell
wave :: IsString a => a
wave = "👋"
```

```repl
The Unicode code point of the wave emoji is 0x1F44B:
>>> '👋'.ord.showHex
"1f44b"

The UTF-8 encoding of the wave emoji is 0xF0 0x9F 0x91 0x8B:
>>> (wave @String).asByteString.unpack.map showHex
["f0","9f","91","8b"]

Solid encodes ByteString literals as UTF-8:
>>> (wave @ByteString).unpack.map showHex
["f0","9f","91","8b"]

Haskell truncates ByteString literals to octets:
>>> map showHex $ Data.ByteString.unpack wave
["4b"]
```

# Extending Solid

## Extending Solid with Haskell

[`Solid.Foreign.Haskell`][foreign-haskell] provides functions that are useful
when you want to use existing Haskell code from Solid.

**Example:**

```haskell top
import Data.Text qualified as Text
import Solid.Foreign.Haskell qualified as Haskell
```

```haskell
toCaseFold :: String -> String
toCaseFold = Haskell.fromText . Text.toCaseFold . Haskell.toText

instance HasField "toCaseFold" String String where
  getField = toCaseFold
```
```repl
>>> toCaseFold name
"joe"

>>> name.toCaseFold
"joe"
```

## Extending Solid with C

[`Solid.Foreign.C`][foreign-c] provides functions that are useful when you want
to use existing C code from Solid.

**Example:**

```haskell top
import Solid.Foreign.C qualified as C
```

```haskell
foreign import ccall unsafe "wchar.h wcwidth" c_wcwidth :: C.WChar -> C.Int
```

```haskell
wcwidth :: Char -> Int
wcwidth = fromEnum . c_wcwidth . C.toWChar
```
```repl
>>> wcwidth '👋'
2
```

# Proposals (not yet implemented, feedback welcome)

## Method chaining

When you want to chain multiple methods that take arguments then you have to
use nested parentheses.

**Example:**

```repl
Set the process environment and capture stdout:
>>> ((Process.shell "pwd").chdir "/tmp").stdout.capture.with Process.stdout
"/tmp\n"
```

This can quickly become unwieldy.

To remedy this situation we steal some Haskell syntax that is not commonly
used, _identifiers that are directly followed by an opening parenthesis without
any separating whitespace_, and desugar it for our own needs:

We desugar:

1. `ident(exp)` to `(ident (exp))`
1. `ident(exp_1, exp_2, ..., exp_n)` to `(ident (exp_1) (exp_2) ... (exp_n))`

With this you can simplify the example from above to:

```haskell ignore
Process.shell("pwd").chdir("/tmp").stdout.capture.with Process.stdout
```

[string]: ../src/String.hs
[byte-string]: ../src/ByteString.hs
[bytes]:../src/Solid/Bytes.hs
[word8]: ../src/Word8.hs
[foreign-c]: ../src/Solid/Foreign/C.hs
[foreign-haskell]: ../src/Solid/Foreign/Haskell.hs
[overloaded_record_dot]: https://downloads.haskell.org/ghc/9.6.2/docs/users_guide/exts/overloaded_record_dot.html
[haskell-pre-processor]: https://downloads.haskell.org/ghc/9.6.2/docs/users_guide/phases.html#options-affecting-a-haskell-pre-processor
