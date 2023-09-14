# Solid for Haskell programmers

by Simon Hengel

<!--ts-->

* [How to read this book](#how-to-read-this-book)
* [Basic types and operations](#basic-types-and-operations)
   * [Functions, methods, and associated modules](#functions-methods-and-associated-modules)
   * [Using a method as a function](#using-a-method-as-a-function)
   * [Argument order](#argument-order)
   * [Method chaining](#method-chaining)
   * [Strings and binary data](#strings-and-binary-data)
      * [String interpolation](#string-interpolation)
      * [ByteString literals](#bytestring-literals)
* [Terminal I/O](#terminal-io)
* [Subprocess management](#subprocess-management)
   * [Running processes](#running-processes)
   * [Interacting with processes](#interacting-with-processes)
      * [Spawning processes](#spawning-processes)
      * [Standard streams, environment, working directory](#standard-streams-environment-working-directory)
      * [Exit status](#exit-status)
* [Extending Solid](#extending-solid)
   * [Extending Solid with Haskell](#extending-solid-with-haskell)
   * [Extending Solid with C](#extending-solid-with-c)
* [Proposals (not yet implemented, feedback welcome)](#proposals-not-yet-implemented-feedback-welcome)
   * [Revised import syntax](#revised-import-syntax)

<!-- Created by https://github.com/ekalinin/github-markdown-toc -->


<!--te-->

# How to read this book

This book is for the impatient Haskell programmer.  It gives a no-frills
introduction to Solid and focuses on the "how" rather than the "why".  As a
reader you should be proficient in Haskell. Solid is mostly a superset of
Haskell, and Haskell concepts are not explained in this book.

If you want to experiment with the examples given in this book in a REPL then:

1. Clone the repository
1. Change into the `book` directory
1. Run `solid repl README.lhs`

```
$ git clone https://github.com/sol/solid
$ cd solid/book
$ solid repl README.lhs
```

# Basic types and operations

## Functions, methods, and associated modules

All basic Solid types have an associated module of the same name.  This module
provides operations on values of that type.

The [`String`][string] module provides operations on `String` values, the
[`ByteString`][byte-string] module provides operations on `ByteString` values,
the [`Either`][either] module provides operations on `Either` values, and so on.

Every operation is available both as a function and as a method.

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

## Method chaining

When you want to chain multiple methods that take arguments then you have to
use nested parentheses.

**Example:**

```repl
>>> ((Process.shell "pwd").chdir "/tmp").read
"/tmp\n"
```

This can quickly become unwieldy.

To remedy this situation Solid steals some Haskell syntax that is not commonly
used, _identifiers that are directly followed by an opening parenthesis without
any separating whitespace_, and desugar it as follows:

1. `ident(exp)` ~> `(ident (exp))`
1. `ident(exp_1, exp_2, ..., exp_n)` ~> `(ident (exp_1) (exp_2) ... (exp_n))`

With this you can simplify the example from above to:

```repl
>>> Process.shell("pwd").chdir("/tmp").read
"/tmp\n"
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

# Terminal I/O

- `IO.Handle.tty?` can be used to detect if a file handle is connected to a
  terminal.

- The [`Solid.Ansi`][ansi] module provides operations that can be used to
  produce color output.

**Example:**

```haskell
retro :: String -> IO ()
retro output = stdout.print output.ansi.bold.green.on_black
```


```haskell
info :: String -> IO ()
info message = do
  when -< stderr.tty? $ do
    stderr.writeLine message
```

```haskell
reportError :: String -> IO ()
reportError message = do
  stderr.tty? >>= \ case
    False -> do
      stderr.writeLine "error: {message}"
    True -> do
      stderr.writeLine "error: {message.ansi.red}"
```

> __Note:__
>
> The `-<` operator is an overloaded version of `=<<` that can be used with
> actions of arity greater one.
>
> **Example:**
>
> ```haskell
> file :: IO FilePath
> file = undefined
>
> mode :: IO IO.Mode
> mode = undefined
>
> action :: FilePath -> IO.Mode -> IO ()
> action = undefined
>
> main :: IO ()
> main = action -< file -< mode
> ```

# Subprocess management

The [`Process`][process] module allows you to spawn new processes, connect to
their input/output/error streams, and obtain their exit status.

## Running processes

`Process.run` can be used to run a process.

**Example:**

```repl
>>> :type Process.run
Process.run :: Process.Config stdin stdout stderr -> IO ()

>>> :type Process.shell
Process.shell :: String -> Process.Config () () ()

>>> :type Process.command
Process.command :: FilePath -> [String] -> Process.Config () () ()
```

```repl
Run a shell command:
>>> Process.shell("echo Hey there 👋").run
Hey there 👋

Run an executable:
>>> Process.command("echo", ["Hey there 👋"]).run
Hey there 👋
```

**Note:** You can use the `IsString` instance of `Process.Config` to run a
shell command.

**Example:**

```repl
Run a shell command:
>>> Process.run "echo Hey there 👋"
Hey there 👋
```

## Interacting with processes

`Process.run` is a specialization that is suitable for simple cases.  It is
defined in terms of `Process.with` and `Process.checkStatus` (both of which
will be discussed in later sections):

```haskell
run :: Process.Config stdin stdout stderr -> IO ()
run config = Process.with config Process.checkStatus
```
or
```haskell
run config = config.with $ \ process -> process.checkStatus
```
or
```haskell
run config = config.with (.checkStatus)
```

To use `Process.run`, you construct a process config and pass it in.  The
previous section showed examples of this.

In the general case, you are going to construct a process config, spawn a
process, access properties of the process instance and wait for process
completion.

1. Construct a process config
1. Spawn a process
1. Access properties of the process instance and wait for process completion

### Spawning processes

`Process.spawn` can be used to spawn a process.

**Example:**

```repl
>>> Process.spawn "echo Hey there 👋" >>= Process.wait
Hey there 👋
```

`Process.with` is an exception-safe abstraction that combines `Process.spawn`,
`Process.wait` and `Process.terminate`.

```haskell
with :: Process.Config stdin stdout stderr -> (Process.Process stdin stdout stderr -> IO a) -> IO a
with config action = bracket config.spawn Process.terminate $ \ process -> do
  action process <* process.wait
```

**Example:**

```repl
>>> Process.with "echo Hey there 👋" Process.wait
Hey there 👋
```

### Standard streams, environment, working directory

The [`Process.Config`][process-config] module provides operations that can be
used to construct a process config.

- `stdin`/`stdout`/`stderr` can be used to connect to the standard streams of a
  spawned process
- `environment` can be used to set the environment for a spawned process
- `chdir` can be used to set the working directory of a spawned process

**Example:**

```repl
Provide stdin as a string and capture stdout:
>>> let config = Process.command "cat" []
>>> config.stdin.set("foo").stdout.capture.with Process.stdout
"foo"
```

```repl
Create a pipe for stdin, capture stdout, redirect stderr to stdout:
>>> let config = Process.shell "tee /dev/stderr"
>>> :{
config.stdin.createPipe.stdout.capture.stderr.toStdout.with $ \ process -> do
  process.stdin.write "foo-"
  process.stdin.close
  process.stdout
:}
"foo-foo-"
```

```repl
Set the process environment:
>>> let config = Process.command "env" []
>>> config.environment([("FOO", "23")]).run
FOO=23
```

```repl
Set the working directory:
>>> let config = Process.command "pwd" []
>>> config.chdir("/tmp").run
/tmp
```

### Exit status

- `Process.status` waits for process completion and retrieves the exit status.
- `Process.wait` waits for process completion and throws an exception on a
  non-zero exit status.

**Example:**

```repl
Retrieve the exit status:
>>> Process.spawn "exit 23" >>= Process.status
ExitFailure 23
```
```repl
Throw an exception on a non-zero exit status:
>>> Process.spawn "exit 23" >>= Process.wait
*** Exception: ExitStatusException {status = 23, command = ShellCommand "exit 23"}
```

> __Note:__
>
>
> `Process.wait` only throws if you haven't retrieved the exit status with
> `Process.status` on that process instance already.  This allows for
> `Process.with`, which uses `Process.wait`, to be used with `Process.status`.
>
> To make this more clear, consider:
>
> ```haskell repl ignore
> -- If you retrieved the exit status with Process.status on the process instance
> -- then Process.wait (and by extension Process.with) will not throw:
> >>> Process.with "exit 23" Process.status
> ExitFailure 23
> ```
> ```haskell repl ignore
> -- On the other hand, if you didn't retrive the exit status already then
> -- Process.wait (and by extension Process.with) will throw an exception on a
> -- non-zero exit status:
> >>> Process.with "exit 23" $ \ _ -> pass
> *** Exception: ExitStatusException {status = 23, command = ShellCommand "exit 23"}
> ```
> If you want to unconditionally throw on a non-zero exit status then use
> `Process.checkStatus` instead of `Process.wait`.

# Extending Solid

## Extending Solid with Haskell

[`Haskell`][foreign-haskell] provides functions that are useful
when you want to use existing Haskell code from Solid.

**Example:**

```haskell top
import Data.Text qualified as Text
import Haskell qualified as Haskell
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

## Revised import syntax

Solid encourages the use of qualified imports.  For that reason Solid
eventually intends to desugar import statements as follows:

1. `import Foo` ~> `import Foo qualified`
1. `import Foo as Bar` ~> `import Foo qualified as Bar`
1. `import Foo (Foo)` ~> `import qualified Foo (Foo)`
1. `import (foo, bar) from Foo` ~> `import Foo (foo, bar)`
1. `import (..) from Foo` ~> `import Foo (..)`


[ansi]: ../src/Solid/Ansi.hs
[string]: ../src/String.hs
[byte-string]: ../src/ByteString.hs
[bytes]:../src/Solid/Bytes.hs
[either]: ../src/Either.hs
[process]: ../src/Process.hs
[process-config]: ../src/Process/Config.hs
[foreign-c]: ../src/Solid/Foreign/C.hs
[foreign-haskell]: ../src/Haskell.hs

[overloaded_record_dot]: https://downloads.haskell.org/ghc/9.6.2/docs/users_guide/exts/overloaded_record_dot.html
[haskell-pre-processor]: https://downloads.haskell.org/ghc/9.6.2/docs/users_guide/phases.html#options-affecting-a-haskell-pre-processor
