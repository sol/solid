# nothing to see here move along
...

...

...

# Solid: A modern re-imagination of Haskell

```
git clone git@github.com:sol/solid.git
cd solid
cabal install
```

```haskell
#!/usr/bin/env solid

name :: String
name = "Joe"

main :: IO ()
main = do
  stdout.writeLine "Hey {name} 👋"
```

```
$ solid main.hs
Hey Joe 👋
```
or
```
$ chmod +x main.hs
$ ./main.hs
Hey Joe 👋
```

# Tooling

`solid` uses `stack` to install GHC `9.6.2` to a private location.  It does not
use any system provided GHC.

As of now `solid` needs a recent version of `stack` to be available on the
`PATH`.  This requirement will be lifted eventually.

`solid` uses the global cabal store (e.g.
`.local/state/cabal/store/ghc-9.6.2/`) for caching.  This reduces build times
and is usually safe.  However, in the unlikely case that you use a different
build of GHC `9.6.1` that is not ABI compatible to what `stack` provides you
may run into issues.

## Haskell Language Server (LSP) support

It is possible to use [Haskell Language Server](https://github.com/haskell/haskell-language-server)
to get LSP diagnostics while developing a script.  This requires the following
configuration file next to your script:

```yaml
# hie.yaml
cradle:
  bios:
    shell: "solid ghc-options $HIE_BIOS_ARG > $HIE_BIOS_OUTPUT"
```


## Using third-party tools

You can use `solid` with third-party tools that expect GHC options.

Examples:

```bash
# start a GHCi session
$ solid with ghci main.hs
```

```bash
# run doctest
$ solid with doctest main.hs
```

```bash
# run sensei
$ solid with sensei main.hs
```

`solid with` does two things:

1. It puts a suitable version of GHC on the `PATH`
1. It calls the specified executable with all required GHC options

## Running `doctest`

`solid with` can be used to run `doctest`.  However, this only works if a
suitable version of `doctest` is available on the `PATH`.  `solid doctest`
provides a more robust way to run `doctest`.

Example:

```bash
$ solid doctest main.hs
```

## Specifying additional dependencies

As of now, `solid` does not provide a mechanism to manage third-party
dependencies.  This limitation will be lifted eventually.

However, `solid` accepts arbitrary GHC options, including `-package`, and
`-package` can be used to specify arbitrary additional Haskell dependencies.

Example:

```bash
$ solid with ghci -package=hspec -package=QuickCheck test/FormatSpec.hs
```

Note that for this to work, a version of the requested package has to be
present in the cabal store.  If a package is not yet in the cabal store, then
you have to populate the cabal store manually, e.g. with `cabal repl
--build-depends`.

Example:

```bash
$ echo | cabal repl --build-depends hspec,QuickCheck
```

# Limitations

- If a `.` is used right after a desugared expression then the column in error
  messages might be inaccurate.  This is currently not easy to fix due to
  [GHC #23040](https://gitlab.haskell.org/ghc/ghc/-/issues/23040)
- The `where` in `module ... where` has to be followed by a newline, e.g. this
  is not supported:

  ```haskell ignore
  module Foo where bar :: Int
                   bar = 23
  ```
- Some modules are imported implicitly.  As of now, explicit imports do not
  prevent implicit imports of the same name.

  Example:

  ```haskell ignore
  import Data.Text qualified as String

  foo = String.length
  ```

  will be desugared to

  ```haskell ignore
  import String qualified
  import Data.Text qualified as String

  foo = String.length
  ```
  resultin in `String.length` being ambiguous.

  This will be addressed eventually.

# Differences from Haskell

- The `IsString` instance of `ByteString` encodes the input string as UTF-8
  (instead of truncating to octets)
