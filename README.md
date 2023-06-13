# nothing to see here move along
...

...

...

# Solid: A functional scripting environment

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

# Haskell Language Server (LSP) support

It is possible to use [Haskell Language Server](https://github.com/haskell/haskell-language-server)
to get LSP diagnostics while developing a script.  This requires the following
configuration:

```yaml
# hie.yaml
cradle:
  bios:
    shell: "solid ghc-options $HIE_BIOS_ARG > $HIE_BIOS_OUTPUT"
```

# GHC

`solid` uses `stack` to install GHC `9.6.1`.

# Limitations

- If a `.` is used right after a desugared expression then the column in error
  messages might be inaccurate.  This is currently not easy to fix due to
  [GHC #23040](https://gitlab.haskell.org/ghc/ghc/-/issues/23040)
- The `where` in `module ... where` has to be followed by a newline, e.g. this
  is not supported:

  ```haskell
  module Foo where bar :: Int
                   bar = 23
  ```
- Some modules are imported implicitly.  As of now, explicit imports do not
  prevent implicit imports of the same name.

  Example:

  ```haskell
  import Data.Text qualified as String

  foo = String.length
  ```

  will be desugared to

  ```haskell
  import String qualified
  import Data.Text qualified as String

  foo = String.length
  ```
  resultin in `String.length` being ambiguous.

  This will be addressed eventually.
