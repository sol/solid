# nothing to see here move along
...

...

...

# Solid: A functional scripting environment

Solid is a modern remix of (GHC) Haskell with a focus on productivity and joy
for programmers.

```
git clone git@github.com:sol/solid.git
cd solid
cabal install
```

```haskell
#!/usr/bin/env solid

names :: [String]
names = ["Jane", "Joe"]

main :: IO ()
main = do
  name <- names.randomChoice
  stdout.writeLine "Hey {name} 👋"
```
```haskell ignore
-- or point-free
main :: IO ()
main = stdout.writeLine . "Hey {} 👋" =<< names.randomChoice
```

```
$ solid main.hs
Hey Jane 👋
```
or
```
$ chmod +x main.hs
$ ./main.hs
Hey Joe 👋
```

# An introduction to Solid for Haskell programmers

Read: [`book/README.md`](book/README.md)

# Tooling

`solid` installs GHC `9.10.1` to a private location.  It does not use any system
provided GHC.

`solid` caches packages in `~/.local/state/solid/store/`.  It does not use the
user-global cabal store.

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

You can use third-party tools that expect GHC options with `solid`.

**Example:**

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

**Example:**

```bash
$ solid doctest main.hs
```

## Literate programming

By default, Solid uses [`markdown-unlit`](https://github.com/sol/markdown-unlit) as the literate pre-processor.

To use the `unlit` executable that ships with GHC you have to specify `-pgmL`
explicitly, e.g.:

```bash
$ solid -pgmL=$(solid repl -e 'Process.read "$(solid ghc-path) --info" <&> ByteString.read! @[([Char], [Char])] <&> List.lookup! "unlit command" >>= print') main.lhs
```

## Specifying additional dependencies

As of now, `solid` does not provide a mechanism to manage third-party
dependencies.  This limitation will be lifted eventually.

However, `solid` accepts arbitrary GHC options, including `-package`, which can
be used to specify arbitrary additional Haskell dependencies.

**Example:**

```bash
$ solid with ghci -package=hspec -package=QuickCheck test/Spec.hs
```

Note that for this to work, a version of the requested package has to be
present in the cabal store.  If a package is not yet in the cabal store, then
you have to populate the cabal store manually, e.g. with `cabal repl
--build-depends`.

**Example:**

```bash
$ echo | cabal repl --build-depends hspec,QuickCheck
```

# Limitations

- The `where` in `module ... where` has to be followed by a newline, e.g. this
  is not supported:

  ```haskell ignore
  module Foo where bar :: Int
                   bar = 23
  ```
