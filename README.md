

# Limitations

- If a `.` is used right after a desugared expression then the column in error
  messages might be inaccurate.  This is currently not easy to fix due to
  [GHC #23040](https://gitlab.haskell.org/ghc/ghc/-/issues/23040)
