target = README.lhs
readme = $(target:.lhs=.md)
ghc_options = $(shell grep 'solid ghc-options' hie.yaml | sed 's/.*solid ghc-options \(.*\) \$$HIE_BIOS_ARG .*/\1/')
options = $(ghc_options) $(target)
repl_options = $(options) -Wno-overlapping-patterns

doctest:
	solid with ./doctest.hs $(repl_options)
	@echo "Expected: $$(grep '>>>' $(target) | wc -l)"
toc:
	@./gh-md-toc --insert --no-backup $(readme)
	sed -i 's/.*#solid-for-haskell-programmers.*//' $(readme)
	sed -i 's/<!-- Added by: .* -->//' $(readme)
sensei:
	solid with sensei $(repl_options)
repl:
	solid with ghci $(repl_options)
