target = README.lhs
readme = $(target:.lhs=.md)

doctest: .ghci
	solid with ./doctest.hs -ghci-script .ghci $(target)
	@echo "Expected: $$(grep '>>>' $(target) | wc -l)"
toc:
	@./gh-md-toc --insert --no-backup $(readme)
	sed -i 's/.*#solid-for-haskell-programmers.*//' $(readme)
	sed -i 's/<!-- Added by: .* -->//' $(readme)
sensei: .ghci
	solid with sensei $(target)
.ghci: hie.yaml
	@echo :set $(shell grep 'solid ghc-options' hie.yaml | sed 's/.*solid ghc-options \(.*\) \$$HIE_BIOS_ARG .*/\1/') > .ghci
