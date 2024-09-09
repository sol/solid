ghc_old = 9.8.2
ghc_new = 9.8.2

.PHONY: *

all:
	@echo run update-ghc target

update-ghc:
	make create-patch
	make copy-new
	make constraints
	git add constraints
	git commit -m 'Copy vendored files from from GHC $(ghc_new)'
	git stash apply && git stash drop
	make commit

commit:
	git commit -a -m 'Depend on GHC $(ghc_new)'

constraints:
	@echo -ne "constraints:\n  , " > constraints
	@ghc-pkg-$(ghc_new) list --global --simple-output | sed 's/ /\n  , /g' | sed 's/-\([0-9]\)/ == \1/g' >> constraints

create-patch:
	make copy-old
	git commit -m 'wip: create-patch'
	git revert HEAD --no-edit
	git reset HEAD^
	git stash
	git reset HEAD^ --hard

copy-new:
	make copy version=$(ghc_new)

copy-old:
	make copy version=$(ghc_old)

copy:
	cd ../ghc && git fetch origin && git checkout ghc-$(version)-release

	cat ../ghc/ghc/ghc-bin.cabal.in \
		| sed 's/@ProjectVersion@/$(version)/' \
		| sed 's/@ProjectVersionMunged@/$(version)/' \
		> vendor/ghc-bin/ghc-bin.cabal
	git add vendor/ghc-bin/ghc-bin.cabal

	git rm -r vendor/ghc-bin/GHCi/
	cp -r ../ghc/ghc/GHCi vendor/ghc-bin/GHCi
	git add vendor/ghc-bin/GHCi/

	cp ../ghc/compiler/GHC/Parser/Lexer.x solid-pp/vendor/Lexer.x
	git add solid-pp/vendor/Lexer.x
