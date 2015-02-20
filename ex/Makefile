GHCI=ghci-7.10.0.20150219
# ghci-7.10.0.20141222
SHELL=bash

main: SameLength.out Coerce.out

%.out: %.hs ../HListPlugin.hs Makefile HList.hs
	$(GHCI) -dtrace-level=1 -ddump-tc-trace -package ghc -i../ $< <<< ':t q' &> $@
	touch $@

clean:
	rm SameLength.out Coerce.out
