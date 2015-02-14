GHCI=ghci-7.10.0.20141222
SHELL=bash

log: a.hs ../HListPlugin.hs
	$(GHCI) -dtrace-level=1 -ddump-tc-trace -package ghc -i../ a.hs <<< ':t p' &> log
	touch log
