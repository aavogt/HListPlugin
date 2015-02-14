#!/bin/bash
distFile="$(echo $(cabal sdist) | awk -F' ' '{print $NF}')"
cabal2nix "$distFile" > default.nix
nix-env -i all
