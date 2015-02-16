#!/usr/bin/env bash

# fail fast if any command dies with an error
set -e
# debug output: print each command before printing
set -x

# build, silencing normal output but allowing errors
cabal build >/dev/null
# lint
cabal exec -- runhaskell test-suite/HLint.hs
# test
cabal exec -- runhaskell -ilibrary -itest-suite test-suite/Spec.hs
