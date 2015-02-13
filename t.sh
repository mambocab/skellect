#!/usr/bin/env bash
set -e # log each command before execution
set -x # fail fast

cabal build >/dev/null # build, showing errors but not regular output
cabal exec -- runhaskell test-suite/HLint.hs # lint
cabal exec -- runhaskell -ilibrary -itest-suite test-suite/Spec.hs # test!

