set -x

cabal build && cabal exec -- runhaskell test-suite/HLint.hs && cabal exec -- runhaskell -ilibrary -itest-suite test-suite/Spec.hs

