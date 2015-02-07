module SkellectSpec (spec) where

import Skellect (skellect)
import Test.Hspec (describe, it, shouldBe, Spec)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
    describe "skellect" $ do
        it "returns unit" $ do
	    skellect `shouldBe` ()

	prop "is equal to unit" $
	    \x -> skellect == x

