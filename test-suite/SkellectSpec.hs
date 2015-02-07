module SkellectSpec (spec) where

import Skellect (skellect)
import Test.Hspec (describe, it, shouldBe, Spec)
import Test.QuickCheck (property)

spec :: Spec
spec =
    describe "skellect" $ do
        it "returns unit" $
	    skellect `shouldBe` ()

	it "is equal to unit" $ property $
	    \x -> skellect `shouldBe` x

