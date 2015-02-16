module ScoreSpec (spec) where

import Skellect.Score (matchLength, suffixesStartingWith)
import Test.Hspec (describe, it, shouldBe, shouldSatisfy, Spec)
import Test.Hspec.QuickCheck (prop)
import Data.List ((\\))

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "matchLength" $ do
        prop "returns 0 if the query is empty" $
            \x -> matchLength "" x `shouldBe` Just 0
        it "returns 1 if the query has length 1 and is in the choice" $
            matchLength "a" "and" `shouldBe` Just 1
        it "finds a match of length 3 for \"ad\" in \"and\"" $
            matchLength "ad" "and" `shouldBe` Just 3
        it "returns Nothing when the choice is empty" $
            matchLength "test" "" `shouldBe` Nothing
        it "returns Nothing when there's no match" $
            matchLength "foo" "bar" `shouldBe` Nothing
        it "returns Nothing for a partial match" $
            matchLength "and" "annie" `shouldBe` Nothing
    describe "suffixesStartingWith" $ do
        it "returns an empty list for an empty choice" $
            suffixesStartingWith 'a' "" `shouldBe` []
        it "returns an empty list if the character is not in the choice" $
            suffixesStartingWith 'a' "bcde" `shouldSatisfy` null
        it "returns the right strings for 'a' in  \"the aardvark\"" $
            suffixesStartingWith 'a' "the aardvark" `shouldSatisfy`
                (null . (\\["aardvark", "ardvark", "ark"]))
        prop "doesn't return empty strings in the output list" $
            \x xs -> "" `notElem` suffixesStartingWith x xs
        prop "returns a list of strings starting with the specified char" $
            \x xs -> all ((==(x :: Char)) . head) $
                suffixesStartingWith x xs
        prop "returns the right number of strings" $
            \x xs -> (length . suffixesStartingWith (x::Char) $ xs)
                == (length . filter (==x) $ xs)

