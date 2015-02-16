module ScoreSpec (spec) where

import Skellect.Score (matchLength
                      ,score
                      ,shortestMatchLength
                      ,suffixesStartingWith)
import Test.Hspec (describe, it, shouldBe, shouldSatisfy, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))
import Data.List (genericLength, (\\))

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "score" $ do
        it "returns 1 for empty queries" $
            score "" "some string" `shouldBe` 1
        it "returns 0 for empty choices" $
            score "a query" "" `shouldBe` 0
        it "returns 0 for a non-matching string" $
            score "haha" "avocado" `shouldBe` 0
        it "returns 0 for a partial match" $
            score "ab" "and" `shouldBe` 0
        it "ignores case for query" $
            score "aA" "aa" `shouldBe` score "aa" "aa"
        it "ignores case for choice" $
            score "aa" "Aa" `shouldBe` score "aa" "aa"
        it "scores above 0 for correct matches" $
            [score "a" "a"
            ,score "a" "aa"
            ,score "ba" "banana"
            ,score "ubs" "/usr/bin/selecta"
            ] `shouldSatisfy` all (>0)
        prop "scores as high as possible when query = choice" $
            \x -> not (null x) ==>
                score x x `shouldBe` 1 / genericLength x
        it "scores better matches higher" $
            score "asp" "asp.." `shouldSatisfy`
                (> score "asp" "a.s.p")
        it "scores shorter matches higher" $
            score "select" "select..." `shouldSatisfy`
                (> score "select" "select......")
    describe "shortestMatchLength" $ do
        prop "returns Just 0 on empty queries" $
            \x -> shortestMatchLength "" x `shouldBe` Just 0
        it "returns Just 2 for \"aa\" and \"aardvark\"" $
            shortestMatchLength "aa" "aardvark" `shouldBe` Just 2
        it "returns Nothing if there isn't a match" $
            shortestMatchLength "ab" "anathema" `shouldBe` Nothing
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

