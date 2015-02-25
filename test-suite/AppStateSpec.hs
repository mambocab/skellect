module AppStateSpec (spec) where

import Skellect.AppState (AppState(..)
                         ,startState
                         ,selection
                         ,sortedMatches
                         ,newQuery
                         ,Command(..)
                         ,run
                         ,query)

import Test.Hspec (describe, it, shouldBe, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "startState" $ do
        prop "initializes with an empty query" $
            \cs -> query (startState cs) `shouldBe` ""
        prop "initializes with the first element as the selection" $
            \s cs -> selection (startState (s : cs)) `shouldBe` Just s
    describe "sortedMatches" $ do
        it "sorts the choices descending" $
            sortedMatches ["lots-of-files","business-papers"
                          ,"lol-worthy","a little odd"] "lol"
                `shouldBe` ["lol-worthy","lots-of-files"]
    describe "run" $ do
        it "adds a character correctly" $ do
            query (run (AppendChar 'x') $
                newQuery (startState []) "foo")
                `shouldBe` "foox"
        it "removes a character correctly" $ do
            query (run DeleteChar $
                newQuery (startState []) "foo")
                `shouldBe` "fo"
        it "removes a word correctly" $ do
            query (run DeleteWord $
                newQuery (startState []) "foo bar")
                `shouldBe` "foo"
        it "deletes the whole query correctly" $ do
            query (run DeleteQuery $
                newQuery (startState []) "foo bar")
                `shouldBe` ""

