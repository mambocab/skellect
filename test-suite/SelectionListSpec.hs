module SelectionListSpec (spec) where

import Skellect.SelectionList (SelectionList(..)
                              ,fromList
                              ,next
                              ,prev)
import Test.Hspec (describe, it, shouldBe, Spec)

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "fromList" $ do
        it "returns EmptySelectionList for []" $
            fromList ([] :: String) `shouldBe` EmptySelectionList
        it "gives an element surrounded by [] and [] for (x:[])" $
            fromList ["foo"] `shouldBe` SelectionList [] "foo" []
        it "returns a selection with the first element selected" $
            fromList ["foo","bar","baz"]
                `shouldBe` SelectionList [] "foo" ["bar","baz"]
    describe "next" $ do
        it "returns an EmptySelectionList for an EmptySelectionList" $
            next (EmptySelectionList :: SelectionList String)
                `shouldBe` EmptySelectionList
        it "doesn't advance past the end of a singleton selection" $
            next (SelectionList [] "foo" [])
                `shouldBe` SelectionList [] "foo" []
        it "advances from the first to the second member of a list" $
            next (SelectionList [] "foo" ["bar"])
                `shouldBe` SelectionList ["foo"] "bar" []
        it "advances from an inner element to the next element" $
            next (SelectionList ["foo"] "bar" ["baz","quux"])
                `shouldBe` SelectionList ["foo","bar"] "baz" ["quux"]
        it "doesn't advance past the end of a longer list" $
            next (SelectionList ["foo","bar","baz"] "quux" [])
                `shouldBe` SelectionList ["foo","bar","baz"] "quux" []
    describe "prev" $ do
        it "returns an EmptySelectionList for an EmptySelectionList" $
            prev (EmptySelectionList :: SelectionList String)
                `shouldBe` EmptySelectionList
        it "doesn't move back past the beginning of a singleton" $
            prev (SelectionList [] "banana" [])
                `shouldBe` SelectionList [] "banana" []
        it "moves from the second to the first member of a list" $
            prev (SelectionList ["foo"] "bar" [])
                `shouldBe` SelectionList [] "foo" ["bar"]
        it "moves from an inner element to the previous element" $
            prev (SelectionList ["foo","bar"] "baz" ["quux"])
                `shouldBe` SelectionList ["foo"] "bar" ["baz","quux"]
        it "doesn't move before the beginning of a longer list" $
            prev (SelectionList [] "foo" ["bar","baz","quux"])
                `shouldBe` SelectionList [] "foo" ["bar","baz","quux"]
