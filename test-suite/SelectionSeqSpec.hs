module SelectionSeqSpec (spec) where

import Skellect.SelectionSeq (SelectionSeq(..)
                             ,fromList
                             ,next
                             ,prev)
import Test.Hspec (describe, it, shouldBe, Spec)

import qualified Data.Sequence as S (fromList)
fromLists :: [a] -> a -> [a] -> SelectionSeq a
fromLists b s a = SelectionSeq (S.fromList b) s (S.fromList a)

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "fromList" $ do
        it "returns EmptySelectionSeq for []" $
            fromList ([] :: String) `shouldBe` EmptySelectionSeq
        it "gives an element surrounded by [] and [] for (x:[])" $
            fromList ["foo"] `shouldBe` fromLists [] "foo" []
        it "returns a selection with the first element selected" $
            fromList ["foo","bar","baz"]
                `shouldBe` fromLists [] "foo" ["bar","baz"]
    describe "next" $ do
        it "returns an EmptySelectionSeq for an EmptySelectionSeq" $
            next (EmptySelectionSeq :: SelectionSeq String)
                `shouldBe` EmptySelectionSeq
        it "doesn't advance past the end of a singleton selection" $
            next (fromLists [] "foo" [])
                `shouldBe` fromLists [] "foo" []
        it "advances from the first to the second member of a list" $
            next (fromLists [] "foo" ["bar"])
                `shouldBe` fromLists ["foo"] "bar" []
        it "advances from an inner element to the next element" $
            next (fromLists ["foo"] "bar" ["baz","quux"])
                `shouldBe` fromLists ["foo","bar"] "baz" ["quux"]
        it "doesn't advance past the end of a longer list" $
            next (fromLists ["foo","bar","baz"] "quux" [])
                `shouldBe` fromLists ["foo","bar","baz"] "quux" []
    describe "prev" $ do
        it "returns an EmptySelectionSeq for an EmptySelectionSeq" $
            prev (EmptySelectionSeq :: SelectionSeq String)
                `shouldBe` EmptySelectionSeq
        it "doesn't move back past the beginning of a singleton" $
            prev (fromLists [] "banana" [])
                `shouldBe` fromLists [] "banana" []
        it "moves from the second to the first member of a list" $
            prev (fromLists ["foo"] "bar" [])
                `shouldBe` fromLists [] "foo" ["bar"]
        it "moves from an inner element to the previous element" $
            prev (fromLists ["foo","bar"] "baz" ["quux"])
                `shouldBe` fromLists ["foo"] "bar" ["baz","quux"]
        it "doesn't move before the beginning of a longer list" $
            prev (fromLists [] "foo" ["bar","baz","quux"])
                `shouldBe` fromLists [] "foo" ["bar","baz","quux"]
