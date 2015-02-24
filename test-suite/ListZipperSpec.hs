module ListZipperSpec (spec) where

import Skellect.ListZipper (ListZipper
                           ,fromList, fromLists, toList
                           ,empty ,before, selection, after
                           ,next, prev)
import Test.Hspec (describe, it, shouldBe, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "next and prev interaction" $ do
        prop "ensures (prev . next) lz == lz when next works" $
            \b s a -> let lz = (fromLists b s a :: ListZipper Int) in
                next lz /= lz ==> (prev . next) lz == lz
        prop "ensures (next . prev) lz == lz when prev works" $
            \b s a -> let lz = (fromLists b s a :: ListZipper Int) in
                prev lz /= lz ==> (next . prev) lz == lz
    describe "before" $ do
        prop "interacts correctly with fromLists" $
            \b s a -> before (fromLists b s a) `shouldBe` (b :: [Int])
    describe "selection" $ do
        prop "interacts correctly with fromLists" $
            \b s a -> selection (fromLists b s a)
                `shouldBe` Just (s :: Int)
    describe "after" $ do
        prop "interacts correctly with fromLists" $
            \b s a -> after (fromLists b s a) `shouldBe` (a :: [Int])
    describe "fromList" $ do
        it "returns EmptyListZipper for []" $
            fromList ([] :: String) `shouldBe` empty
        it "gives an element surrounded by [] and [] for (x:[])" $
            fromList ["foo"] `shouldBe` fromLists [] "foo" []
        it "returns a selection with the first element selected" $
            fromList ["foo","bar","baz"]
                `shouldBe` fromLists [] "foo" ["bar","baz"]
        prop "gives the correct selection" $
            \x xs -> selection (fromList $ x : xs)
                `shouldBe` Just (x :: Int)
        prop "round-trips correctly with toList" $
            \xs -> (xs :: [Int]) == (toList . fromList) xs
    describe "fromLists" $ do
        prop "gives the correct selection" $ do
            \b s a -> selection (fromLists b s a)
                `shouldBe` Just (s :: Int)
        prop "round-trips correctly with toList" $ do
            \b s a -> (b ++ (s:a) :: [Int]) == toList (fromLists b s a)
    describe "next" $ do
        it "returns an EmptyListZipper for an EmptyListZipper" $
            next (empty :: ListZipper Int)
                `shouldBe` empty
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
        prop "advances selection correctly" $
            \b s s' a -> selection (next $ fromLists b s (s':a))
                `shouldBe` Just (s' :: Int)
    describe "prev" $ do
        it "returns an EmptyListZipper for an EmptyListZipper" $
            prev (empty :: ListZipper String)
                `shouldBe` empty
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
        prop "changes selection correctly" $
            \b s' s a -> selection (prev $ fromLists (b ++ [s']) s a)
                `shouldBe` Just (s' :: Int)
