module UtilSpec (spec) where

import Skellect.Utils (headMay, nonEmptyLines)
import Test.Hspec (describe, it, shouldBe, Spec)
import Test.QuickCheck (property)

validLineSplits :: String -> [String] -> Bool
validLineSplits "" [] = True
validLineSplits ('\n':xs) ys = validLineSplits xs ys
validLineSplits (x:xs) (y:ys)
    | x == head y = validLineSplits xs (remainingSplit (tail y: ys))
    | otherwise   = False
        where
            remainingSplit ("":as) = as
            remainingSplit as = as
validLineSplits _  [] = False
validLineSplits "" _  = False

spec :: Spec
spec = do
    describe "nonEmptyLines" $ do
        it "splits great-newline-pancakes into [\"great\",\"pancakes\"]" $
            nonEmptyLines "great\npancakes" `shouldBe` ["great","pancakes"]
        it "ignores newlines at the beginning of the string" $
            nonEmptyLines "\noh\nno!" `shouldBe` ["oh","no!"]
        it "ignores newlines at the end of the string" $
            nonEmptyLines "just\nfine\n" `shouldBe` ["just","fine"]
        it "ignores the empty string between two newlines" $
            nonEmptyLines "what\n\nnow?" `shouldBe` ["what","now?"]
        it "doesn't let empty strings in its output" $ property $
            \s -> "" `notElem` nonEmptyLines s
        it "always provides a valid split" $ property $
            \s -> validLineSplits s (nonEmptyLines s)

    describe "headMay" $ do
        it "returns Nothing for empty lists" $
            headMay [] `shouldBe` (Nothing :: Maybe Char)
        it "returns Just the first element of a list" $
            headMay [5, 2, 9] `shouldBe` (Just 5 :: Maybe Integer)
        it "works on infinite lists" $
            headMay [4..] `shouldBe` (Just 4 :: Maybe Integer)
        it "works on singleton lists" $ property $
            \x -> headMay [x] == (Just x :: Maybe Integer)
        it "works on arbitrary lists" $ property $
            \x xs -> headMay (x:xs) == (Just x :: Maybe Float)

