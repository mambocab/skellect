module UtilSpec (spec) where

import Skellect.Utils (nonEmptyLines)
import Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec =
    describe "nonEmptyLines" $ do
        it "splits great-newline-pancakes into [\"great\",\"pancakes\"]" $
            nonEmptyLines "great\npancakes" `shouldBe` ["great","pancakes"]
        it "ignores newlines at the beginning of the string" $
            nonEmptyLines "\noh\nno!" `shouldBe` ["oh","no!"]
        it "ignores newlines at the end of the string" $
            nonEmptyLines "just\nfine\n" `shouldBe` ["just","fine"]
        it "ignores the empty string between two newlines" $
            nonEmptyLines "what\n\nnow?" `shouldBe` ["what","now?"]

