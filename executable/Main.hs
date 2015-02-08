module Main (main) where

import Skellect.Utils (nonEmptyLines)

main :: IO ()
main = do
    choicesInput <- getContents
    putStrLn $ unlines $ nonEmptyLines choicesInput

