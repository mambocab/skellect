module Skellect.Utils (nonEmptyLines) where

nonEmptyLines :: String -> [String]
nonEmptyLines = filter (not . null) . lines 

