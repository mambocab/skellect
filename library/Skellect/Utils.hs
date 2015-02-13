module Skellect.Utils (headMay, nonEmptyLines) where

headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

nonEmptyLines :: String -> [String]
nonEmptyLines = filter (not . null) . lines
