module Skellect.Utils (headMay, lower, nonEmptyLines) where

import Data.Char (toLower)

headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

nonEmptyLines :: String -> [String]
nonEmptyLines = filter (not . null) . lines

lower :: String -> String
lower = map toLower
