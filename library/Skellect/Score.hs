module Skellect.Score (matchLength, suffixesStartingWith) where

import Data.List (tails)
import Control.Applicative ((<$>))

matchLength :: Eq a => [a] -> [a] -> Maybe Integer
matchLength [] _ = Just 0
matchLength query@(q:qs) (c:cs) = (+1) <$> matchLength rq cs
    where rq = if q == c then qs else query
matchLength _ [] = Nothing

suffixesStartingWith :: Eq a => a -> [a] -> [[a]]
suffixesStartingWith c = filter cHead . tails
    where
        cHead []    = False
        cHead (x:_) = x == c

