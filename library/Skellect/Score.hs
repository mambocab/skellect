module Skellect.Score (matchLength
                      ,shortestMatchLength
                      ,suffixesStartingWith) where

import Data.List (tails)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<$>))

shortestMatchLength :: Eq a => [a] -> [a] -> Maybe Integer
shortestMatchLength []          _      = Just 0
shortestMatchLength query@(q:_) choice =
    let matchQuery = matchLength query
        candidates = suffixesStartingWith q choice
        lengths = mapMaybe matchQuery candidates
    in if null lengths then Nothing else Just (minimum lengths)

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

