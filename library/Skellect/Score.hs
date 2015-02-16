module Skellect.Score (matchLength
                      ,score
                      ,shortestMatchLength
                      ,suffixesStartingWith) where
import Skellect.Utils (lower)
import Data.List (genericLength, tails)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<$>))

score :: String -> String -> Rational
score ""    _      = 1
score _     ""     = 0
score query choice =
    let match = shortestMatchLength (lower query) (lower choice)
    in case match of
        Nothing -> 0
        Just x  ->
            genericLength query / fromIntegral x / genericLength choice

shortestMatchLength :: Eq a => [a] -> [a] -> Maybe Integer
shortestMatchLength []          _      = Just 0
shortestMatchLength query@(q:_) choice =
    let matchQuery = matchLength query
        candidates = suffixesStartingWith q choice
        lengths = mapMaybe matchQuery candidates
    in case lengths of
        [] -> Nothing
        xs -> Just (minimum xs)

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

