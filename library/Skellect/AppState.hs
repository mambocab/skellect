module Skellect.AppState (AppState(..)
                         ,startState
                         ,selection
                         ,sortedMatches
                         ,newQuery
                         ,Command(..)
                         ,run
                         ,query) where

import qualified Skellect.ListZipper as LZ (ListZipper
                                           ,fromList
                                           ,selection
                                           ,next, prev)
import Skellect.Score (score)

import Data.List (sortBy)
import Data.Ord (comparing)

data AppState = AppState AppData (LZ.ListZipper String) String

data AppData = AppData {prompt  ::  String
                       ,choices :: [String]}

data Command = AppendChar Char
             | DeleteChar | DeleteWord | DeleteQuery
             | MoveUp | MoveDown

run :: Command -> AppState -> AppState
run (AppendChar c) as = newQuery as $ query as ++ [c]
run DeleteChar as = newQuery as $ init . query $ as
run DeleteWord as = newQuery as $ (unwords . init . words . query) as
run DeleteQuery as = newQuery as ""
run MoveUp   (AppState ad ms q) = AppState ad (LZ.prev ms) q
run MoveDown (AppState ad ms q) = AppState ad (LZ.next ms) q

matches :: AppState -> LZ.ListZipper String
matches (AppState _ m _) = m

query :: AppState -> String
query (AppState _ _ q) = q

startState :: [String] -> AppState
startState = startStateWithPrompt defaultPrompt

defaultPrompt :: String
defaultPrompt = ">"

startStateWithPrompt :: String -> [String] -> AppState
startStateWithPrompt p cs = AppState (AppData p cs) (LZ.fromList cs) ""

selection :: AppState -> Maybe String
selection = LZ.selection . matches

appStateWithQuery :: AppData -> String -> AppState
appStateWithQuery ad@(AppData _ cs) q =
    AppState ad (LZ.fromList $ sortedMatches cs q) q

newQuery :: AppState -> String -> AppState
newQuery (AppState ad _ _) =
    appStateWithQuery ad

sortedMatches :: [String] -> String -> [String]
sortedMatches cs q = map fst sortedDescending
    where
        sortedDescending = sortBy (flip (comparing snd)) positives
        positives = filter ((>0) . snd) choiceScores
        choiceScores = zip cs $ map (score q) cs
