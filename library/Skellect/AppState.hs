module Skellect.AppState (AppState(..)
                         ,startState
                         ,selection
                         ,sortedMatches
                         ,newQuery
                         ,Command(..)
                         ,run
                         ,before, after
                         ,prompt, query) where

import qualified Skellect.ListZipper as LZ (ListZipper
                                           ,fromList
                                           ,selection, before, after
                                           ,next, prev)
import Skellect.Score (score)

import Data.List (sortBy)
import Data.Ord (comparing)

data AppState = AppState {appData :: AppData
                         ,windowSize :: Int
                         ,matches :: LZ.ListZipper String
                         ,query :: String
                         }

data AppData = AppData {appDataPrompt  ::  String
                       ,choices :: [String]}

data Command = AppendChar Char
             | DeleteChar | DeleteWord | DeleteQuery
             | MoveUp | MoveDown

run :: Command -> AppState -> AppState
run (AppendChar c) as = newQuery as $ query as ++ [c]
run DeleteChar as = let q = query as in
    case q of
        "" -> as
        _  -> newQuery as $ init . query $ as
run DeleteWord as = let q = query as in
    case q of
        "" -> as
        _  -> newQuery as $ (unwords . init . words . query) as
run DeleteQuery as = newQuery as ""
run MoveUp   as@(AppState _ _ ms _) = as { matches = LZ.prev ms }
run MoveDown as@(AppState _ _ ms _) = as { matches = LZ.next ms }

prompt = appDataPrompt . appData

startState :: [String] -> AppState
startState cs = startStateWithPrompt defaultPrompt (min maxHeight (length cs)) cs

defaultPrompt :: String
defaultPrompt = ">"

maxHeight :: Int
maxHeight = 20

startStateWithPrompt :: String -> Int -> [String] -> AppState
startStateWithPrompt p ws cs = AppState (AppData p cs) ws (LZ.fromList $ take ws cs) ""

before, after :: AppState -> [String]
before = LZ.before . matches
after  = LZ.after . matches

selection :: AppState -> Maybe String
selection = LZ.selection . matches

appStateWithQuery :: AppData -> Int -> String -> AppState
appStateWithQuery ad@(AppData _ cs) ws q =
    AppState ad ws (LZ.fromList $ take ws $ sortedMatches cs q) q

newQuery :: AppState -> String -> AppState
newQuery (AppState ad ws _ _) =
    appStateWithQuery ad ws

sortedMatches :: [String] -> String -> [String]
sortedMatches cs q = map fst sortedDescending
    where
        sortedDescending = sortBy (flip (comparing snd)) positives
        positives = filter ((>0) . snd) choiceScores
        choiceScores = zip cs $ map (score q) cs
