module Skellect.AppState (AppState(..)
                         ,startState
                         ,selection
                         ,sortedMatches
                         ,newQuery
                         ,Command(..)
                         ,run
                         ,before, after
                         ,prompt, query, windowSize
                         ) where

import qualified Skellect.ListZipper as LZ (ListZipper
                                           ,fromList
                                           ,size
                                           ,selection, before, after
                                           ,next, prev
                                           )
import Skellect.Score (score)

import Data.List (sortBy)
import Data.Ord (comparing)

data AppState = AppState {appData :: AppData
                         ,matches :: LZ.ListZipper String
                         ,matchesLength :: Int
                         ,query :: String
                         }

data AppData = AppData {appDataPrompt  ::  String
                       ,appDataRenderSize :: Int
                       ,choices :: [String]
                       }

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
run MoveUp   as = as { matches = LZ.prev $ matches as }
run MoveDown as = as { matches = LZ.next $ matches as }

prompt :: AppState -> String
prompt as = promptNumber as ++ (appDataPrompt . appData) as

promptNumber :: AppState -> String
promptNumber as = padToLength m (show $ matchesLength as)
    where m = length . show . length . choices . appData $ as

padToLength :: Int -> String -> String
padToLength = padTolengthWithChar ' '

padTolengthWithChar :: Char -> Int -> String -> String
padTolengthWithChar c i s = replicate (i - length s) c ++ s

windowSize :: AppState -> Int
windowSize = appDataRenderSize . appData

startState :: [String] -> AppState
startState cs = startStateWithPrompt defaultPrompt (min maxHeight (length cs)) cs

defaultPrompt :: String
defaultPrompt = ">"

maxHeight :: Int
maxHeight = 20

startStateWithPrompt :: String -> Int -> [String] -> AppState
startStateWithPrompt p ws cs = AppState ad zipper len ""
    where
        ad = AppData p ws cs
        zipper = LZ.fromList $ take ws cs
        len = length cs

before, after :: AppState -> [String]
before = LZ.before . matches
after  = LZ.after . matches

selection :: AppState -> Maybe String
selection = LZ.selection . matches

appStateWithQuery :: AppData -> String -> AppState
appStateWithQuery ad@(AppData _ ws cs) q =
    AppState ad (LZ.fromList $ take ws sms) len q
        where
            sms = sortedMatches cs q
            len = length sms

newQuery :: AppState -> String -> AppState
newQuery = appStateWithQuery . appData

sortedMatches :: [String] -> String -> [String]
sortedMatches cs q = map fst sortedDescending
    where
        sortedDescending = sortBy (flip (comparing snd)) positives
        positives = filter ((>0) . snd) choiceScores
        choiceScores = zip cs $ map (score q) cs
