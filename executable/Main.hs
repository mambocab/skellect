module Main (main) where

import Skellect.AppState

import Prelude hiding (lookup)
import System.Console.ANSI
import System.IO
import Data.Map.Strict (findWithDefault, fromList)
import System.Exit
import Control.Monad (replicateM_)

main :: IO ()
main = do
    choicesInput <- getContents
    let choicesLines = lines choicesInput
    let initState = startState choicesLines
    let viewHeight = 1 + windowSize initState
    tty <- openFile "/dev/tty" ReadWriteMode
    hSetBuffering tty NoBuffering
    hPutStr tty $ replicate viewHeight '\n'
    hCursorUp tty viewHeight
    loop tty viewHeight initState

commandMap = fromList [('\^N', MoveDown)
                      ,('\^P', MoveUp)
                      ,('\^H', DeleteChar)
                      ,('\^W', DeleteWord)
                      ,('\^U', DeleteQuery)
                      ]

charToCommand c = findWithDefault (AppendChar c) c commandMap

loop :: Handle -> Int -> AppState -> IO ()
loop tty viewHeight state = do
    hPutSelectionState tty state viewHeight
    nextChar <- hGetChar tty
    hClearLines tty viewHeight
    case nextChar of
        '\^C' -> endLoopAndPut Nothing
        '\n'  -> endLoopAndPut $ selection state
        _     -> loop tty viewHeight $ run (charToCommand nextChar) state

endLoopAndPut :: Maybe String -> IO ()
endLoopAndPut Nothing  = exitFailure
endLoopAndPut (Just s) = putStrLn s >> exitSuccess

hClearLines :: Handle -> Int -> IO ()
hClearLines tty height = do
    hClearLine tty
    replicateM_ height (hCursorDown tty 1 >> hClearLine tty)
    hCursorUp tty height

hPutSelectionState :: Handle -> AppState -> Int -> IO ()
hPutSelectionState tty state viewHeight = do
    hClearLines tty viewHeight
    let b = before state
    let a = after state
    hSetCursorColumn tty 0
    hPutStrLn tty $ prompt state ++ " " ++ query state
    hPutStr tty $ unlines b
    hPutSelection tty state
    hPutStr tty $ unlines a

    let linesPrinted = sum [2, length b, length a]
    hCursorUp tty linesPrinted
    hCursorForward tty $ promptLineLength state

promptLineLength :: AppState -> Int
promptLineLength state = (+1) $ sum $ map length [prompt state, query state]

hPutSelection :: Handle -> AppState -> IO ()
hPutSelection tty as = case selection as of
    Nothing -> hPutStrLn tty ""
    Just s  -> do
        hSetSGR tty [SetSwapForegroundBackground True]
        hPutStrLn tty s
        hSetSGR tty [SetSwapForegroundBackground False]
