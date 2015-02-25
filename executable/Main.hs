module Main (main) where

import Skellect.AppState

import System.Console.ANSI
import System.IO

maxHeight :: Int
maxHeight = 20

main :: IO ()
main = do
    choicesInput <- getContents
    let initState = startState $ lines choicesInput
    let viewHeight = min maxHeight $ length . lines $ choicesInput
    tty <- openFile "/dev/tty" ReadWriteMode
    hSetBuffering tty NoBuffering
    hPutStr tty $ replicate viewHeight '\n'
    hCursorUp tty viewHeight
    loop tty viewHeight initState

loop :: Handle -> Int -> AppState -> IO ()
loop tty viewHeight state = do
    nextChar <- hGetChar tty
    hPutSelectionState tty state
    hCursorUp tty $ viewHeight + 1
    let state' = state
    loop tty viewHeight state'

hPutSelectionState tty state = do
    hPutStrLn tty $ unlines . before $ state
    hPutSelection tty state
    hPutStr tty $ unlines . after $ state

hPutSelection :: Handle -> AppState -> IO ()
hPutSelection tty as = case selection as of
    Nothing -> hPutStr tty ""
    Just s  -> do
        hSetSGR tty [SetSwapForegroundBackground True]
        hPutStrLn tty s
        hSetSGR tty [SetSwapForegroundBackground False]
