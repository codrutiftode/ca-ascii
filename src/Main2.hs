module Main2
where

import Control.Concurrent (threadDelay)
import Text.Printf (printf)

-- The custom cellular automaton data structure
import CellAutomaton (CA, initCA, updateCA)

-- Projects CA to a 2d grid
import GridRenderer (caToGrid, Grid, GridPoint)

cls :: String
cls = "\ESC[2J"

move0 :: String
move0 = "\ESC[H"

-- Adds color information for one point
colorChar :: Int -> GridPoint -> String
colorChar colorUnit (ch, depth) = prefix ++ [ch] 
    where
    r,g,b :: Int
    r = 0
    g = 100
    b = 100
    prefix, postfix :: String
    prefix = printf "\ESC[38;2;%d;%d;%dm" r g b
    postfix = "\ESC[0m"


-- Adds color information to the grid
colorGrid :: Grid -> [String]
colorGrid grid = [concatStr item | item <- gridWithColor]
    where
    concatStr :: [String] -> String
    concatStr = foldr (++) ""
    colorUnit = 255 `div` 1 -- TODO
    gridWithColor :: [[String]]
    gridWithColor = [map (colorChar colorUnit) row | row <- grid]

putLines :: [String] -> IO ()
putLines = putStrLn . foldr (++) "" . map (++ "\n")

-- Takes a CA, renders it, then repeats
mainLoop :: CA -> IO ()
mainLoop ca = do 
    putStrLn move0
    putLines (colorGrid (caToGrid ca))
    threadDelay 100000
    mainLoop (updateCA ca)

-- The entry point
main :: IO ()
main = do
    putStrLn "\ESC[25l"
    putStrLn cls
    mainLoop (initCA)
