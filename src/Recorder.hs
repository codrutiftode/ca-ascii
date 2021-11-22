module Recorder (cls, resetScreen) where

import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import System.IO

-- The custom cellular automaton data structure
import CellAutomaton (CA, CARules, initCA, updateCA, containerBox)

-- Projects CA to a 2d grid
import GridRenderer (caToGrid, Grid, GridPoint, ViewportOptions, prepareRenderCA)

-- Used in the depth calulation to compute pixel brightness
colorRange :: (Int, Int)
colorRange = (0, 150)

-- Resets the colors
resetScreen :: String
resetScreen = "\ESC[0m"

-- Clears the screen
cls :: String
cls = "\ESC[2J"

-- Moves the cursor to the (0, 0) position
move0 :: String
move0 = "\ESC[H"


-- Computes a map for coloring different depths differently
calcDepthRange :: Grid -> (Int, Int)
calcDepthRange grid = (minimum depthList, maximum depthList)
    where depthList = map snd (concat grid)


-- Adds color information for one point
colorChar :: (Int, Int) -> GridPoint -> String
colorChar (minDepth, maxDepth) (ch, depth) = prefix ++ [ch]
    where
    -- Calc a color point by projecting the depth range onto the color range and seeing where the current depth lands
    colorPoint :: Int
    colorPoint = round ((fromIntegral depth) / depthLen * colorLen) + (255 - snd colorRange)
        where depthLen = fromIntegral (maxDepth - minDepth)
              colorLen = fromIntegral (snd colorRange - fst colorRange)
    r,g,b :: Int
    r = 0
    g = colorPoint
    b = 0 
    prefix = printf "\ESC[38;2;%d;%d;%dm" r g b


-- Adds color information to the grid
colorGrid :: Grid -> [String]
colorGrid grid = [concatStr item | item <- gridWithColor]
    where
    concatStr :: [String] -> String
    concatStr = foldr (++) ""
    depthRange = calcDepthRange grid
    gridWithColor :: [[String]]
    gridWithColor = [map (colorChar depthRange) row | row <- grid]

-- Prints a list of lines as one string
linesToStr :: [String] -> String
linesToStr = foldr (++) "" . map (++ "\n")

-- Takes a CA, renders it, then repeats
recordCA :: Handle -> Int -> Int -> CARules -> ViewportOptions -> CA -> IO ()
recordCA handle noFrames caSize caRules vOptions ca
    | snd ca == noFrames = return ()
    | otherwise = do
        let preparedCA = prepareRenderCA ca (containerBox caSize)
        let stage = snd ca
        putStrLn (printf "Recording frame #%d/%d..." (stage + 1) noFrames)

        let frame = move0 ++ linesToStr (colorGrid (caToGrid caSize vOptions preparedCA))
        hPutStrLn handle (show (length frame))
        hPutStr handle frame
        recordCA handle noFrames caSize caRules vOptions (updateCA caSize caRules ca)


-- Records an animation for given parameters
record :: String -> Int -> CARules -> ViewportOptions -> Int -> IO ()
record filepath caSize (neighStayAlive, neighBecomeAlive) (viewportAngle, zRotSpeed) noFrames = 
    do
    putStrLn (printf "Preparing to record %d frames to '%s'." noFrames filepath)
    withFile filepath WriteMode (\handle -> do
        hPutStrLn handle (show caSize) 
        hPutStrLn handle (show neighStayAlive)
        hPutStrLn handle (show neighBecomeAlive)
        hPutStrLn handle (show viewportAngle)
        hPutStrLn handle (show zRotSpeed)
        hPutStrLn handle (show noFrames)
        recordCA handle noFrames caSize (neighStayAlive, neighBecomeAlive) (viewportAngle, zRotSpeed) initCA)

