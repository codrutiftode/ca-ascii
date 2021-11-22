module Player where

import System.IO
import Control.Concurrent (threadDelay)
import Recorder (cls, resetScreen)
import Text.Printf (printf)

-- Returns the first line and the rest of the string
lineFromStr :: String -> (String, String)
lineFromStr = lineFromStr' ""
    where lineFromStr' :: String -> String -> (String, String)
          lineFromStr' prev (c:cs)
                | c == '\n' = (reverse prev, cs)
                | otherwise = lineFromStr' (c:prev) cs
                

-- Parses the list of frames from the file, received as a big string
parseFramesData :: String -> [String]
parseFramesData "" = []
parseFramesData blob = (take frameLen restBlob) : parseFramesData (drop frameLen restBlob)
    where separation = lineFromStr blob
          frameLen = read (fst separation)
          restBlob = snd separation


-- Lower level playing of frames with timing
playFrames :: String -> Int -> Int -> IO ()
playFrames framesBlob frameMS noFrames = playFrames' noFrames framesData
    where 
    framesData :: [String]
    framesData = parseFramesData framesBlob

    playFrames' :: Int -> [String] -> IO () 
    playFrames' _ [] = return ()
    playFrames' 0 _  = return ()
    playFrames' i (f:fs) = do
        putStrLn f
        threadDelay (frameMS * 1000)
        playFrames' (i - 1) fs
        

-- Plays an animation from a file at a certain speed
play :: String -> Int -> IO () 
play filepath frameMS = do
    handle <- openFile filepath ReadMode
    caSize            <- hGetLine handle
    neighStayAlive    <- hGetLine handle
    neighBecomeAlive  <- hGetLine handle
    viewportAngle     <- hGetLine handle
    zRotSpeed         <- hGetLine handle
    noFrames          <- hGetLine handle
    framesBlob        <- hGetContents handle        

    -- Show recording details
    putStrLn (printf "Recording details for '%s':" filepath)
    putStrLn ("Number of frames: " ++ noFrames)
    putStrLn ("CA Rules:\n-> stay alive: " ++ (show neighStayAlive) ++ " neighbours.")
    putStrLn ("-> become alive: " ++ (show neighBecomeAlive) ++ " neighbours.")
    putStrLn (printf "Viewport:\n-> angle: %s\n-> zRotSpeed: %s" viewportAngle zRotSpeed)
    putStr "\nPlay animation? (y/n) "
    shouldContinue <- getChar
    if shouldContinue == 'y' then do
        putStrLn cls
        playFrames framesBlob frameMS (read noFrames)
        putStr resetScreen
        hClose handle
    else hClose handle

