module Main where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , attrName
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, bg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))
import Text.Printf (printf)
import System.Console.Terminal.Size
import Data.Maybe (fromJust)

-- The custom cellular automaton data structure
import CellAutomaton (CA, initCA, updateCA)

-- Projects CA to a 2d grid
import GridRenderer (caToGrid, Grid, GridPoint)

-- The Tick event, fires every x milliseconds
data Tick = Tick

-- The name of the current window, not used
-- but required by the API, so set to ()
type Name = ()


-- Defines the structure of the main application
-- Most importantly: registers the draw frame function,
-- the main event handler and the color configurations (appAttrMap)
mainApp :: App CA Tick Name
mainApp = App {
    appDraw = drawUI,
    appChooseCursor = neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return,
    appAttrMap = const (attrMap V.defAttr [(attrName "ollo", bg V.red)])
}


-- Sets up the main loop to run at "msFrame" ms per frame.
-- BChan is a part of Brick that automates the tick event
-- to fire repeatedly at a certain interval
mainLoop :: Int -> App CA Tick Name -> IO ()
mainLoop msFrame app = do
    chan <- newBChan 10
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay msFrame 
    ca <- initCA
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void (customMain initialVty buildVty (Just chan) app ca)


-- The main entry point of the app
main :: IO ()
main = mainLoop 100000 mainApp


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
colorGrid :: Grid -> [Widget ()]
colorGrid grid = [str (concatStr item) | item <- gridWithColor]
    where
    concatStr :: [String] -> String
    concatStr = foldr (++) ""
    colorUnit = 255 `div` 1 -- TODO
    gridWithColor :: [[String]]
    gridWithColor = [map (colorChar colorUnit) row | row <- grid]


-- Draws one frame
drawUI :: CA -> [Widget Name]
drawUI ca = [ C.center (withAttr (attrName "ollo") $ vBox rowsArr) ]
    where 
    rowsArr :: [Widget ()]
    rowsArr = colorGrid (caToGrid ca)
    -- rowsArr = [str (show (width (fromJust size)))] -- TODO


-- Handles incoming events
-- For now, supports: 'q' for quitting, and the tick event
-- which signals the need to re-render.
handleEvent :: CA -> BrickEvent Name Tick -> EventM Name (Next CA)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') []))  =  halt g
handleEvent g (AppEvent Tick)                        =  continue (updateCA g)
handleEvent g _                                      =  continue g
