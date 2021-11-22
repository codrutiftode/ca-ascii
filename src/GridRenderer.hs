module GridRenderer (caToGrid, GridPoint, Grid, ViewportOptions,
    caToViewport, depthAnalysis, prepareRenderCA)
where

import CellAutomaton (CA, caToList)
import Vector3
import Tree (treeFromList, treeHasNode)
import Data.List (nubBy, sort, reverse, find)

type Viewport = [Vector3]
type GridPoint = (Char, Int)
type Grid = [[GridPoint]]
type ViewportOptions = (Angle, Int) -- Viewport angle / zRotationSpeed


-- Converts a CA to a viewport with floating-point coordinates
-- First, it rotates the CA cube around the Z-axis by a certain angle determined
-- by the rotation speed and the stage of evolution.
-- Then, it rotates the whole viewport around the x-axis by the angle that
-- the viewport is with respect to the XY plane, so that a top-down view is achieved.
-- Afterwards, the xy coords on the screen can be calculated from x and y components
-- of the vectors, and the z component gives a depth for each point, that can be used
-- to compute the brightness of a pixel.
caToViewport :: Int -> ViewportOptions -> ([Vector3], Int) -> Viewport
caToViewport caSize vOptions (cells, stage) = rotateVectorsX viewportBalancingAngle viewport
    where 
    viewportAngle = fst vOptions
    caCubeRotSpeed = snd vOptions
    caAngle = (fromIntegral (stage * caCubeRotSpeed `mod` 360) + 1) / 180 * pi

    viewport :: [Vector3]
    viewport = rotateVectorsZ caAngle cells 
      
    -- The normal is perpendicular to x-axis, at "viewportAngle" radians to the XY plane
    viewportNormal :: Normal 
    viewportNormal = (0, -(cos viewportAngle), sin viewportAngle)

    -- The angle between the viewport and the XY plane.
    -- Rotate by it and it has the same effect as aligning the viewport and the XY plane,
    -- or achieving a kind of "top-down" view.
    viewportBalancingAngle :: Angle
    viewportBalancingAngle = asin (1 / (lenV viewportNormal) * (viewportNormal `dotV` (0, 1, 0)))


-- Depth analysis to remove points at the same position, but different depths
depthAnalysis :: Viewport -> [(Int, Int, Int)]
depthAnalysis v = nubBy (\(x1,y1,_) -> \(x2,y2,_) -> x1 == x2 && y1 == y2) (reverse (sort integralPoints))
    where
    integralPoints :: [(Int, Int, Int)]
    integralPoints = map (\(x,y,z) -> (round x, round y, round z)) v


-- Converts a viewport to a grid of displayable characters
viewportToGrid :: Int -> Viewport -> Grid
viewportToGrid caSize v = [[let vCell = (i,j) `vCellLookup` vCells in (charMap vCell, getDepth vCell)
                            | i <- [limL..limR]]
                            | j <- reverse [limL..limR]]
    where
    limL :: Int
    limL = (-caSize `div` 2) - (caSize `div` 10 * 4)
    limR = (caSize `div` 2) + (caSize `div` 10 * 4)
    vCells = depthAnalysis v 

    charMap :: Maybe a -> Char
    charMap (Just _) = '#'
    charMap Nothing  = ' '

    getDepth :: Maybe (Int, Int, Int) -> Int
    getDepth (Just (a, b, depth)) = depth
    getDepth Nothing = 0

    vCellLookup :: (Int, Int) -> [(Int, Int, Int)] -> Maybe (Int, Int, Int)
    vCellLookup pos vCells = find (\(x,y,z) -> (x,y) == pos) vCells 


-- Adds extra elements to a CA, for rendering purposes (for example,
-- adding a container box around the CA). This should always be called
-- before rendering a CA to get it in the right format.
prepareRenderCA :: CA -> [Vector3] -> ([Vector3], Int)
prepareRenderCA ca extraVs = ((caToList ca) ++ extraVs, snd ca)

-- Converts a CA to displayable 2d grid of characters
caToGrid :: Int -> ViewportOptions -> ([Vector3], Int) -> Grid
caToGrid caSize vOptions preparedCA = viewportToGrid caSize (caToViewport caSize vOptions preparedCA)
