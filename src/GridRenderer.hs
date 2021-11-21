module GridRenderer (caToGrid, GridPoint, Grid, 
    caToViewport, depthAnalysis, 
    viewportBalancingAngle, viewportNormal)
where

import CellAutomaton (CA, caToList, caSize)
import Vector3
import Tree (treeFromList, treeHasNode)
import Data.List (nubBy, sort, reverse, find)

type Viewport = [(Vector3, Float)]
type GridPoint = (Char, Int)
type Grid = [[GridPoint]]

-- Constant viewport normal
viewportNormal :: Normal 
viewportNormal = (0, -caSizeF, caSizeF)
    where caSizeF = fromIntegral caSize

-- Projecting 3d on 2d viewport leaves the viewport vectors in 3d space.
-- We need to rotate the viewport so that it is parallel to the XY plane, so we get
-- the angle between the viewport normal and the x basis vector, then rotate the viewport
-- clockwise by that angle.
viewportBalancingAngle :: Angle
viewportBalancingAngle = asin (1 / (lenV viewportNormal) * (viewportNormal `dotV` (0, 1, 0)))

-- Projects vectors on the plane with the given normal
projectVectors :: [Vector3] -> Normal -> Viewport
projectVectors vs n = map projV vs
    where
    projV :: Vector3 -> (Vector3, Float)
    projV p = (((-depth) `scaleV` n) `addV` p, depth) 
        where depth = (n `dotV` p) / (n `dotV` n)

-- projectVectors :: [Vector3] -> Normal -> Viewport
-- projectVectors vs n = map (\(x,y,z) -> ((x,y,0), 0)) vs


-- Rotates vectors in a viewport around the x axis, while maintaining depth values
rotateViewportX :: Viewport -> Angle -> Viewport
rotateViewportX v a = map rotateXExt v
    where
    rotateXExt :: (Vector3, Float) -> (Vector3, Float)
    rotateXExt (v, depth) = (rotateX a v, depth)


-- Converts a CA to a viewport with floating-point coordinates
caToViewport :: CA -> Viewport
caToViewport ca = rotateViewportX projectedViewport viewportBalancingAngle
-- caToViewport ca = rotateViewportX projectedViewport 0
    where cells = caToList ca
          stage = snd ca
          caAngle = (fromIntegral (stage `mod` 360) + 1) / 180 * pi
          projectedViewport = projectVectors (rotateVectorsZ caAngle cells) viewportNormal


-- Depth analysis to remove points at the same position, but different depths
depthAnalysis :: Viewport -> [((Int, Int), Int)]
depthAnalysis v = nubBy (\x y -> fst x == fst y) (reverse (sort integralPoints))
    where
    integralPoints :: [((Int, Int), Int)]
    integralPoints = [((round x, round y), round depth) | ((x,y,z), depth) <- v ]


-- Counts the number of different depths
-- Useful for the color scheme
countDepths :: Grid -> Int
countDepths grid = 1 -- TODO


-- Converts a viewport to a grid of displayable characters
viewportToGrid :: Viewport -> Grid
viewportToGrid v = [[let vCell = (i,j) `vCellLookup` vCells in (charMap vCell, getDepth vCell)
                    | i <- [limL..limR]]
                    | j <- reverse [limL..limR]]
    where
    limL :: Int
    limL = (-caSize `div` 2) - (caSize `div` 10 * 4)
    limR = (caSize `div` 2) + (caSize `div` 10 * 4)
    vCells = depthAnalysis v 

    charMap :: Maybe a -> Char
    charMap (Just _) = '#'
    charMap Nothing  = '-'

    getDepth :: Maybe ((Int, Int), Int) -> Int
    getDepth (Just ((a, b), depth)) = depth
    getDepth Nothing = 0

    vCellLookup :: (Int, Int) -> [((Int, Int), Int)] -> Maybe ((Int, Int), Int)
    vCellLookup pos vCells = find ((== pos) . fst) vCells 


-- Used externally: converts a CA to displayable 2d grid of characters
caToGrid :: CA -> Grid
caToGrid = viewportToGrid . caToViewport
