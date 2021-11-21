-- Defines a 3d cellular automaton and operations on it
module CellAutomaton (CA, updateCA, initCA, caSize, caToList, Vector3) 
where

import Tree (Tree, treeHasNode, treeFromList, treeToList)
import Vector3

type CellVal = Int
type CellPos = Vector3
type CA = (Tree CellPos, Int)

-- The size of the 3d CA cube
caSize :: Int
caSize = 40


-- Get the value of the cell at the given position
getCellVal :: CellPos -> Tree CellPos -> CellVal
getCellVal pos cells = fromEnum (treeHasNode pos cells)


-- Returns the neighbours for a cell
-- Currently using the VonNeumann neighbourhood
neighbours :: CellPos -> Tree CellPos -> [CellVal]
neighbours (x,y,z) cells = [1 | i <- [1,-1], j <- [1,-1], k <- [1,-1], 
                             getCellVal (x+i, y+j, z+k) cells == 1]


-- Returns the new value for a cell based on
-- its previous value and its neighbours
-- Currently: Crystal Growth (Jason Rampe) 1: 0-6/1,3/2/N
updateCell :: CellVal -> [CellVal] -> CellVal
updateCell prev neighbours
    | prev == 1 = 1
    | prev == 0 = fromEnum (length (neighbours) `elem` [1,3])


-- Updates the whole CA
updateCA :: CA -> CA 
updateCA (cells, stage) = (cells, stage + 1) -- DUMMY: Uncomment below
updateCA (cells, stage) = (newCells, stage + 1) 
     where
     newCells = treeFromList [ cellPos | x <- [limL..limR], 
                                         y <- [limL..limR],
                                         z <- [limL..limR], 
                                         let cellPos = intToVect (x,y,z), 
                     updateCell (getCellVal cellPos cells) (neighbours cellPos cells) == 1]
                     where limL = - (caSize `div` 2)
                           limR = (caSize `div` 2)


-- Initializes the CA 
initCA :: CA
initCA = (treeFromList initPointsF, 0) 
    where 
    limL = -caSize `div` 2
    limR = caSize `div` 2
    initPoints = concat [[(limL, limL, x), (limL, limR, x),
                  (limR, limL, x), (limR, limR, x),
                  (x, limL, limR), (x, limL, limL),
                  (x, limR, limL), (x, limR, limR),
                  (limL, x, limR), (limL, x, limL),
                  (limR, x, limL), (limR, x, limR)]
                  | x <- [limL..limR]]
    initPointsF = map intToVect initPoints

initCA' :: CA
initCA' = (treeFromList [(0, 0, 0)], 0)


-- Converts a CA to a list, hiding the underlying representation
caToList :: CA -> [CellPos]
caToList (cells, _) = treeToList cells

-- Gets the current stage of the CA
getCAStage :: CA -> Int
getCAStage (cells, stage) = stage

