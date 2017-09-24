{- 
  File      :  Grid.hs 
  Copyright : (c) Wei Chen, 02/25/17 
  Contains Spot, Grid, grid1, grid2, grid3, initialGrids
-}

module Grid
(
    Spot(..),
    Grid(..),
    grid1,
    grid2,
    grid3,
    initialGrids

) where 

import Data.List
import System.Environment 
import System.IO
import System.Random
import Data.Array.IO
import Control.Monad
import Shuffle


{- The two agents 
AgentA = X, AgentB = O, Empty = E
-}
data Spot = AgentA | AgentB | Empty
    deriving (Eq, Show)


{- The number of rows and columns followed by the actual grid data -}
data Grid a = Grid {nRows :: Int, nCols :: Int, grid :: [a]}
    deriving (Eq, Show)


{-shuffle (replicate 40 AgentA ++ replicate 40 AgentB ++ replicate 20 Empty)-}
grid1 :: Grid Spot 
grid1 = Grid 10 10 [AgentA,AgentB,Empty,AgentA,Empty,Empty,AgentA,AgentA,Empty,Empty,AgentB,AgentB,AgentB,AgentB,AgentA,AgentB,AgentA,AgentA,AgentB,AgentB,AgentB,AgentA,AgentB,AgentB,AgentA,AgentB,AgentB,AgentB,AgentB,AgentA,AgentA,AgentA,AgentA,AgentA,AgentB,AgentA,Empty,AgentB,AgentB,AgentA,Empty,AgentB,AgentA,AgentB,AgentB,AgentA,Empty,AgentB,AgentA,AgentA,AgentA,AgentA,AgentB,AgentA,AgentB,AgentB,AgentB,AgentB,AgentB,Empty,AgentB,AgentB,Empty,Empty,Empty,AgentA,Empty,Empty,AgentB,AgentA,Empty,Empty,AgentA,AgentB,AgentB,AgentA,AgentA,AgentA,Empty,AgentA,Empty,AgentA,AgentA,AgentA,AgentB,AgentB,AgentA,AgentB,AgentA,AgentB,AgentB,AgentA,AgentA,AgentA,AgentB,Empty,Empty,AgentA,AgentB,AgentA]


{-shuffle (replicate 42 AgentA ++ replicate 42 AgentB ++ replicate 16 Empty)-}
grid2 :: Grid Spot 
grid2 = Grid 10 10 [AgentA,AgentA,AgentB,AgentB,AgentB,AgentB,AgentB,AgentB,Empty,AgentA,AgentA,Empty,Empty,AgentB,Empty,AgentA,AgentA,Empty,AgentB,AgentA,AgentB,AgentA,AgentB,AgentA,AgentA,AgentA,AgentA,AgentA,AgentA,Empty,AgentB,AgentA,AgentB,AgentB,AgentB,AgentB,AgentA,Empty,AgentA,Empty,AgentB,AgentB,AgentA,AgentA,AgentB,AgentA,AgentA,AgentA,AgentB,AgentA,AgentB,AgentB,AgentA,AgentB,AgentB,AgentB,Empty,AgentB,AgentA,AgentB,AgentB,AgentA,Empty,AgentB,AgentA,AgentB,Empty,AgentB,AgentB,AgentA,AgentB,AgentA,AgentB,Empty,AgentA,Empty,AgentA,AgentA,Empty,AgentA,AgentA,Empty,AgentB,AgentA,AgentB,AgentA,AgentA,AgentB,AgentB,AgentA,AgentB,AgentB,AgentB,AgentA,Empty,AgentA,AgentA,AgentB,AgentB,AgentA]


{-shuffle (replicate 38 AgentA ++ replicate 38 AgentB ++ replicate 24 Empty)-}
grid3 :: Grid Spot 
grid3 = Grid 10 10 [Empty,AgentA,Empty,AgentB,AgentA,AgentA,Empty,AgentA,AgentA,AgentB,AgentA,AgentB,AgentB,AgentB,AgentA,AgentB,Empty,Empty,AgentA,AgentA,AgentA,AgentA,AgentA,AgentB,AgentB,AgentA,AgentB,AgentA,AgentA,AgentB,Empty,Empty,AgentA,AgentA,Empty,AgentA,AgentB,AgentA,AgentB,AgentA,AgentB,AgentB,Empty,Empty,Empty,AgentB,AgentB,AgentA,AgentB,Empty,AgentB,AgentB,AgentB,AgentA,Empty,AgentB,AgentB,Empty,AgentA,AgentB,AgentB,AgentB,Empty,AgentB,AgentB,AgentB,AgentA,AgentB,AgentA,Empty,AgentA,Empty,AgentA,AgentA,AgentA,AgentA,AgentB,AgentB,Empty,AgentA,AgentB,AgentB,Empty,Empty,AgentB,AgentA,AgentA,AgentA,AgentA,AgentA,AgentB,Empty,AgentB,AgentA,Empty,AgentB,AgentA,Empty,Empty,AgentB]


initialGrids :: [Grid Spot]
initialGrids = [grid1, grid2, grid3]

