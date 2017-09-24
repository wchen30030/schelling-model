{- 
  File      :  Simulation.hs 
  Copyright : (c) Wei Chen, 02/25/17 
  imports from Grid and Shuffle and runs the simulation
  contains main, printEle, printRow, printGrid, countSpot, 
  checkSatisfyReturn, checkSatisfy
-}

module Main where

import Data.List
import System.Environment 
import System.IO
import System.Random
import Data.Array.IO
import System.Directory
import Control.Monad
import Grid
import Shuffle


{-print next agent symbol-}
printEle :: Spot -> IO ()
printEle x = do
    if x == AgentA
        then do putStr "X"
    else if x == AgentB
        then do putStr "O"
    else do
                putStr "E"


{-print row of symbols-}
printRow :: [Spot] -> IO ()
printRow (x:xs) = do
    if xs==[]
        then do 
            printEle x
            putStrLn ""
            return ()
    else do 
            printEle x
            printRow xs 


{-print the grid of symbols-}
printGrid :: Grid Spot -> IO ()
printGrid (Grid r c grid) = do
    printRow (take c grid)
    if r>1 
        then do 
            printGrid (Grid (r-1) c (drop c grid))
    else
        return ()


{-count number of times a spot appears in a list-}
countSpot :: Spot -> [Spot] -> Int
countSpot s [] = 0
countSpot s (x:xs)
    | s == x = 1 + countSpot s xs
    | otherwise = countSpot s xs


{-second part of checkSatisfy-}
checkSatisfyReturn :: Spot -> [Spot] -> Double -> Grid Spot -> IO Bool
checkSatisfyReturn cell list threshold (Grid r c grid) = do
    let num =  countSpot cell list
    let den = (length list) - (countSpot Empty list)
    if num == 0
        then return (threshold == 0 )
    else do
        return (threshold <= (fromIntegral num) / (fromIntegral den))


{-check if cell is satisfied. 9 cases depending if touching sides-}
checkSatisfy :: Int -> Double -> Grid Spot -> IO Bool
checkSatisfy index threshold (Grid r c grid) = do
    let cell = grid !! index
    if index == 0--UL
        then do
            let list = [grid !! 1] ++ [grid !! c] ++ [grid !! (c+1)]
            checkSatisfyReturn cell list threshold (Grid r c grid)
    else if index == (c-1)--UR
        then do
            let list = [grid !! (c-2)] ++ [grid !! (2*c-2)] ++ [grid !! (2*c-1)]
            checkSatisfyReturn cell list threshold (Grid r c grid)
    else if index == c*(r-1)--LL
        then do
            let list = [grid !! (c*r-c+1)] ++ [grid !! (c*r-2*c+1)] ++ [grid !! (c*r-2*c)]
            checkSatisfyReturn cell list threshold (Grid r c grid)
    else if index == (c*r-1)--LR
        then do
            let list = [grid !! (c*r-2)] ++ [grid !! (c*r-2-c)] ++ [grid !! (c*r-c-1)]
            checkSatisfyReturn cell list threshold (Grid r c grid)
    else if index < (c-1)--U
        then do
            let list = [grid !! (index-1)] ++ [grid !! (index+1)] ++ [grid !! (index+c)] ++ [grid !! (index+c-1)] ++ [grid !!(index+c+1)]
            checkSatisfyReturn cell list threshold (Grid r c grid)
    else if index > c*(r-1)--Lower
        then do
            let list = [grid !! (index-1)] ++ [grid !! (index+1)] ++ [grid !! (index-c)] ++ [grid !! (index-c-1)] ++ [grid !!(index-c+1)]
            checkSatisfyReturn cell list threshold (Grid r c grid)
    else if 0 == (mod index c)--Left
        then do
            let list = [grid !! (index-c)] ++ [grid !! (index+c)] ++ [grid !! (index+1)] ++ [grid !! (index+1-c)] ++ [grid !!(index+1+c)]
            checkSatisfyReturn cell list threshold (Grid r c grid)
    else if (c-1) == (mod index c)--R
        then do
            let list = [grid !! (index-c)] ++ [grid !! (index+c)] ++ [grid !! (index-1)] ++ [grid !! (index-1-c)] ++ [grid !!(index-1+c)]
            checkSatisfyReturn cell list threshold (Grid r c grid)
    else do--Middle
            let list = [grid !! (index-c)] ++ [grid !! (index+c)] ++ [grid !! (index-1)] ++ [grid !! (index+1)] ++ [grid !!(index-1+c)] ++ [grid !!(index-1-c)] ++ [grid !!(index+1+c)]++ [grid !!(index+1-c)]
            checkSatisfyReturn cell list threshold (Grid r c grid)


{-
outputs list of int with satisfied A & B, empty, unsatisfied A & B
empty = 0, satisfied A = 1, satisfied B = 2, unsatisfied A = 3, unsatisfied B = 4
-}
allTypes :: Int -> [Int] -> Double -> Grid Spot -> IO [Int]
allTypes index typeList threshold (Grid r c grid) = do
    if index == r*c
        then do
            return typeList
    else if (grid !! index) == Empty 
        then do
            let typeList' = typeList ++ [0]
            allTypes (index+1) typeList' threshold (Grid r c grid)
    else if (grid !! index) == AgentA
        then do 
            satis <- checkSatisfy index threshold (Grid r c grid)
            if satis
                then do 
                    let typeList' = typeList ++ [1]
                    allTypes (index+1) typeList' threshold (Grid r c grid)
            else do
                    let typeList' = typeList ++ [3]
                    allTypes (index+1) typeList' threshold (Grid r c grid)
    else do 
            satis' <- checkSatisfy index threshold (Grid r c grid)
            if satis'
                then do 
                    let typeList' = typeList ++ [2]
                    allTypes (index+1) typeList' threshold (Grid r c grid)
            else do
                    let typeList' = typeList ++ [4]
                    allTypes (index+1) typeList' threshold (Grid r c grid)


{-changes number of empty(0) to 1 or 2 based on indices-}
changeEmpty :: Int -> [Int] -> [Int] -> IO [Int]
changeEmpty b indices convert = do
    if length indices == 0
        then do
            return convert
    else do
        let indices' = drop 1 indices
        let index = head indices
        let convert' = (take index convert) ++ [b] ++ (drop (index+1) convert)
        changeEmpty b indices' convert'


{-converts the same amount of unsatisfied A from empty to A-}
convertToAgent :: Int -> Int -> [Int] -> IO [Int]
convertToAgent a b convert = do
    let count = length $filter (==a) convert
    if count == 0
        then do
            return convert
    else do
        let indices = findIndices (==0) convert
        temp <- shuffle indices
        let indices' = take count temp
        changeEmpty b indices' convert


{-converts all unsatisfied to empty-}
convertToEmpty :: [Int] -> IO [Int]
convertToEmpty convert = do
    let ind1 = findIndices (==3) convert
    let ind2 = findIndices (==4) convert
    let ind = ind1 ++ ind2
    changeEmpty 0 ind convert


{-convert symbol abbreviation back to spot. last step in one iteration-}
convertToSpot :: [Spot] -> [Int] -> IO [Spot]
convertToSpot spot convert = do
    if length convert == 0
        then do
            return spot
    else do
        let convert' = drop 1 convert
        let current = head convert
        if current == 0
            then do
                let spot' = spot ++ [Empty]
                convertToSpot spot' convert'
        else if current == 1
            then do
                let spot' = spot ++ [AgentA]
                convertToSpot spot' convert'
        else do
                let spot' = spot ++ [AgentB]
                convertToSpot spot' convert'


{-one iteration of moving unsatisfied-}
oneIteration :: Double -> Grid Spot -> IO (Grid Spot)
oneIteration threshold (Grid r c grid) = do
    convert <- allTypes 0 [] threshold (Grid r c grid)
    if (False == elem 3 convert && False == elem 4 convert)
        then do 
            return (Grid r c grid)
    else do
        convert' <- convertToAgent 3 1 convert
        convert'' <- convertToAgent 4 2 convert'
        convert''' <- convertToEmpty convert''
        finalConvert <- convertToSpot [] convert'''
        return (Grid r c finalConvert)


{-iterate until finish-}
allIterations :: Double -> Grid Spot -> IO ()
allIterations threshold (Grid r c grid) = do
    (Grid r c grid') <- oneIteration threshold (Grid r c grid)
    if grid' == grid
        then do
            putStrLn "Final Grid"
            printGrid (Grid r c grid')
            return()
    else do
            allIterations threshold (Grid r c grid')


{-initializes the simulation-}
main :: IO () 
main = do
    args <- getArgs
    putStrLn "Please enter the threshold. E.g. 0.3"
    th  <- getLine
    let t = read th::Double
    if (args == []) || (length args ==1 && args!!0== "-i")
        then do 
            i <- randomRIO(0,2) :: IO Int
            let g = initialGrids !! i
            putStrLn "Initial Grid"
            printGrid g
            allIterations t g
    else if length args ==1 && head args == "-r"
        then do
            putStrLn "How many rows?"
            row <- getLine
            let r = read row::Int
            putStrLn "How many columns?"
            col <- getLine
            let c = read col::Int
            putStrLn "Number of Xs?"
            xs <- getLine
            let x = read xs::Int
            putStrLn "Number of Os?"
            os <- getLine
            let o = read os::Int
            grid <- shuffle (replicate x AgentA ++ replicate o AgentB ++ replicate (r*c-x-o) Empty)
            let g = Grid r c grid
            putStrLn "Initial Grid"
            printGrid g
            allIterations t g
    else do
            putStrLn "ERROR: invalid argument(s)."

