{-# LANGUAGE TupleSections #-}

module Day06 where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Semigroup
import Data.List
import Data.Coerce

manhattanDistance (xa,ya) (xb,yb) = abs (xa - xb) + abs (ya - yb)

make = Map.mapMaybe snd . foldl' f Map.empty
    where
        f dict (pos,point) = Map.insertWith ins point (manhattanDistance pos point ,Just pos) dict
        ins (newDist, newVal) (oldDist, oldVal)
            | newDist < oldDist = (newDist, newVal)
            | newDist == oldDist = (oldDist, Nothing)
            | otherwise = (oldDist, oldVal)

solveA positions = maximum $ Map.withoutKeys nearestMap outsidePositions
    where
        nearestMap = Map.foldl' (\dict s -> Map.insertWith (+) s 1 dict) Map.empty $ make positionPoints
        outsidePositions = Set.fromList $ Map.elems $ make positionLimitPoints

        (minX,maxX,minY,maxY) = tops positions
        points = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
        lims = fmap (0,) [0..maxY] ++ fmap (maxX,) [0..maxY] ++ fmap (,0) [0..maxX] ++ fmap (,maxY) [0..maxX]
        
        positionPoints = (,) <$> positions <*> points
        positionLimitPoints = (,) <$> positions <*> lims

solveB positions = length $ filter f points
    where
        limit = 10000 
        f x = limit > sum (fmap (manhattanDistance x) positions)
        (minX,maxX,minY,maxY) = tops positions
        points = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
        

tops :: [(Int,Int)] -> (Int,Int,Int,Int)
tops = coerce . foldMap f 
    where
        f (x,y) = (Min x, Max x, Min y, Max y) 

parse = fmap (\x -> readPos $ '(' : x ++ ")") . lines
    where
        readPos :: String -> (Int,Int)
        readPos = read 


main :: IO ()
main = do
    contents <- readFile "data/day6.txt"
    let parsed = parse contents
    print $ solveA parsed
    print $ solveB parsed
