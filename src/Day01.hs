module Day01 where

import qualified Data.Set as Set
import Data.List

import Debug.Trace

parse :: String -> [Int]
parse = fmap read . lines . filter (/='+')

solveA :: String -> Int
solveA = sum . parse

--First solution
solveB :: String -> Int
solveB = g Set.empty 0 . cycle . parse
    where
        g cur c (x:xs) = if Set.member c cur then c else g (Set.insert c cur) (c+x) xs

--Using scanl instead of manually doing the addition
solveB2 :: String -> Int
solveB2 = g Set.empty . scanl (+) 0 . cycle . parse
    where
        g cur (x:xs) = if Set.member x cur then x else g (Set.insert x cur) xs

--One-liner
solveB3 :: String -> Int
solveB3 = fst . head . filter (uncurry Set.member) . (zip <*> scanl (flip Set.insert) Set.empty) . scanl (+) 0 . cycle . parse
        
main :: IO ()
main = do
    contents <- readFile "data/day1.txt"
    print $ solveA contents
    print $ solveB contents
    print $ solveB2 contents
    print $ solveB3 contents
    
    

