{-# LANGUAGE BangPatterns #-}

module Day07 where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Map.Strict ((!),(!?))
import Data.Char (isUpper)
import Data.Tuple (swap)
import Data.Tuple.Extra (uncurry3)
import Debug.Trace
import Data.List

parse xs = allBefore
    where
        tuppels = map (f . filter isUpper) . lines $ xs
        
        f [_,x,y] = (x,y)
        after = Map.fromListWith (++) $ (fmap . fmap) (:[]) tuppels
        before = Map.fromListWith Set.union $ fmap (fmap Set.singleton . swap) tuppels
        allBefore = Map.unionWith Set.union before $ Map.fromSet (const Set.empty) everyKey
        everyKey = Set.union (Map.keysSet after) (Map.keysSet before)
        
solveA before = findSteps "" Set.empty before
solveB before = findAndCount "" Set.empty before 0 Set.empty

findSteps :: String -> Set Char -> Map Char (Set Char) -> String
findSteps res current before 
    | Map.null before    = reverse $ Set.toDescList current ++ res
    | Set.null current = 
        let 
            (reached, notReached) = Map.partition check before
            check x = Set.isSubsetOf x (Set.fromList res)
        in
            findSteps res (Map.keysSet reached) notReached
    | otherwise = 
        let
            (cur, delCurrent) = Set.deleteFindMin current
            newRes = cur : res
            (reached, notReached) = Map.partition check before
            check x = Set.isSubsetOf x (Set.fromList newRes)
            
            newCurrent = Set.union delCurrent (Map.keysSet reached)      
        in 
            findSteps newRes newCurrent notReached

wlim = 5
negVal = 4

--Horrible. But at least it does A and B in one pass
findAndCount :: String -> Set Char -> Map Char (Set Char) -> Int -> Set (Int,Char) -> (String,Int)
findAndCount res current before sec workers
    | Map.null before && Set.null workers && Set.null current = 
        (reverse res,sec - 1)
    | Map.null before && Set.null workers
    , Just (cur, delCurrent)  <- Set.minView current = 
        let     
            newWork = fromEnum cur - negVal + sec
            newWorkers = (Set.insert (newWork, cur) workers)
        in 
           findAndCount res delCurrent before sec newWorkers
    | Just ((w,c),delWork) <- Set.minView workers
    , w == sec =
        let
            newRes = c : res
            (reached, notReached) = Map.partition check before
            check x = Set.isSubsetOf x (Set.fromList newRes)
            
            newCurrent = Set.union current (Map.keysSet reached)     
        in
            findAndCount newRes newCurrent notReached sec delWork
    | Set.null current && Set.null workers = 
        let 
            (reached, notReached) = Map.partition check before
            check x = Set.isSubsetOf x (Set.fromList res)
            newSec = if Map.null reached then sec else sec+1
        in
            findAndCount res (Map.keysSet reached) notReached newSec workers
    | Just (cur, delCurrent)  <- Set.minView current
    , Set.size workers < wlim =
        let     
            newWork = fromEnum cur - negVal + sec
        in 
           findAndCount res delCurrent before sec (Set.insert (newWork, cur) workers)
    
    | otherwise = 
        findAndCount res current before (sec+1) workers
    

main :: IO ()
main = do
    contents <- readFile "data/day7.txt"
    let parsed = parse contents
    print $ solveA parsed
    print $ solveB parsed
