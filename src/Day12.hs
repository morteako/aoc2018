{-# LANGUAGE Strict #-}

module Day12 where

import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn,divvy)
import Data.Bifunctor
import Debug.Trace
import Data.List

toTup [x,y] = (x,y)

--modified input
parse = parseLines . lines . filter (/='\r')
parseLines (ini : rules) = (False:False:False:toB ini, s)
    where 
        ss = fmap (fmap head . toTup . splitOn " => ") rules
        s = Map.fromList $ fmap (bimap toB (=='#')) $ ss
        toB = fmap (=='#')


f1 = [False]
f2 = [False,False]
f3 = [False,False,False]

type Bs = [Bool]

getCount (x1:x2:x3:_)
    | x1 = (f3,3)
    | x2 = (f2,2)
    | x3 = (f1,1)
    | otherwise = ([],0)

apply rules (state,count) = (ins++newState, count + c)
    where
        (ins,c) = getCount newState
        newState = fmap f fives
        addedState = [False,False] ++ state
        fives = divvy 5 1 $ addedState
        f x = Map.findWithDefault False x rules

            
sumStuff (state, count) = zip [0 - count..] state

fixSum = sum . fmap fst . filter snd

showPlants (s,c) = (fmap (\x -> if x then '#' else '.') s,c)

findLoop c s ((x,z):xs) = traceShow c $ if elem x s then c else findLoop (c+1) (x:s) xs

main :: IO ()
main = do
    contents <- readFile "data/day12.txt"
    let (init,rules) = parse contents

    let res = iterate (apply rules) (init ++ replicate 10000 False,3) 
    let tjue21 = res !! 20
    
    print $ fixSum $ sumStuff $ tjue21
    --part 2 "manually"
