module Day18 where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!),(!?),Map)
import Control.Lens
import qualified Data.Bimap as Bi
import Data.List.Extra
import Data.Maybe

data Acre = Open | Tree | Lumber deriving (Eq, Ord, Show)

acreBiMap = Bi.fromList [(Open,"."),(Tree,"|"),(Lumber,"#")]

showAcre = (acreBiMap Bi.!)
parseAcre = (acreBiMap Bi.!>)

showGrid :: Map (Int, Int) Acre -> String
showGrid = unlines . (fmap . concatMap) (showAcre . snd)  . groupOn (fst . fst) . Map.toList


parse :: String -> Map (Int, Int) Acre
parse str = Map.fromList yed
    where
        yed = concatMap xed $ zip [0..] $ lines str
        xed (y,ys) = (\ (x, c) -> ((y, x), parseAcre [c])) <$> zip [0 ..] ys

change :: Map (Int, Int) Acre -> (Int,Int) -> Acre -> Acre
change grid pos acre 
    | acre == Open
    , length (filter (==Tree) ads) >= 3
    = Tree
    | acre == Tree
    , length (filter (==Lumber) ads) >= 3
    = Lumber
    | acre == Lumber
    , elem Lumber ads && elem Tree ads
    = Lumber
    | acre == Lumber = Open 
    | otherwise = acre
        where
            ads = mapMaybe getAcre $ adjecent pos
            getAcre yx = grid !? yx

adjecent (y,x) = [(y',x')  | y' <- [y-1..y+1],x' <- [x-1..x+1], (y',x') /= (y,x)]

doStep g = Map.mapWithKey (change g) g

count g = trees*lums
    where
        trees = Map.size $ Map.filter (==Tree) g
        lums = Map.size $ Map.filter (==Lumber) g

solveA = count . (!! 10) . iterate doStep 

lim = 1000000000

f prev g c 
    |   c == lim = Left (count g)
    |   Map.member g prev = Right (c, prev ! g, g)
    |   otherwise =
            f (Map.insert g c prev) (doStep g) (c+1)

solveB g = findLim resG loopFrom loopInc
    where
        Right (loopFrom,loopTo,resG) = f Map.empty g 0
        loopInc = loopFrom - loopTo
        
findLim g cur inc 
    | cur+inc >= lim = f Map.empty g cur
    | otherwise = findLim g (cur+inc) inc

main :: IO ()
main = do
    contents <- filter (/='\r') <$> readFile "data/day18.txt"
    
    let p = parse contents
    
    print $ solveA p
    print $ solveB p
