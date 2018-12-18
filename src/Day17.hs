{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Day17 where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!),(!?),Map)
import Text.Regex
import Data.Semigroup
import Data.List.Extra
import Control.Monad.State.Strict
import Control.Lens (Identity(..))
import Data.Maybe
import Control.Monad


newtype X = X {getX :: Int} deriving (Show,Eq,Ord,Bounded, Enum, Num)
newtype Y = Y {getY :: Int} deriving (Show,Eq,Ord,Bounded, Enum, Num)

data Ground = Clay | StillWater | FallWater | Sand | OutsideSand deriving Eq

parse :: String -> (Y,Map (Y,X) Ground)
parse str = (minY,Map.union clayMap sandMap)
    where
        clays = case traverse (matchRegex re) $ lines str of
            Just res -> concatMap f res
            Nothing -> error "fail"
        clayMap = Map.fromList $ ( ,Clay) <$> clays

        allCoords = (,) <$> [0 .. maxY] <*> [minX - 1 .. maxX + 1]
        sandMap = Map.fromList $ (,Sand) <$> allCoords 

        (Min minY,Max maxY,Min minX,Max maxX) = foldMap g clays
        g (y, x) = (Min y, Max y, Min x, Max x)

        re = mkRegex "(.)=([0-9]+), (.)=([0-9]+)..([0-9]+)"
        f [a,aval,b,bstart,bstop] 
            |   a == "x" = [(Y y , X $ read aval) | y <- [read bstart..read bstop]]
            |   a == "y" = [(Y $ read aval , X x) | x <- [read bstart..read bstop]]
            |   otherwise = error $ "WTF" ++ show [a,aval,b,bstart,bstop] 

instance Show Ground where
    show Clay = "#"
    show Sand = "."
    show FallWater = "|"
    show StillWater = "~"
    show OutsideSand = "x"
   
type Grid = Map (Y,X) Ground

data Dir = L | R deriving Show

next L (y,x) = (y,x-1)
next R (y,x) = (y,x+1)

down (y,x) = (y+1,x)
up   (y,x) = (y-1,x)

data Action = Filled | Dropped deriving Show

showGrid :: Grid -> String
showGrid grid = unlines $ (fmap . concatMap) (show . snd)  $ groupOn (fst . fst) $ Map.toList grid

getRes :: (Y,X) -> State Grid Ground
getRes pos = gets (Map.findWithDefault OutsideSand pos)

setStillWater :: (Y,X) -> State Grid ()
setStillWater pos = modify (Map.insert pos StillWater)

setFallWater :: (Y,X) -> State Grid ()
setFallWater pos = modify (Map.insert pos FallWater)

dropWater :: (Y,X) -> State Grid ()
dropWater pos = do
    res <-  getRes pos
    case res of 
        OutsideSand -> return ()
        FallWater   ->  return ()
        Clay    -> upWater (up pos)
        StillWater   -> upWater (down pos)
        Sand    -> do
            setFallWater pos
            dropWater $ down pos


upWater :: (Y,X) -> State Grid ()
upWater pos@(y,x) = do
    l <- spreadWater L pos
    r <- spreadWater R pos
    case (l, r) of
        (Filled,Filled) -> upWater $ up pos
        _ ->  fixSpread L pos >> fixSpread R pos
            
fixSpread :: Dir -> (Y,X) -> State Grid ()
fixSpread dir pos = do
    under <- getRes $ down pos
    res <- getRes pos
    case res of 
        FallWater   -> fixSpread dir (next dir pos)
        StillWater   -> setFallWater pos >> fixSpread dir (next dir pos)
        _    -> return ()
        




spreadWater :: Dir -> (Y,X) -> State Grid Action
spreadWater dir pos = do
    res <- getRes pos
    under <- getRes $ down pos
    if under == Sand || under == FallWater
    then dropWater pos >> return Dropped
    else case res of 
        Clay    -> return Filled
        OutsideSand -> return Dropped
        _    -> setStillWater pos >> spreadWater dir (next dir pos)
        
        
countWater minY = Map.size . Map.filterWithKey f
    where
        f (ky,_) x = ky /= minY && x == StillWater || x == FallWater

fixStillWater :: Map (Y,X) Ground -> Map (Y,X) Ground
fixStillWater grid = Map.mapWithKey f grid
    where
        f pos FallWater = if grid Map.! up pos `elem` [StillWater,Clay] && grid Map.! down pos `elem` [StillWater,Clay] then StillWater else FallWater
        f _ x = x

countStillWater minY = Map.size . Map.filterWithKey f
    where
        f (ky,_) x = ky /= minY && x == StillWater
            

main :: IO ()
main = do
    contents <- readFile "data/day17.txt"
    let (minY,grid) = parse contents
    let Identity (_,s) = runStateT (dropWater (minY, X 500)) grid
    
    print $ countWater minY s

    let fixedGrid = fixStillWater s
    print $ countStillWater minY fixedGrid

    -- writeFile "grid.txt" $ showGrid s
    -- writeFile "grid2.txt" $ showGrid fixedGrid 



    