module Day11 where


import Data.List.Extra 
import Data.List.Split (divvy)
import Control.Lens.Tuple (_1,_3)
import Control.Lens (view, over)
import Debug.Trace

power gridSerial (x,y) = hunDig - 5
    where
        rackId = x+10
        powLev = rackId * y
        inc = powLev + gridSerial
        powLev2 = max 0 (inc * rackId)
        hunDig = (powLev2 `div` 100) `mod` 10

lim = 300

grid :: [(Int,Int)]
grid = (,) <$> [1..lim] <*> [1..lim]


gridPow = fmap (\(x,y) -> (x,y,power 9005 (x,y))) grid

--non-sum version
--threeGroups = concat . fmap (fmap concat . divvy 3 1) . transpose . fmap (divvy 3 1) . groupOn fst3 $ gridPow

threeGroups = concat . fmap (fmap sumStuffGen . divvy 3 1) $ transpose $ fmap (fmap sumStuffGen . divvy 3 1) . groupOn (view _1) $ gridPow

add4 w (x,y,z) = (x,y,z,w)

solveA = maximumOn (view _3) threeGroups

--Takes a list of tuples (with at least 3 fields), and returns a tuple (x,y,s,z) where x,y,z are the corresponding values from the first tuple
--and s is the sum of all third field values
sumStuffGen xs = foldl' (\v x -> over _3 (+ (view _3 x)) v) (head xs) (tail xs)

allGroups = concatMap f [1..300]
    where
        f size = traceShow size concat . fmap (fmap sumStuffGen . divvy size 1) . transpose . fmap (fmap sumStuffGen . divvy size 1) $ gridGrouped size
        gridGrouped size = groupOn (view _1) $ fmap (add4 size) gridPow

solveB = maximumOn (view _3) allGroups

main :: IO ()
main = do
    print $ solveA
    -- slow. Takes ~5 minutes
    print $ solveB 
    
