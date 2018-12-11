module Day10 where

import qualified Data.Map.Strict as Map
import Data.Char (isDigit)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Semigroup (Min(..),Max(..))
import Data.List.Extra (groupOn)
import Data.Coerce (coerce)

parse :: String -> Set [Int]
parse = Set.fromList . map (map read . words) . lines . filter (\x -> isDigit x || x == '-' || x == ' ' || x == '\n')

        
getRes = zipWith (,) [0..] . iterate (Set.map addVelo)
    where addVelo [x,y,vx,vy] = [x+vx,y+vy,vx,vy]

calc :: Set [Int] -> (Int, String)
calc s = (maxY - minY, display togheter)
    where
        set = Set.map (\[x,y,_,_] -> (y,x)) s

        (minY, maxY, minX, maxX) = coerce $ foldMap f set
        f (y,x) = (Min y, Max y, Min x, Max x)

        mapX = Map.fromSet (const '#') set
        allElse =  Map.fromList [((y,x),'.') | y <- [minY..maxY], x <- [minX..maxX]]
        togheter = Map.union mapX allElse

display = unlines . (map . map) snd . groupOn (fst . fst) . Map.toAscList

solve = head . filter (\(_,(yDist,_)) -> yDist < 10) . (fmap . fmap) calc . getRes

main :: IO ()
main = do
    contents <- readFile "data/day10.txt"
    let (sec,(_,msg)) = solve $ parse contents 
    putStrLn msg
    print sec
    
    