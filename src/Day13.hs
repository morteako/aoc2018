module Day13 where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!),(!?))
import Control.Lens
import qualified Data.Bimap as Bi
import Data.List.Extra


data Dir = U | D | L | R deriving (Show,Eq,Ord)
data Turn = TurnRett | TurnVenstre | TurnHoyre deriving (Show,Eq)
data Rail = Straight | Corner deriving (Show,Eq)

tup = (,)

dirBimap = Bi.fromList [tup U "^", tup D "v", tup L "<", tup R ">"]
showDir = (dirBimap Bi.!)
parseDir = (dirBimap Bi.!>)

-- rBimap = Bi.fromList [tup LLCorner "\\", tup LRCorner "/", tup L "<", tup R ">"]
showRail Straight = "."
showRail Corner = "x"

parseRail '\\' = Corner
parseRail '/' = Corner
parseRail '|' = Straight
parseRail '-' = Straight

showMap :: Map.Map (Int,Int) Char -> String
showMap = unlines . (fmap . concatMap) snd  . groupOn (fst . fst) . Map.toList . Map.map (:[])

putMap = putStrLn . showMap

parse str = (dict, Map.filter (\x -> Bi.memberR [x] dirBimap) dict)
    where
        dict = Map.fromList yed
        yed = concatMap xed $ zip [0..] $ lines str
        xed (y,ys) = (\ (x, c) -> ((y, x), c)) <$> zip [0 ..] ys




main :: IO ()
main = do
    contents <- filter (/='\r') <$> readFile "data/day13t.txt"
    
    let (d,p) = parse contents
    putMap d
    print p
