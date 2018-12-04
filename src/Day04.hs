module Day04 where

import qualified Data.MultiSet as MSet
import Data.Maybe
import Data.List
import Data.List.Extra
import Data.Ord
import Data.Time

parse = sortOn getDT . map f . lines . filter (/='\r')
    where
        parseDay :: String -> Maybe Day
        parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" 
        parseTime :: String -> Maybe TimeOfDay
        parseTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" 


        parseStr ["Guard",'#':nr,_b,_s] = Begins $ read nr
        parseStr ["falls",_]= Falls
        parseStr ["wakes",_]= Wakes

        day xs = fromJust $ parseDay $ tail $ take 17 xs
        time xs  = fromJust $ parseTime $ tail $ take 17 xs
        constructor xs = parseStr $ words $ drop 19 xs

        f xs =  constructor xs (day xs) (time xs)

data Action = Begins Int Day TimeOfDay | Falls Day TimeOfDay | Wakes Day TimeOfDay deriving (Eq,Show)

getDT (Begins _ day time) = (day,time)
getDT (Falls day time) = (day,time)
getDT (Wakes day time) = (day,time)

tup4 (_,_,_,d) = d

solveA (infos) = sleepyMin*sleepyGuard
    where
        sleepyMin = (snd . fst) $ maximumOn snd $ MSet.toOccurList $ MSet.filter (\(k,v) -> k == sleepyGuard) sleepMins
        sleepyGuard = fst $ head $ (maximumOn (sum . fmap snd)) $ groupOn fst $ fmap (\((a,b),c) -> (a,c)) $ MSet.toOccurList sleepMins
        sleepMins = tup4 $ foldl' generateMinutes (undefined, undefined, undefined, MSet.empty) infos

        generateMinutes :: (Int,Day,TimeOfDay,MSet.MultiSet (Int,Int)) -> Action -> (Int,Day,TimeOfDay,MSet.MultiSet (Int,Int))
        generateMinutes (_,_,_,mset) (Begins nr day tod) = (nr, day, tod, mset)
        generateMinutes (guard, prevDay, prevTime ,mset) (Wakes wday wtod) = 
            let mins = MSet.fromList $ zip (repeat guard) [todMin prevTime .. todMin wtod - 1]
            in (guard, wday, wtod, MSet.union mset mins)
        generateMinutes (guard, prevDay, prevTime ,mset) (Falls wday wtod) = 
            (guard, wday, wtod, mset)
        

solveB (infos) = (uncurry (*)) $ fst $ maximumBy (comparing snd) $ MSet.toOccurList sleepMins
    where
        sleepMins = tup4 $ foldl' generateMinutes (undefined, undefined, undefined, MSet.empty) infos

        generateMinutes :: (Int,Day,TimeOfDay,MSet.MultiSet (Int,Int)) -> Action -> (Int,Day,TimeOfDay,MSet.MultiSet (Int,Int))
        generateMinutes (_,_,_,mset) (Begins nr day tod) = (nr, day, tod, mset)
        generateMinutes (guard, prevDay, prevTime ,mset) (Wakes wday wtod) = 
            let mins = MSet.fromList $ zip (repeat guard) [todMin prevTime .. todMin wtod - 1]
            in (guard, wday, wtod, MSet.union mset mins)
        generateMinutes (guard, prevDay, prevTime ,mset) (Falls wday wtod) = 
            (guard, wday, wtod, mset)




            
        


main :: IO ()
main = do
    contents <- readFile "data/day4.txt"
    print $ solveA . parse $ contents
    print $ solveB . parse $ contents