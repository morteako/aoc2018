module Day04 where

import qualified Data.MultiSet as MSet
import Data.Maybe
import Data.List
import Data.List.Extra
import Data.Tuple.Extra (thd3)
import Data.Time

--Cluncky parsing. Should have used regex
parse = sortOn getDT . map parseLine . lines . filter (/='\r')
    where
        parseDay :: String -> Maybe Day
        parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" 
        parseTime :: String -> Maybe TimeOfDay
        parseTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" 

        parseStr ["Guard",'#':nr,_b,_s] = Begins $ read nr
        parseStr ["falls",_]= Falls
        parseStr ["wakes",_]= Wakes

        day  = fromJust . parseDay . tail . take 17 
        time   = fromJust . parseTime . tail . take 17 
        constructor  = parseStr . words . drop 19 

        parseLine xs =  constructor xs (day xs) (time xs)

        getDT (Begins _ day time) = (day,time)
        getDT (Falls day time) = (day,time)
        getDT (Wakes day time) = (day,time)

data Action = Begins Int Day TimeOfDay | Falls Day TimeOfDay | Wakes Day TimeOfDay deriving (Eq,Show)

createSleepMins infos = thd3 $ foldl' generateMinutes (undefined, undefined, MSet.empty) infos
    where
        generateMinutes (guard, prevTime ,mset) action = 
            case action of
                Begins nr day tod -> (nr, tod, mset)
                Wakes wday wtod ->
                    let mins = MSet.fromList $ zip (repeat guard) [todMin prevTime .. todMin wtod - 1]
                    in (guard, wtod, MSet.union mset mins)
                Falls wday wtod -> (guard, wtod, mset)

solveA infos = sleepyMin*sleepyGuard
    where
        sleepyMin = (snd . fst) $ maximumOn snd $ MSet.toOccurList $ MSet.filter (\(k,v) -> k == sleepyGuard) sleepMins
        sleepyGuard = fst $ head $ (maximumOn (sum . fmap snd)) $ groupOn fst $ MSet.toOccurList $ MSet.map fst sleepMins
        sleepMins = createSleepMins infos
           
solveB infos = (uncurry (*)) $ fst $ maximumOn snd $ MSet.toOccurList $ createSleepMins infos


main :: IO ()
main = do
    contents <- readFile "data/day4.txt"
    print $ solveA . parse $ contents
    print $ solveB . parse $ contents