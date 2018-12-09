module Day09 where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq

maxPlayers = 493
maxMarble = 71863*100


next p = mod p maxPlayers + 1

getIndex i seq 
    | i == Seq.length seq = i
    | otherwise =  i `mod` Seq.length seq

doStuff :: Map Int Int -> Seq.Seq Int -> Int -> Int -> Int -> Map Int Int
doStuff players marbles pc mc curMarbleI 
    | mc == maxMarble + 1 = players
    | mod mc 23 == 0 =
        let
            p = Map.adjust (+ (mc + prevCurMarble)) pc players
            cur7 = getIndex (curMarbleI - 7) marbles
            prevCurMarble = Seq.index marbles cur7
            marDel = Seq.deleteAt cur7 marbles
            newCurMarble = Seq.index marDel cur7
        in
            doStuff p marDel (next pc) (mc+1) cur7
    | otherwise = 
        let
            newCurMarbleI = (getIndex (curMarbleI + 2) marbles)
            s = Seq.insertAt newCurMarbleI mc marbles  
        in 
            doStuff players s (next pc) (mc+1) newCurMarbleI
        

start = doStuff (Map.fromList $ zip [1..maxPlayers] (repeat 0)) (Seq.fromList [0]) 1 1 0 


main :: IO ()
main = print $ maximum start