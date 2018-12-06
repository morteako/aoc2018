module Day05 where

import Data.Char (toLower)
import Control.Arrow ((&&&))


react :: String -> String
react (x:y:xs) 
    |   toLower x == toLower y && x /= y = react xs
    |   otherwise = x:react (y:xs)
react xs = xs

--Should have used foldr and do everything in one pass instead of finding the fix point
-- -1 because of newline at the end
fullyReact :: String -> Int
fullyReact = subtract 1 . length . until ((==) <*> react) react
    
solveA = fullyReact 

solveB xs =  minimum [fullyReact $ filter ((char /=) . toLower) xs| char <- ['a'..'z']]
   
main :: IO ()
main = readFile "data/day5.txt" >>= print . (solveA &&& solveB)