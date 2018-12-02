module Day02 where

import Data.List 

solveA xs = res2 * res3
    where
        chars = fmap (group . sort) xs
        res2 = length [() | c <- chars, any ((==2) . length ) c]
        res3 = length [() | c <- chars, any ((==3) . length ) c]

solveA2 xs = product $ fmap res [2,3]
    where
        chars = fmap (group . sort) xs
        res x = length [() | c <- chars, any ((==x) . length ) c]
        

solveB xs = fmap snd $ head [x `intersect` y | x <- ixed, y <- ixed, length (x \\ y) == 1]
    where
        ixed = fmap (zip [0..]) xs

main :: IO ()
main = do
    contents <- readFile "data/day2.txt"
    print $ solveA $ lines contents
    print $ solveA2 $ lines contents
    print $ solveB $ lines contents