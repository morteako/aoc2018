{-# LANGUAGE RecordWildCards #-}

module Day03 where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!),(!?))
import Data.List

data Claim = Claim {
    claimId :: Int,
    leftEdge :: Int,
    topEdge :: Int,
    w :: Int,
    h :: Int
} deriving Show

parse = fmap (f . fmap read . words . fmap rep) . lines
    where
        rep x = if elem x "x:#@," then ' ' else x
        f [claimId,leftEdge, topEdge, w,h] = Claim{..}
        
side = 1000

insides Claim{..} = (,) <$> [left..right] <*> [bot..top]
    where
        top = side - topEdge - 1
        bot = top - h + 1
        left = leftEdge
        right = leftEdge + w - 1

solveA claims = Map.size $ Map.filter (>=2) $ foldl' f Map.empty $ concatMap insides claims
    where
        f dict (x,y) = Map.insertWith add (x,y) 1 dict
        add _ old = old + 1

solveB claims = claimId $ head $ filter (all (\x -> counts ! x == 1) . insides) claims
    where
        counts = foldl' f Map.empty $ concatMap insides claims
        f dict (x,y) = Map.insertWith add (x,y) 1 dict
        add _ old = old + 1

main :: IO ()
main = do
    contents <- readFile "data/day3.txt"
    print $ solveA . parse $ contents
    print $ solveB . parse $ contents