{-# LANGUAGE TemplateHaskell #-}
module Day15 where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!),(!?),Map)
import Control.Lens
import Control.Lens.Indexed
import Control.Lens.TH
import Data.List.Extra

data GE = Goblin | Elf deriving Show

data Unit = Unit {
    -- _xPos   :: Int,
    -- _yPos   :: Int,
    _ge     :: GE,
    _hp     :: Int
} | Wall | Dot deriving Show
makeLenses ''Unit


type Grid = Map Pos Unit

data Pos = Pos { _yPos :: Int, _xPos :: Int } deriving (Show,Eq, Ord)
makeLenses ''Pos

parse :: String -> Map Pos Unit
parse xs = Map.fromList yed
    where
        yed = concatMap xed $ zip [0..] $ lines xs
        xed (y,ys) = (\ (x, c) -> (Pos y x, f c)) <$> zip [0 ..] ys
        f '#' = Wall
        f 'G' = Unit Goblin 200
        f 'E' = Unit Elf 200
        f '.' = Dot 
        f x = error $ show x

showMap :: Grid -> String
showMap grid = unlines $ (fmap . fmap) (f . snd) $ groupOn (view yPos . fst) $ Map.toAscList grid
    where
        f Dot = '.'
        f Wall = '#'
        f (Unit Goblin _) = 'G'
        f (Unit Elf _) = 'E'
        --f (Unit Goblin x) = 'e'

printGrid :: Grid -> IO ()
printGrid = putStrLn . showMap

main :: IO ()
main = do
    contents <- filter (/='\r') <$> readFile "data/day15.txt"
    print contents
    let p = parse contents
    print p
    putStrLn $ showMap p