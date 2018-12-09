{-# LANGUAGE TemplateHaskell #-}

module Day08 where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Lens
import Control.Lens.Lens
import Control.Lens.TH
import Data.Maybe (mapMaybe)

data T = T {_counter :: Int, _restT :: [Int]}
makeLenses ''T

data Tree = Tree (Map Int Tree) [Int]


findSum :: Tree -> Int
findSum (Tree m meta) = sum meta + sum (Map.map findSum m)

rootVal :: Tree -> Int
rootVal (Tree m meta) 
    | Map.null m = sum meta
    | otherwise = sum $ fmap rootVal $ mapMaybe (`Map.lookup` m) meta

solve :: String -> (Int,Int)
solve xs = (findSum tre,rootVal tre)
    where 
        tre :: Tree
        tre = go . map (read::String -> Int) . words $ xs
        
        go (c:metas:rest) = evalState (createTree 1 c metas) (T 0 rest)
        
        createTree :: Int -> Int -> Int -> State T Tree
        createTree childCounter 0 m = do
            -- <<%= updates the state, but returns the old value
            rest <- restT <<%= drop m
            return $ Tree Map.empty $ take m rest
        createTree childCounter c m = do
            (childs : metas : rest) <- restT <<%= drop 2
            childTree <- createTree 1 childs metas
            Tree restOfCurrent metas <- createTree (childCounter+1) (c-1) m
            return $ Tree (Map.insert childCounter childTree restOfCurrent) metas 

main :: IO ()
main = readFile "data/day8.txt" >>= print . solve 
    
    


