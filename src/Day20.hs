{-# LANGUAGE TemplateHaskell #-}

module Day20 where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!),(!?),Map)
import Text.ParserCombinators.ReadP 
import Data.List.Split
import Data.List
import Data.Maybe
import Control.Monad
import qualified Control.Monad.State.Strict as S

import Control.Lens
import Control.Lens.TH

data Reg = Lit Char | Group [Reg] | Alt [[Reg]] deriving (Show,Eq)


parseStart = do
    char '^'
    parseRe


parseRe :: ReadP [Reg]
parseRe = do
    c <- look
    case c of
        "$" -> get >> return []
        (')':_) -> get >> return []
        ('(':_) -> do
            get
            p <- parseRe
            rest <- parseRe
            return $ Group p : rest
        (x:_) -> do
            get
            rest <- parseRe
            return $ Lit x : rest
        "" -> error "no $"

            
fix (Group xs) = Alt $ splitOn [Lit '|'] $ fmap fix xs
fix (Lit c) = Lit c

runParse p s = 
    case readP_to_S p s of
        [(x,"")] -> x
        _ -> error "parse error"


showRe' (Lit c) = [c]
showRe' (Group xs) =  error "SHOULDNT HAPPEN"
showRe' (Alt xs) = "(" ++ intercalate "|" s ++ ")"
    where
        s = fmap (concatMap showRe' ) xs
        
showRe reg = "^" ++ foldMap showRe' reg ++ "$"

getRe s = fix <$> runParse parseStart s

getDirPos (y,x) 'W' = ((y,x-1),(y,x-2))
getDirPos (y,x) 'E' = ((y,x+1),(y,x+2))
getDirPos (y,x) 'N' = ((y-1,x),(y-2,x))
getDirPos (y,x) 'S' = ((y+1,x),(y+2,x))

data Ground = Wall | Door | Open deriving (Show,Eq)

createMap m pos [] = m
createMap m pos (Lit d:rest) = 
    let
        (doorPos,nextPos) = getDirPos pos d
        newM = Map.insert doorPos Door m
        newM' = Map.insert nextPos Open newM
    in
        if Map.member doorPos m then m else createMap newM' nextPos rest
createMap m pos (Alt alts:rest) = let
        f mm alt = createMap mm pos (alt++rest)
    in
        foldl' f m alts

data Walk = Walk {_grid :: Map (Int,Int) Ground, _paths :: Map (Int,Int) Int} deriving (Show,Eq)
makeLenses ''Walk

walk :: (Int,Int) -> Int -> S.State Walk ()
walk pos c = do
    Walk m ps <- S.get

    when (maybe True (c <) (Map.lookup pos ps)) $ do
        paths %= Map.insertWith min pos c
        let ns = neighs pos
        let p (n,nn) = Map.findWithDefault Wall n m == Door
        let doors = filter p ns
        let step (n,nn) = 
                walk nn (c+1)
        mapM_ step doors


neighs pos = fmap (\x -> (addTup x pos, addTup x $ addTup x pos)) newDir
    where 
        addTup (a,b) (c,d) = (a+c,b+d)
        newDir = [(1,0), (-1,0),(0,-1),(0,1)]
        

solveA' str = S.execState (walk (0,0) 0) (Walk grid Map.empty)
    where 
        grid = createMap Map.empty (0,0) (getRe str)

solveA = maximum . _paths . solveA'
solveB = length . filter (>=1000) . Map.elems . _paths . solveA'

main :: IO ()
main = do
    contents <- readFile "data/day20.txt"
    print $ solveA contents
    print $ solveB contents