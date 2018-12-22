{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Day15 where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!),(!?),Map)
import Control.Lens
import Control.Lens.Indexed
import Control.Lens.TH
import Data.List.Extra
import Control.Monad.State.Strict
import Control.Lens.Extras (is)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

data GE = Goblin | Elf deriving (Show,Eq,Ord)

data Unit = Unit {
    -- _xPos   :: Int,
    -- _yPos   :: Int,
    _ge     :: GE,
    _hp     :: Int
} | Wall | Dot deriving (Show,Eq,Ord)
makeLenses ''Unit
makePrisms ''Unit


type Grid = Map Pos Unit

data Pos = Pos { _yPos :: Int, _xPos :: Int } deriving (Eq, Ord)
makeLenses ''Pos

instance Show Pos where
    show (Pos y x) = "(Y="++show y++ "," ++ "X=" ++ show x ++ ")"

data Combat = Combat {_grid :: Grid, _roundC :: Int} deriving Show
makeLenses ''Combat

parse :: String -> Map Pos Unit
parse xs = Map.fromList yed
    where
        yed = concatMap xed $ zip [0..] $ lines xs
        xed (y,ys) = (\ (x, c) -> (Pos y x, f c)) <$> zip [0 ..] ys
        f '#' = Wall
        f 'G' = Unit Goblin startHp
        f 'E' = Unit Elf startHp
        f '.' = Dot 
        f x = error $ show x

startHp = 200

showMap :: Grid -> String
showMap grid = unlines $ (fmap . fmap) (f . snd) $ groupOn (view yPos . fst) $ Map.toAscList grid
    where
        f Dot = '.'
        f Wall = '#'
        f (Unit Goblin _) = 'G'
        f (Unit Elf _) = 'E'

printGrid :: Grid -> IO ()
printGrid = putStrLn . showMap


atkVal = 3

updateAttack unit@Unit{..} = if _hp - atkVal <= 0 then Dot else unit & hp -~ atkVal
updateAttack Dot = Dot
updateAttack Wall = error "attacks WALL!?"

attack :: (Pos,Pos) -> State Combat ()
attack (atk, targ) = do
    g <- gets (view grid)
    when (is _Unit (g ! targ)) (grid %= Map.adjust updateAttack targ)


doAttacks :: [(Pos,Pos)] -> State Combat [()]
doAttacks = traverse attack

getOrder :: State Combat [Pos]
getOrder = gets (Map.keys . Map.filter (is _Unit) . view grid)
    
adjecent (Pos y x) = [Pos (y-1) x, Pos (y+1) x,Pos y (x-1), Pos y (x+1)]
adjWithSelf yx = yx:adjecent yx 

data FindState = FindState {_fgrid :: Grid , _reachable :: Set ([Pos],Unit)} deriving Show
makeLenses ''FindState

-- findReachable :: [Pos] -> State FindState ()
-- findReachable (pos:ps) = do
--     traceShow ps when (notElem pos ps) $ do
--         val <- gets (Map.lookup pos . _fgrid)
--         case val of
--             Just Unit{..} -> do
--                 reachable %= Set.insert (pos:ps,Unit{..})                
--             Just Dot    -> traverse (\pp -> findReachable (pp:pos:ps)) (adjecent pos) >> return ()
--             _           -> return ()


--findReachable :: GE -> Grid -> Map Pos [Pos] -> [Pos]
findReachable = undefined
    where
        neighs = undefined
  

doRound = do
    order <- getOrder
    traverse choose order
    roundC %= (+1)
 
choose pos = do
    res <- traceShow pos gets (flip findNearestEnemy pos . _grid)
    traceShow res unless (res == Nothing) $ do
        let Just (path, _) = res
        case path of 
            [unitPos] -> attack (pos,unitPos)
            _ -> do
                unit <- gets ((! pos) . _grid)
                grid %= Map.insert pos Dot
                grid %= Map.insert (last path) unit

findNearestEnemy g pos = findMin $ Set.toList $ removeFriendly $ reachs
    where
        Unit uge _ = g ! pos
        removeFriendly =  Set.filter ((/= uge) . _ge . snd)
        findMin xs = if null xs then Nothing else Just $ minimumOn (\(path,_) -> (length path, head path)) xs
        reachs = _reachable $ execState actions s
        actions = traverse (findReachable . (:[])) (adjecent pos)
        s =  (FindState g Set.empty) 

filterUnits = Map.filter (is _Unit)

startCombat :: Grid -> State Combat a -> Combat
startCombat grid s = execState s (Combat grid 0)
        
testCombat g s = do
    let Combat grid count = startCombat g s
    putStrLn $ showMap grid
    mapM_ print $ Map.toList $ filterUnits grid


main :: IO ()
main = do
    contents <- filter (/='\r') <$> readFile "data/day15t2.txt"
    let inputGrid = parse contents
    putStrLn $ showMap inputGrid
    print inputGrid 
    print "-"
    testCombat inputGrid doRound
    testCombat inputGrid (doRound >> doRound)
    --print $ findNearestEnemy p (Pos 1 1)

    --let att = execState (replicateM 67 (attack (Pos 5 18))) p

    -- let Combat g counter = startCombat p (doAttacks [(Pos 8 1, Pos 8 17),(Pos 8 17, Pos 8 23)])
    -- print $ Map.filter (is _Unit) g
    
    --print $ Map.filter (is _Unit) att
    

    -- print $ Map.filter (is _Unit) p
    
