{-# LANGUAGE Strict #-}
module Day19 where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!),(!?),Map)
import Control.Lens
import Data.Bits
import Data.List.Split
import Data.List

import qualified Debug.Trace as T
traceShow xs = T.traceShow xs

eval _ (Val v) = v
eval m (Reg r) = m Map.! r


evalOp :: Map Int Int -> Opcode -> Map Int Int
evalOp before op = Map.insert c res before
    where
        evalb :: Var -> Int
        evalb = eval before
        (res,c) = case op of
            Add a b c -> (eval before a + eval before b, c)
            Mul a b c -> (eval before a * eval before b, c)
            Ban a b c -> (eval before a .&. eval before b, c)
            Bor a b c -> (eval before a .|. eval before b, c)
            Set a _ c -> (eval before a, c)
            Gt a b c -> (fromEnum $ eval before a > eval before b, c)
            Eq a b c -> (fromEnum $ eval before a == eval before b, c) 

checkRes before after op = after == evalOp before op

data Var = Val Int | Reg Int deriving (Show,Eq,Ord)

data Opcode = 
        Add Var Var Int
    |   Mul Var Var Int
    |   Ban Var Var Int
    |   Bor Var Var Int
    |   Gt Var Var Int
    |   Eq Var Var Int
    |   Set Var Var Int
    deriving Show

parse :: String -> (Map Int Int, Int, Map Int Opcode)
parse str = f $ lines str
    where
        f (ipLine:ins) = (Map.fromList [(x,if x == 0 then 1 else 0) | x <- [0..5]], read . head . tail . words $ ipLine, Map.fromList $ zip [0..] (map (g . words) ins ))

        g [i,a,b,c] = createOp i (read a) (read b) (read c)
        

createOp "addi" a b c = Add (Reg a) (Val b) c
createOp "addr" a b c = Add (Reg a) (Reg b) c
createOp "muli" a b c = Mul (Reg a) (Val b) c
createOp "mulr" a b c = Mul (Reg a) (Reg b) c
createOp "bani" a b c = Ban (Reg a) (Val b) c
createOp "banr" a b c = Ban (Reg a) (Reg b) c
createOp "bori" a b c = Bor (Reg a) (Val b) c
createOp "borr" a b c = Bor (Reg a) (Reg b) c
createOp "gtri" a b c = Gt (Reg a) (Val b) c
createOp "gtir" a b c = Gt (Val a) (Reg b) c
createOp "gtrr" a b c = Gt (Reg a) (Reg b) c
createOp "eqri" a b c = Eq (Reg a) (Val b) c
createOp "eqir" a b c = Eq (Val a) (Reg b) c
createOp "eqrr" a b c = Eq (Reg a) (Reg b) c
createOp "seti" a b c = Set (Val a) (Reg b) c
createOp "setr" a b c = Set (Reg a) (Reg b) c

incIp = Map.adjust (+1)

doInstrs ip ops regs c
    | c >= 1000000 = error "stop"
    | traceShow (Map.elems regs) False = regs
    | ipVal <- regs ! ip
    , Just ins <- ops !? ipVal
    = traceShow ipVal doInstrs ip ops (incIp ip $ evalOp regs ins) (c+1)
    | otherwise = regs

main :: IO ()
main = do
    contents <- filter (/='\r') <$> readFile "data/day19.txt"

    let (regs,ip,ops) = parse contents
    
    --print (regs,ip,ops)

    --print $ doInstrs ip ops regs 0 
    --print $ doInstrs ip ops (Map.fromList $ zip [0..] [0,9,19,10551408,10551407,0]) 0
    let nums = [1, 2, 3, 4, 6, 7, 8, 12, 14, 16, 21, 24, 28, 31, 42, 48, 56, 62, 84, 93, 112, 124, 168, 186, 217, 248, 336, 372, 434, 496, 651, 744, 868, 1013, 1302, 1488, 1736, 2026, 2604, 3039, 3472, 4052, 5208, 6078, 7091, 8104, 10416, 12156, 14182, 16208, 21273, 24312, 28364, 31403, 42546, 48624, 56728, 62806, 85092, 94209, 113456, 125612, 170184, 188418, 219821, 251224, 340368, 376836, 439642, 502448, 659463, 753672, 879284, 1318926, 1507344, 1758568, 2637852, 3517136, 5275704, 10551408]
    print $ sum  nums