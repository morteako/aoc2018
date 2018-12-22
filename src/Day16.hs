module Day16 where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!),(!?),Map)
import Control.Lens
import Data.Bits
import Data.List.Split
import Data.List


--Horrible stuff. Should refactor everything

eval _ (Val v) = v
eval m (Reg r) = m Map.! r



create :: Int -> Int -> Int -> [Opcode]
create a b c = res
    where 
        res = regVal ++ regReg ++ valReg
        regReg = [ f (Reg a) (Reg b) c  | f <- [Add,Mul,Ban,Bor]  ] ++ [ f (Reg a) (Reg b) c  | f <- [Gt,Eq]  ]
        regVal = [ f (Reg a) (Val b) c  | f <- [Add,Mul,Ban,Bor]  ] ++ [ f (Reg a) (Val b) c  | f <- [Gt,Eq]  ] ++ [Set (Reg a) (Val b) c]
        valReg = [ f (Val a) (Reg b) c  | f <- [Gt,Eq]  ] ++ [Set (Val a) (Reg b) c]


evalOp :: Map Int Int -> Opcode -> Map Int Int
evalOp before op = Map.insert c res before
    where
        evalb :: Var -> Int
        evalb = eval before
        (res,c) = case op of
            Add a b c -> (evalb a + evalb b, c)
            Mul a b c -> (evalb a * evalb b, c)
            Ban a b c -> (evalb a .&. evalb b, c)
            Bor a b c -> (evalb a .|. evalb b, c)
            Set a _ c -> (evalb a, c)
            Gt a b c -> (fromEnum $ evalb a > evalb b, c)
            Eq a b c -> (fromEnum $ evalb a == evalb b, c) 

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

getCons :: Opcode -> String
getCons op = 
    case op of
        Add  _ (Val _) _ -> "addi"
        Add  _ (Reg _) _ -> "addr"
        Mul _ (Val _) _ -> "muli"
        Mul _ (Reg _) _ -> "mulr"
        Ban _ (Val _) _ -> "bani"
        Ban _ (Reg _) _ -> "banr"
        Bor _ (Val _) _ -> "bori"
        Bor _ (Reg _) _ -> "borr"
        Gt  (Reg _) (Val _) _ -> "gtri"
        Gt  (Val _) (Reg _) _ -> "gtir"
        Gt  (Reg _) (Reg _) _ -> "gtrr"
        Eq  (Reg _) (Val _) _ -> "eqri"
        Eq  (Val _) (Reg _) _ -> "eqir"
        Eq  (Reg _) (Reg _) _ -> "eqrr"
        Set (Val _) _ _ ->  "seti"
        Set (Reg _) _ _ ->  "setr"


parse :: String -> [(Int,Map Int Int, Map Int Int, [Opcode])]
parse str = f $ lines str
    where

        f (before: vals: after:"":rest) = let [i,a,b,c] = map read $ words vals in (i,toMap $ drop 8 before, toMap $ drop 7 after, create a b c) : f rest
        f xs = []

        toMap = Map.fromList . zip [0..] . read


    
countOps :: (Int,Map Int Int, Map Int Int, [Opcode]) -> Int
countOps (_, before, after, ops) = length $ filter (checkRes before after) ops

findValid :: (Int,Map Int Int, Map Int Int, [Opcode]) -> Set.Set String
findValid (_, before, after, ops) = Set.fromList $ getCons <$> filter (checkRes before after) ops

parse2 :: String -> [[Int]]
parse2 s = fmap (map read . words) $ tail $ lines $ splitOn "\n\n\n" s !! 1


findCorrect :: Map Int (Set.Set String) ->  [(Int,Map Int Int, Map Int Int, [Opcode])] -> Map Int (Set.Set String) 
findCorrect m ops = foldl' f m ops
    where
        f mm op = Map.adjust (Set.intersection $ findValid op) (view _1 op) mm

fixOps = fmap (over _2 (Set.elemAt 0)) . fixOps' . Map.toList
fixOps' d = if all ((==1) . Set.size . snd) xs then d else (zi, z):fixOps' ys
    where
        xs@((zi,z):zs) = sortOn (Set.size . snd) d
        ys = fmap (over _2 (flip Set.difference z)) zs


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
createOp "seti" a b c = Set (Val a) undefined c
createOp "setr" a b c = Set (Reg a) undefined c

solveB state ops instrs = foldl' f state instrs
    where
        f s [i,a,b,c] = evalOp s $ getOpCons i a b c
        getOpCons i = createOp $ ops Map.! i
        
solveA = length . filter (>=3) . map countOps

main :: IO ()
main = do
    contents <- filter (/='\r') <$> readFile "data/day16.txt"

    let p = parse contents
    
    print $ 547 == solveA p

    let testProg = parse2 contents
    let m =  Map.fromList $ fmap (\(i,_,_,ops) -> (i,Set.fromList $ fmap getCons ops)) p
    let fixedCorrs = Map.fromList $ fixOps $ findCorrect m p

    print $ 582 == ((Map.! 0) $ solveB (Map.fromList $ [(x,0) | x <- [0..3]]) fixedCorrs testProg)