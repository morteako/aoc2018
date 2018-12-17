module Day14 where


import qualified Data.Sequence as Seq
import Data.Sequence 
import Data.Maybe
import Data.List
import Control.Lens (view)
import Control.Lens.Tuple
import Debug.Trace
import Data.Foldable (toList)
import Data.List.Split



f (seq,elf1,elf2) = (newSeq,nelf1,nelf2)
    where
        
        nelf1 = (elf1 + elf1Val + 1) `mod` newSeqSize
        nelf2 = (elf2 + elf2Val + 1) `mod` newSeqSize
        newSeqSize = Seq.length newSeq
        newSeq = foldl' (Seq.|>) seq val
        elf1Val = Seq.index seq elf1
        elf2Val = Seq.index seq elf2

        elfSum = elf1Val + elf2Val
        val = toNr elfSum

toNr x = if x >= 10 then 1 <| mod x 10 <| Seq.empty else Seq.singleton x

g x (s,a,b) = (foldMap show $ Seq.take 10 $ Seq.drop x s,a,b)

main :: IO ()
main = do
    let res = iterate f (Seq.fromList [3,7],0,1)
    let solveA target = view _1 $ g target $ foldl' (\x _ -> f x) (Seq.fromList [3,7],0,1) [1..target+10]
    putStrLn $ solveA 540561
    
    let res = foldl' (\x _ -> f x) (Seq.fromList [3,7],0,1) [1..40000000]
    let check target = fst $ head $ Data.List.filter ((==target) . snd) $ Data.List.zip [0..] $ divvy 6 1 $ toList $ view _1 res
    
    print $ check [5,4,0,5,6,1]
    
    

