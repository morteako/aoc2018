module Day6 where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!),(!?))
import Control.Lens

main :: IO ()
main = do
    contents <- readFile "6.txt"
    print "ok"