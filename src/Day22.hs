module Day22 where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!),(!?))
import Control.Lens

main :: IO ()
main = do
    contents <- readFile "22.txt"
    print "ok"