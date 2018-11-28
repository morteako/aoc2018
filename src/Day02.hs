module Day02 where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!),(!?))
import Control.Lens

main :: IO ()
main = do
    contents <- readFile "02.txt"
    print "ok"