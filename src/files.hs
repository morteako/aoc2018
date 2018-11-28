import Data.Foldable

main = do
    contents <- readFile "Day1.hs"

    let repl :: String -> String
        repl v = concatMap (\x -> if x == '1' then v else [x]) contents


    forM_ (fmap (("0"++) . show) [1..9] ++ fmap show [10..24]) $ \x ->
        writeFile ("Day" ++ x ++ ".hs") (repl x) 