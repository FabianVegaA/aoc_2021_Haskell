import Data.List.Split (splitOn)
import System.Directory.Internal.Prelude ( getArgs )

main :: IO ()
main = do
  file <- getArgs >>= readFile . head

  let digitalOutputs = fmap (words . (!! 1) . splitOn "|") . lines $ file

  print $ length . filter ((`elem` [2, 3, 4, 7]) . length) . foldl1 (++) $ digitalOutputs