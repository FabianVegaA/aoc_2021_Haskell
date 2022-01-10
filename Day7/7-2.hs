import Data.List (sort)
import Data.List.Split (splitOn)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  file <- getArgs >>= readFile . head
  print $ minimumFuel $ parseInput file

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

minimumFuel :: [Int] -> Int
minimumFuel l = sum $ fmap ((\n -> n * (n + 1) `div` 2) . abs . (-) minPositionFuel) l
  where
    minPositionFuel = media l

media :: [Int] -> Int
media l = sum l `div` length l
