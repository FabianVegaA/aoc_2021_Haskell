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
minimumFuel l = sum $ fmap (abs . (-) minPositionFuel) l
  where
    minPositionFuel = median l

median :: [Int] -> Int
median l =
  let l' = sort l
      n = length l'
   in if odd n
        then l' !! (n `div` 2)
        else (l' !! (n `div` 2 - 1) + l' !! (n `div` 2)) `div` 2