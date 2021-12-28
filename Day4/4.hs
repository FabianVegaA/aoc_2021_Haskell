import Data.List (find, transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromJust, isJust)
import System.Directory.Internal.Prelude (getArgs)

data GridNum a = Marked a | Unmasked a deriving (Eq, Show, Read)

type Grid a = [[GridNum a]]

data State a = Winner Int (Grid a) | Continue [Grid a] deriving (Show)

main :: IO ()
main = do
  file <- getArgs >>= readFile . head

  let randomNums = parseRandomNums . head . lines $ file
  let grids = parseGrids file

  print $ solve randomNums grids

solve :: [Int] -> [Grid Int] -> Int
solve rn gs = n * total
  where
    Winner n g = foldl markGrids (Continue gs) rn
    total = sum . map (\(Unmasked n) -> n) . filter (not . isMarked) $ concat g

parseRandomNums :: String -> [Int]
parseRandomNums = fmap (\n -> read n :: Int) . splitOn ","

parseGrids :: String -> [Grid Int]
parseGrids =
  fmap
    (fmap (fmap (\n -> Unmasked (read n :: Int)) . words))
    . chunksOf 5
    . filter (/= "")
    . tail
    . lines

isMarked :: GridNum Int -> Bool
isMarked (Marked _) = True
isMarked _ = False

markGrids :: State Int -> Int -> State Int
markGrids (Winner n grid) _ = Winner n grid
markGrids (Continue grids) n
  | isJust bingo = Winner n (fromJust bingo)
  | otherwise = Continue grids'
  where
    bingo = find isBingo grids'

    grids' = fmap (`markGrid` n) grids

    markGrid :: Grid Int -> Int -> Grid Int
    markGrid grid n = fmap (\r -> if Unmasked n `elem` r then mark n r else r) grid

    mark :: Int -> [GridNum Int] -> [GridNum Int]
    mark n = fmap (\x -> if Unmasked n == x then Marked n else x)

    isBingo :: Grid Int -> Bool
    isBingo gd = any (all isMarked) gd || any (all isMarked) (transpose gd)