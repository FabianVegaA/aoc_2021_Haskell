import Data.List (delete, find, transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromJust, isJust)
import System.Directory.Internal.Prelude (getArgs)

data GridNum a = Marked a | Unmasked a deriving (Eq, Show, Read)

type Grid a = [[GridNum a]]

data State a = Winner Int (Grid a) [Grid a] | Continue [Grid a] deriving (Show)

main :: IO ()
main = do
  file <- getArgs >>= readFile . head

  let randomNums = parseRandomNums . head . lines $ file
  let grids = parseGrids file

  print $ solve randomNums grids

solve :: [Int] -> [Grid Int] -> Int
solve rn gs =
  let Winner n g _ = foldl markGrids (Continue gs) rn
      total = sum . map (\(Unmasked n) -> n) . filter (not . isMarked) . concat $ g
   in total * n

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

markGrids :: State Int -> Int -> State Int
markGrids (Winner i grid grids) n
  | isJust bingo = Winner n (fromJust bingo) grids'
  | otherwise = Winner i grid grids'
  where
    (bingo, grids') = bingoRest n grids
markGrids (Continue grids) n
  | isJust bingo = Winner n (fromJust bingo) grids'
  | otherwise = Continue grids'
  where
    (bingo, grids') = bingoRest n grids

bingoRest :: Int -> [Grid Int] -> (Maybe (Grid Int), [Grid Int])
bingoRest n grids =
  let grids' = fmap (`markGrid` n) grids
      bingo = find isBingo grids'
   in ( bingo,
        case bingo of
          Just _ -> filter (not . isBingo) grids'
          Nothing -> grids'
      )

markGrid :: Grid Int -> Int -> Grid Int
markGrid grid n = fmap (\r -> if Unmasked n `elem` r then mark n r else r) grid
  where
    mark n = fmap (\x -> if Unmasked n == x then Marked n else x)

isBingo :: Grid Int -> Bool
isBingo grid = any (all isMarked) grid || any (all isMarked) (transpose grid)

isMarked :: GridNum Int -> Bool
isMarked (Marked _) = True
isMarked _ = False