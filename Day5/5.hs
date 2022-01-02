import Data.Char (isDigit)
import Data.List (transpose)
import Data.List.Split (split, splitOn)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  file <- getArgs >>= readFile . head

  let unpackCoords = (\((x1 : y1 : xs1) : (x2 : y2 : xs2) : xs) -> ((x1, y1), (x2, y2)))
  let linesInput = fmap (unpackCoords . fmap (fmap (\i -> read i :: Int) . splitOn ",") . splitOn " -> ") . lines $ file

  let lineMap = foldl markMap (replicate 1000 . replicate 1000 $ '.') linesInput

  print $ length . filter (\c -> c /= '.' && c /= '1' && c /= '\n') $ unlines lineMap

markMap :: [String] -> ((Int, Int), (Int, Int)) -> [String]
markMap m ((x1, y1), (x2, y2))
  | y1 == y2 =
    let row = m !! y1
        (a, b) = if x1 > x2 then (x2, x1) else (x1, x2)
        init' = take a row
        mid' = take (b - a + 1) (drop a row)
        end' = drop (b + 1) row

        initRows = take y1 m
        endRows = drop (y1 + 1) m

        mark i = if isDigit i then succ i else '1'
     in initRows ++ [init' ++ fmap mark mid' ++ end'] ++ endRows
  | x1 == x2 = transpose $ markMap (transpose m) ((y1, x1), (y2, x2))
  | otherwise = m
