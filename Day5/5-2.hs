{-# LANGUAGE MultiWayIf #-}

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  file <- getArgs >>= readFile . head

  let linesInput = parseLinesInput file
  let lineMap = foldl markMap (replicate 1000 . replicate 1000 $ '.') linesInput

  print $ length . filter (\c -> c /= '.' && c /= '1') $ concat lineMap

parseLinesInput :: String -> [((Int, Int), (Int, Int))]
parseLinesInput = fmap (unpackCoords . splitCoords . splitOn " -> ") . lines
  where
    strToInt i = read i :: Int
    splitCoords = fmap (fmap strToInt . splitOn ",")
    unpackCoords = \((x1 : y1 : xs1) : (x2 : y2 : xs2) : xs) -> ((x1, y1), (x2, y2))

markMap :: [String] -> ((Int, Int), (Int, Int)) -> [String]
markMap m ((x1, y1), (x2, y2)) =
  foldl (\m' (i, j) -> insertAt i j m') m if
        | abs (y2 - y1) == abs (x2 - x1) -> diagonalPos ((x1, y1), (x2, y2))
        | y1 == y2 -> linealPosX ((x1, y1), (x2, y2))
        | x1 == x2 -> linealPosY ((x1, y1), (x2, y2))
        | otherwise -> []
  where
    mark :: Char -> Char
    mark c = if isDigit c then succ c else '1'

    insertAt :: Int -> Int -> [String] -> [String]
    insertAt i j m =
      let (init', mid' : end') = splitAt j m
          (midInit, v : midEnd) = splitAt i mid'
       in init' ++ (midInit ++ mark v : midEnd) : end'

    linealPosX :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
    linealPosX ((x1, y1), (x2, _)) = [(x, y1) | x <- makeRange x1 x2]

    linealPosY :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
    linealPosY ((x1, y1), (_, y2)) = [(x1, y) | y <- makeRange y1 y2]

    makeRange :: Int -> Int -> [Int]
    makeRange a b = if a < b then [a .. b] else [b .. a]

    diagonalPos :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
    diagonalPos ((x1, y1), (x2, y2)) = [(x, (c - slop * a) + slop * x) | x <- [a .. b]]
      where
        slop = (y2 - y1) `div` (x2 - x1) :: Int
        (a, b, c) = if x1 > x2 then (x2, x1, y2) else (x1, x2, y1)
