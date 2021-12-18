import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  file <- getArgs >>= readFile . head
  print $ uncurry (*) $ driveSub $ getInstructions $ lines file

driveSub :: [(String, Int)] -> (Int, Int)
driveSub xs = foldl1 (\(x, y) (a, b) -> (x + a, y + b)) $ toCoord <$> xs
  where
    toCoord (i, v) =
      case i of
        "forward" -> (v, 0)
        "down" -> (0, v)
        "up" -> (0, - v)
        _ -> (0, 0)

iterInstructions :: (Int, Int, Int) -> [(String, Int)] -> (Int, Int, Int)
iterInstructions curr [] = curr
iterInstructions curr (i : is) = iterInstructions (drive i curr) is
  where
    drive (i, v) (h, d, a) =
      case i of
        "forward" -> (h + v, v * a + d, a)
        "down" -> (h, d, a + v)
        "up" -> (h, d, a - v)
        _ -> (h, d, a)

getInstructions :: [String] -> [(String, Int)]
getInstructions [] = []
getInstructions (x : xs) = (i, read v :: Int) : getInstructions xs
  where
    (i, v) = break (== ' ') x
