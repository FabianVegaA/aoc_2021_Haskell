import Data.List.Split (splitOn)
import System.Directory.Internal.Prelude (getArgs)

iterations :: Int
iterations = 256

main :: IO ()
main = do
  file <- getArgs >>= readFile . head

  let input = (\i -> read i :: Int) <$> splitOn "," file

  print $ length $ last $ take (iterations + 1) . iterate nextState $ input

nextState :: [Int] -> [Int]
nextState xs = nextIter ++ replicate (length . filter (== 0) $ xs) 8
  where
    nextIter = fmap next xs
    next x = case x of
      0 -> 6
      _ -> pred x
