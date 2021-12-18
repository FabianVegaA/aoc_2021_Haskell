import Data.List (group, sort, transpose)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  file <- getArgs >>= readFile . head
  let byteToInt = foldl (\acc x -> acc * 2 + x) 0
  let groupOccurs = (\(x : y : xs) -> if x > y then [0, 1] else [1, 0]) . (fmap length . group . sort)
  print $ product $ fmap byteToInt $ transpose $ fmap groupOccurs (transpose $ lines file)
