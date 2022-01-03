import Data.List (group, sort, transpose)
import System.Directory.Internal.Prelude (getArgs)

data Support = O2 | CO2 deriving (Eq, Show)

main :: IO ()
main = do
  file <- getArgs >>= readFile . head
  let byteChains = lines file
  print $ product $ (\sp -> byteToInt $ scrubberRating sp 0 byteChains) <$> [O2, CO2]

byteToInt :: String -> Int
byteToInt = foldl (\acc x -> acc * 2 + x) 0 . map (\x -> read [x])

scrubberRating :: Support -> Int -> [String] -> String
scrubberRating _ _ [x] = x
scrubberRating sp i bs = scrubberRating sp (i + 1) $ filter scrubber bs
  where
    z : o : _ = fmap length . group . sort $ fmap (!! i) bs
    scrubber = \s ->
      s !! i == case sp of
        O2 -> if z > o then '0' else '1'
        CO2 -> if z <= o then '0' else '1'
