import Data.List (delete, find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  file <- getArgs >>= readFile . head

  let digitalOutputs = fmap ((\(x : y : xs) -> (words x, words y)) . splitOn "|") . lines $ file

  print $ sum $ fmap (uncurry decodeMsg) digitalOutputs

decodeMsg :: [String] -> [String] -> Int
decodeMsg xs ys = sum $ zipWith (\i u -> u * 10 ^ i) [0 ..] (fromJust . (`lookup'` dict) <$> ys)
  where
    dict = getSeqDict xs

lookup' :: String -> [(String, Int)] -> Maybe Int
lookup' k kv = snd <$> result
  where
    result = find ((=== k) . fst) kv

(===) :: (Eq a) => [a] -> [a] -> Bool
(===) xs' ys' = length xs' == length ys' && (and . fmap (`elem` ys') $ xs')

contain :: Eq a => [a] -> [a] -> Bool
contain xs ys = length xs <= length ys && (and . fmap (`elem` ys) $ xs)

getSeqDict :: [String] -> [(String, Int)]
getSeqDict seqs = zip [zero, one, two, three, four, five, six, seven, eigth, nine] [0 ..]
  where
    Just one = find ((== 2) . length) seqs
    Just four = find ((== 4) . length) seqs
    Just seven = find ((== 3) . length) seqs
    Just eigth = find ((== 7) . length) seqs

    of_six_parts = filter ((== 6) . length) seqs
    Just nine = find (contain four) of_six_parts
    Just zero = find (contain seven) $ delete nine of_six_parts
    six = head $ filter (`notElem` [nine, zero]) of_six_parts

    of_five_parts = filter ((== 5) . length) seqs
    Just three = find (contain one) of_five_parts
    Just five = find (`contain` nine) $ delete three of_five_parts
    two = head $ filter (`notElem` [five, three]) of_five_parts
