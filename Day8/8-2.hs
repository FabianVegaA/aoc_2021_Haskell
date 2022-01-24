import Data.List (find, intersect, isSubsequenceOf, nub, sort)
import Data.List.Split (splitOn)
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  file <- getArgs >>= readFile . head

  let digitalOutputs = fmap (words . (!! 1) . splitOn "|") . lines $ file

  print ""

getDictSeq :: [String] -> [(String, Int)]
getDictSeq seqs = [(one, 1), (four, 4), (seven, 7), (eigth, 8)]
  where
    Just one = find ((== 2) . length) seqs
    Just four = find ((== 4) . length) seqs
    Just seven = find ((== 3) . length) seqs
    Just eigth = find ((== 7) . length) seqs

    Just nine = find (isSubsequenceOf four) . filter ((== 6) . length) $ seqs
    Just three = find (isSubsequenceOf seven) . filter ((== 5) . length) $ seqs

    Just six = find (isSubsequenceOf (one `intersect` four)) . filter ((== 6) . length) $ seqs
    zero = nub . fmap sort . filter ((== 6) . length) $ seqs
