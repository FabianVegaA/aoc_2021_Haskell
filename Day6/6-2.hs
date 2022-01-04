import Data.List (group, sort)
import Data.List.Split (splitOn)
import System.Directory.Internal.Prelude (getArgs)

iterations :: Int
iterations = 1024

main :: IO ()
main = do
  file <- getArgs >>= readFile . head

  let input = fmap (\xs -> (head xs, length xs)) . group . sort $ read <$> splitOn "," file

  print $ sum . valuesAL . last . take (succ iterations) . iterate nextState $ input

nextState :: [(Int, Int)] -> [(Int, Int)]
nextState l
  | hasKeyAL l 0 = addToAL (addToAL nextIter 6 o) 8 $ findValuesAL l 0
  | otherwise = nextIter
  where
    next :: (Int, b) -> (Int, b)
    next (x, o) = case x of
      0 -> (6, o)
      _ -> (pred x, o)

    nextIter = fmap next l
    (_, o) =
      foldl1
        (\al1 al2 -> (fst al1, snd al1 + snd al2))
        $ filter ((== 6) . fst) nextIter

addToAL :: (Eq a) => [(a, b)] -> a -> b -> [(a, b)]
addToAL l key value = (key, value) : delFromAL l key

delFromAL :: (Eq a) => [(a, b)] -> a -> [(a, b)]
delFromAL l key = filter (\a -> fst a /= key) l

hasKeyAL :: (Eq a) => [(a, b)] -> a -> Bool
hasKeyAL l key = elem key $ map fst l

findValuesAL :: (Eq a) => [(a, b)] -> a -> b
findValuesAL l key = snd . head . filter ((== key) . fst) $ l

valuesAL :: [(a, b)] -> [b]
valuesAL = fmap snd
