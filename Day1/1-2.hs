import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  file <- getArgs >>= readFile . head
  let messures = (\l -> read l :: Int) <$> lines file
  let sumMesures = zipWith3 (\a b c -> a + b + c) messures (tail messures) (tail $ tail messures)
  print $ foldl didIncrease 0 $ zipWith (flip (-)) sumMesures (tail messures)
  where
    didIncrease count delta
      | delta > 0 = count + 1
      | otherwise = count