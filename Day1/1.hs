import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
  file <- getArgs >>= readFile . head
  let messures = (\l -> read l :: Int) <$> lines file
  print $ foldl didIncrease 0 $ zipWith (flip (-)) messures (tail messures)
  where
    didIncrease count delta
      | delta > 0 = count + 1
      | otherwise = count