import System.Exit

main :: IO ()
main = do
  freeMemory <- read
                <$> (!!1) <$> words
                <$> (!!2) <$> lines
                <$> readFile "/proc/meminfo"
  putStr $ show $ round $ freeMemory / 1000
  putStrLn "MB"
  exitWith $ if freeMemory > 500000 then ExitSuccess else ExitFailure 33
