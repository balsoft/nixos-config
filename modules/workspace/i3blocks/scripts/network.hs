{-# LANGUAGE BangPatterns #-}

import Control.Concurrent (threadDelay)
import Control.Monad (mapM)
import System.Directory (listDirectory)
import Data.Bool (bool)

path :: String
path = "/sys/class/net/"

data Statistics = Statistics !Float !Float

instance Semigroup Statistics where
  Statistics a b <> Statistics c d = Statistics (a + c) (b + d)

instance Monoid Statistics where
  mempty = Statistics 0 0

icon :: String -> String
icon i = "<span font='Material Icons 11'>" ++ i ++ "</span>"

readInterface :: FilePath -> IO Statistics
readInterface interface = do 
  rx <- read <$> readFile (path ++ interface ++ "/statistics/rx_bytes")
  tx <- read <$> readFile (path ++ interface ++ "/statistics/tx_bytes")
  return $ Statistics rx tx

readInterfaces :: [FilePath] -> IO Statistics
readInterfaces interfaces = mconcat <$> (mapM readInterface interfaces)

main :: IO ()
main = do
  interfaces <- listDirectory path
  Statistics rx tx <- readInterfaces interfaces
  threadDelay 1000000
  Statistics rx' tx' <- readInterfaces interfaces
  putStrLn
    $  (bool (icon "\58052") "" (rx' - rx > 100))
    ++ (bool (icon "\58054") "" (tx' - tx > 100))
