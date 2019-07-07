{-# LANGUAGE BangPatterns #-}
import Control.Concurrent (threadDelay)
import Control.Monad (mapM)
import System.Directory (listDirectory)

interface :: String
interface = "/sys/class/net/wlan0/statistics/"

path = "/sys/class/net/"

data Statistics = Statistics Float Float

instance Semigroup Statistics where
  Statistics a b <> Statistics c d = Statistics (a + c) (b + d)

instance Monoid Statistics where
  mempty = Statistics 0 0

icon i = "<span font='Material Icons 11'>" ++ i ++ "</span>"

readInterface :: FilePath -> IO Statistics
readInterface interface = do 
  rx <- read <$> readFile (path ++ interface ++ "/statistics/rx_bytes")
  tx <- read <$> readFile (path ++ interface ++ "/statistics/tx_bytes")
  seq (rx + tx) (return ())
  return $ Statistics rx tx

readInterfaces interfaces = mconcat <$> (mapM readInterface interfaces)

main :: IO ()
main = do
  interfaces <- listDirectory path
  Statistics rx tx <- readInterfaces interfaces
  threadDelay 1000000
  Statistics rx' tx' <- readInterfaces interfaces
  putStrLn
    $  (icon "\58052")
    ++ show (round $(rx' - rx)/1000) ++ "kBps "
    ++ (icon "\58054")
    ++ show (round $(tx' - tx)/1000) ++ "kBps"
