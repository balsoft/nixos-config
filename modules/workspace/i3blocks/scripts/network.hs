import Control.Concurrent (threadDelay)

interface :: String
interface = "/sys/class/net/wlan0/statistics/"

icon i = "<span font='Material Icons 11'>" ++ i ++ "</span>"

main :: IO ()
main = do
  rx <- read <$> readFile (interface ++ "/rx_bytes")
  tx <- read <$> readFile (interface ++ "/tx_bytes")
  seq (rx + tx) (return ())
  threadDelay 1000000
  rx' <- read <$> readFile (interface ++ "/rx_bytes")
  tx' <- read <$> readFile (interface ++ "/tx_bytes")
  putStrLn $ (icon "\58052") ++ show  (round $(rx' - rx)/1000) ++ "kBps " ++ (icon "\58054") ++ show (round $(tx' - tx)/1000) ++ "kBps"
