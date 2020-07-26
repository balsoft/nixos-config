{-# LANGUAGE BangPatterns, TemplateHaskell #-}

import Control.Concurrent (threadDelay)
import Control.Monad (mapM, join)
import System.Directory (listDirectory)
import Data.Bool (bool)
import Language.Haskell.TH.Syntax (liftString, runIO)
import System.Environment (getEnv)
import Control.Monad.IO.Class (liftIO)

path :: String
path = "/sys/class/net/"

data Statistics = Statistics !Float !Float

instance Semigroup Statistics where
  Statistics a b <> Statistics c d = Statistics (a + c) (b + d)

instance Monoid Statistics where
  mempty = Statistics 0 0

icon :: String -> String
icon i = "<span font='"++ $(join $ liftIO $ liftString <$> getEnv "ICONFONT") ++ "'>" ++ i ++ "</span>"

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
  putStrLn $ (icon "\58052") ++ (show $ round $ (rx' - rx) / 10^3)
          ++ (icon "\58054") ++ (show $ round $ (tx' - tx) / 10^3)
