{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import Prelude hiding (readFile)
import System.Directory
import System.FilePath
import Control.Monad (forM, join)
import System.Posix.Files
import Data.List (isPrefixOf, isInfixOf)
import System.Exit
import Language.Haskell.TH.Syntax (liftString, runIO)
import System.Environment (getEnv)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Control.Exception
import Data.Text (unpack)
import Data.Text.IO (readFile)

-- | Traverse from 'top' directory and return all the files by
-- filtering with 'include' predicate.
traverseDir :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
traverseDir top include = do
  ds <- getDirectoryContents top
  paths <- forM (filter include ds) $ \d -> do
    let path = top </> d
    s <- getFileStatus path
    if isDirectory s
      then traverseDir path include
      else return [path]
  return $ concat paths



-- | Get temperatures from hardware sensors in </sys/class/hwmon>
getTemps :: IO [Int]
getTemps = do
  hwmons <- traverseDir "/sys/class/hwmon"
    (
      \name
      -> ("hwmon" `isPrefixOf` name)
      || ("temp" `isInfixOf` name) && ("input" `isInfixOf` name)
    )
  fmap (round . (/1000) . read . unpack) <$> traverse (handle (\e -> do pure (e :: IOException); pure "0") . readFile) hwmons
  
-- | Get a symbol corresponding to the temperature
getSymbol :: Integral n => n -> String
getSymbol t
  | t < 50    = "\57868" -- 
  | t < 80    = "\57866" -- 
  | otherwise = "\57867" -- 

icon :: String -> String
icon s = "<span font='"++ $(join $ liftIO $ liftString <$> getEnv "ICONFONT") ++ "'>" ++ s ++ "</span>"

main :: IO ()
main = do
  maxTemp <- maximum <$> getTemps
  putStrLn $ (getSymbol <> show) maxTemp <> "°"
  exitWith $ if maxTemp < 80 then ExitSuccess else ExitFailure 33
