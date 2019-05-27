import System.Directory
import System.FilePath
import Control.Monad (filterM, forM)
import System.Posix.Files
import Data.List (isPrefixOf, isInfixOf)
import System.Exit


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


-- | Get temperatures from hardware sensors in /sys/class/hwmon
getTemps :: IO [Int]
getTemps = do
  hwmons <- traverseDir "/sys/class/hwmon"
    (
      \name
      -> ("hwmon" `isPrefixOf` name)
      || ("temp" `isInfixOf` name) && ("input" `isInfixOf` name)
    )
  fmap (round .(/1000) . read) <$> traverse (readFile) hwmons
  
-- | Get a symbol corresponding to the temperature
getSymbol :: Integral n => n -> String
getSymbol t
  | t < 50    = "\57868" -- 
  | t < 80    = "\57866" -- 
  | otherwise = "\57867" -- 

main :: IO ()
main = do
  maxTemp <- maximum <$> getTemps
  putStrLn $ (getSymbol <> show) maxTemp <> "°"
  exitWith $ if maxTemp < 80 then ExitSuccess else ExitFailure 33
