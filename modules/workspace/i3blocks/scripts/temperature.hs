import System.Directory
import Control.Monad (filterM)
import System.Exit

temp :: String -> String
temp name = "/sys/class/thermal/" ++ name ++ "/temp"

getSymbol :: Integral n => n -> String
getSymbol t
  | t < 50 = "\57868"
  | t < 80 = "\57866"
  | otherwise = "\57867"

main :: IO ()
main = do
  thermalZones <- listDirectory "/sys/class/thermal"
  temps <- filterM (doesFileExist . temp) thermalZones
  maxTemp <- maximum <$> map (round . (/1000) . read) <$> sequence (map (readFile . temp) temps)
  putStrLn $ (getSymbol <> show) maxTemp <> "°"
  exitWith $ if maxTemp < 80 then ExitSuccess else ExitFailure 33
