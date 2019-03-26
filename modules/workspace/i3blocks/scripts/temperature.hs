import System.Directory
import Control.Monad (filterM)
import System.Exit

temp :: String -> String
temp name = "/sys/class/thermal/" ++ name ++ "/temp"

main :: IO ()
main = do
  thermalZones <- listDirectory "/sys/class/thermal"
  temps <- filterM (doesFileExist . temp) thermalZones
  maxTemp <- foldr max 0 <$> map (round . (/1000) . read) <$> sequence (map (readFile . temp) temps)
  putStr $ show maxTemp
  putStrLn "°"
  exitWith $ if maxTemp < 80 then ExitSuccess else ExitFailure 33
