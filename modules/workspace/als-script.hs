import Data.Sequence
import Control.Concurrent (threadDelay)
import System.Exit (exitWith, ExitCode( ExitSuccess ))


als_file :: String
als_file = "/sys/devices/platform/asus-nb-wmi/als_enable"

luminosity_file :: String
luminosity_file = "/sys/devices/LNXSYSTM:00/LNXSYBUS:00/ACPI0008:00/iio:device0/in_illuminance_input"

max_luminosity :: Float
max_luminosity = 2000

brightness_file :: String
brightness_file = "/sys/class/backlight/intel_backlight/brightness"

max_brightness :: Float
max_brightness = 937

keyboard_file :: String
keyboard_file = "/sys/class/leds/asus::kbd_backlight/brightness"

max_keyboard :: Float
max_keyboard = 3



isAlsEnabled :: IO Bool
isAlsEnabled = (==1) <$> read <$> readFile als_file

getBrightness :: IO Float
getBrightness = (/max_brightness) <$> read <$> readFile brightness_file

setBrightness :: Float -> IO ()
setBrightness = writeFile brightness_file . show . round . (*max_brightness) . min 1

getLuminosity :: IO Float
getLuminosity = (/max_luminosity) <$> read <$> readFile luminosity_file

setKeyboardBrightness = writeFile keyboard_file . show . round . (*max_keyboard)


updateBrightness :: Seq Float -> IO ()
updateBrightness Empty = do
  brightness <- getLuminosity
  updateBrightness $ Data.Sequence.replicate 50 brightness
updateBrightness xss@(xs :|> _) = do
  let average = (sum xss / (fromIntegral $ Data.Sequence.length xss) + 0.0001) ** 0.2
  setBrightness average
  setKeyboardBrightness $ (1 - average) ** 4
  luminosity <- getLuminosity
  threadDelay 50000
  als_enabled <- isAlsEnabled
  if als_enabled
    then 
    updateBrightness $ luminosity <| xs
    else
    exitWith ExitSuccess
  
startAls :: IO ()
startAls = do
  writeFile als_file $ show 1
  updateBrightness Empty

stopAls :: IO ()
stopAls = writeFile als_file $ show 0
  
main :: IO ()
main = do
  als_enabled <- isAlsEnabled
  if als_enabled
    then
    stopAls
    else
    startAls


