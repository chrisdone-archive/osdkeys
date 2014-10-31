-- | Main entry point to osdkeys.
--
-- Show keys pressed with an on-screen display (Linux only)

module Main where

import Data.Maybe
import OSDKeys
import OSDKeys.Types
import System.Process

import System.Environment
import Text.Read

-- | Main entry point.
main :: IO ()
main =
  do args <- getArgs
     case args of
       [mdevice] -> run mdevice Nothing
       [mdevice,mmax] -> run mdevice (Just mmax)
       _ -> error "Arguments: DEVICE-ID [<max-keys-on-screen>]\n\n\
                  \Use `xinput list' to get device ID."

-- | Run on the device and with the given max.
run :: String -> Maybe String -> IO ()
run mdevice mmax =
  case readMaybe mdevice of
    Nothing ->
      do xinputOutput <- readProcess "xinput"
                                     ["list"]
                                     ""
         error ("Need a device id. Here are the current devices: \n\n" ++
                xinputOutput)
    Just device ->
      startOSDKeys (Device device)
                   (fromMaybe 64 (mmax >>= readMaybe))
