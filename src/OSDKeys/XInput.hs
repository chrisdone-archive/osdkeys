{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- | A conduit interface to XInput events.

module OSDKeys.XInput
  (xinputSource)
  where

import           OSDKeys.Types

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import           Data.Monoid
import           System.Exit

-- | Source of xinput keys.
xinputSource :: MonadIO m
             => Device -> ConduitM i (Event,KeyCode) m ExitCode
xinputSource (Device device) =
  do (exitCode,()) <- sourceCmdWithConsumer
                        ("unbuffer xinput test " <> show device)
                        (CB.lines $= CL.mapMaybe parse $=
                         awaitForever (lift . yield))
     return exitCode

-- | Parse an xinput test line.
parse :: ByteString -> Maybe (Event,KeyCode)
parse line =
  case S8.words (S8.drop 4 line) of
    [mode,S8.readInt -> Just (code,_)] ->
      return (if mode == "release"
                 then Release
                 else Press
             ,KeyCode code)
    _ -> Nothing
