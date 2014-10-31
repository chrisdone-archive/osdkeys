{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

-- | Show keys pressed with an on-screen display (Linux only).

module OSDKeys (startOSDKeys) where

import           OSDKeys.Mappings
import           OSDKeys.Types
import           OSDKeys.XInput

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Foldable (toList)
import           Data.Maybe
import           Data.Sequence ((|>))
import qualified Data.Sequence as Q
import qualified Data.Set as S
import           Libnotify

-- | Main entry point.
startOSDKeys :: Device -> Int -> IO ()
startOSDKeys d maxCombos =
  do token <- display (summary "Keys pressed" <>
                       body "Started!")
     void (runResourceT
             (void (xinputSource d) $$
              CL.foldM (consume token)
                       (State mempty mempty)))
  where consume token state event =
          liftIO (do let !newState =
                           update state maxCombos event
                     display_ (reuse token <>
                               body (encodeNotify (showEmacsCombos (toList (stateCombos newState)))))
                     return newState)

-- | Update the state with the new key event.
update :: State -> Int -> (Event,KeyCode) -> State
update state@(State modifiers combos) maxCombos (event,code) =
  if elem key modifierKeys
     then state {stateModifiers =
                   case event of
                     Press ->
                       S.insert key modifiers
                     Release ->
                       S.delete key modifiers}
     else case event of
            Press ->
              state {stateCombos =
                       limit (combos |>
                              Combo modifiers key)}
            Release -> state
  where key =
          fromMaybe (Unknown code)
                    (lookup code codeMapping)
        limit s =
          if Q.length s > maxCombos
             then Q.drop 1 s
             else s

-- | Encode some string for notify.
encodeNotify :: String -> String
encodeNotify = go
  where go (x:xs)
          | Just rep <- lookup x encodingMap = rep ++ go xs
          | otherwise = x : go xs
        go [] = []

-- | Pseudo-HTML mapping for for notify.
encodingMap :: [(Char,String)]
encodingMap =
  [('&',"&amp;")
  ,('<',"&lt;")
  ,('>',"&gt;")
  ,('\'',"&apos;")
  ,('"',"&quot;")]
