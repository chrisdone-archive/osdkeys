{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | All types.

module OSDKeys.Types where

import Data.Sequence (Seq)
import Data.Set (Set)

-- | Key processing state.
data State =
  State {stateModifiers :: !(Set Key)
         -- ^ Perhaps on some systems order of key press matters, but
         -- this type assumes it doesn't.
        ,stateCombos :: !(Seq Combo)
         -- ^ A sequence of key combinations e.g. \"a\" \"C-f\",
         -- \"Alt-DEL\", etc.
        }

-- | A combination of some modifiers and a key.
data Combo = Combo !(Set Key) !Key
  deriving (Show)

-- | An event.
data Event
  = Press
  | Release
  deriving (Enum,Bounded,Eq,Show)

-- | Key code.
newtype KeyCode =
  KeyCode Int
  deriving (Eq,Show,Num,Ord)

-- | Device identifier.
newtype Device = Device Int
  deriving (Num)

-- | Well-typed key.
data Key
  = CtrlL
  | CtrlR
  | AltL
  | AltR
  | ShiftL
  | ShiftR
  | RET
  | SuperL
  | SuperR
  | CapsLock
  | SPC
  | F Int
  | Escape
  | Backspace
  | Insert
  | Delete
  | Home
  | Prior
  | Next
  | End
  | UpArr
  | DownArr
  | LeftArr
  | RightArr
  | PrintScreen
  | Menu
  | TAB
  | Plain Char
  | Unknown KeyCode
  deriving (Show,Ord,Eq)
