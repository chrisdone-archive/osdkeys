-- | Various key mappings.

module OSDKeys.Mappings where

import OSDKeys.Types

import Data.Foldable (toList)
import Data.Monoid
import qualified Data.Set as S

-- | Mapping from keycodes to something for humans to read.
codeMapping :: [(KeyCode, Key)]
codeMapping =
  [(50,ShiftL)
  ,(62,ShiftR)
  ,(37,CtrlL)
  ,(105,CtrlR)
  ,(64,AltL)
  ,(108,AltR)
  ,(133,SuperL)
  ,(134,SuperR)
  ,(24,Plain 'q')
  ,(25,Plain 'w')
  ,(26,Plain 'e')
  ,(27,Plain 'r')
  ,(28,Plain 't')
  ,(29,Plain 'y')
  ,(30,Plain 'u')
  ,(31,Plain 'i')
  ,(32,Plain 'o')
  ,(33,Plain 'p')
  ,(34,Plain '[')
  ,(35,Plain ']')
  ,(51,Plain '\\')
  ,(38,Plain 'a')
  ,(39,Plain 's')
  ,(40,Plain 'd')
  ,(41,Plain 'f')
  ,(42,Plain 'g')
  ,(43,Plain 'h')
  ,(44,Plain 'j')
  ,(45,Plain 'k')
  ,(46,Plain 'l')
  ,(47,Plain ';')
  ,(48,Plain '\'')
  ,(36,RET)
  ,(52,Plain 'z')
  ,(53,Plain 'x')
  ,(54,Plain 'c')
  ,(55,Plain 'v')
  ,(56,Plain 'b')
  ,(57,Plain 'n')
  ,(58,Plain 'm')
  ,(59,Plain ',')
  ,(60,Plain '.')
  ,(61,Plain '/')
  ,(66,CapsLock)
  ,(49,Plain '`')
  ,(10,Plain '1')
  ,(11,Plain '2')
  ,(12,Plain '3')
  ,(13,Plain '4')
  ,(14,Plain '5')
  ,(15,Plain '6')
  ,(16,Plain '7')
  ,(17,Plain '8')
  ,(18,Plain '9')
  ,(19,Plain '0')
  ,(20,Plain '-')
  ,(21,Plain '=')
  ,(65,SPC)
  ,(67,F 1)
  ,(68,F 2)
  ,(69,F 3)
  ,(70,F 4)
  ,(71,F 5)
  ,(72,F 6)
  ,(73,F 7)
  ,(74,F 8)
  ,(75,F 9)
  ,(76,F 10)
  ,(95,F 11)
  ,(96,F 12)
  ,(9,Escape)
  ,(22,Backspace)
  ,(118,Insert)
  ,(119,Delete)
  ,(110,Home)
  ,(112,Prior)
  ,(117,Next)
  ,(115,End)
  ,(111,UpArr)
  ,(116,DownArr)
  ,(113,LeftArr)
  ,(114,RightArr)
  ,(135,Menu)
  ,(107,PrintScreen)
  ,(23,TAB)]

-- | Mapping for shift keys.
shiftMapping :: [(Char,Char)]
shiftMapping =
  [('a','A')
  ,('b','B')
  ,('c','C')
  ,('d','D')
  ,('e','E')
  ,('f','F')
  ,('g','G')
  ,('h','H')
  ,('i','I')
  ,('j','J')
  ,('k','K')
  ,('l','L')
  ,('m','M')
  ,('n','N')
  ,('o','O')
  ,('p','P')
  ,('q','Q')
  ,('r','R')
  ,('s','S')
  ,('t','T')
  ,('u','U')
  ,('v','V')
  ,('w','W')
  ,('x','X')
  ,('y','Y')
  ,('z','Z')
  ,(',','<')
  ,('.','>')
  ,('/','?')
  ,(';',':')
  ,('\'','"')
  ,('\\','|')
  ,(']','}')
  ,('[','{')
  ,('=','+')
  ,('-','_')
  ,('0',')')
  ,('9','(')
  ,('8','*')
  ,('7','&')
  ,('6','^')
  ,('5','%')
  ,('4','$')
  ,('3','#')
  ,('2','@')
  ,('1','!')
  ,('`','~')]

-- | Render a list of combos in Emacs-style notation.
showEmacsCombos :: [Combo] -> String
showEmacsCombos = unwords . words . go
  where go (x@(Combo mods key):xs)
          | not (S.null (S.filter (not . flip elem shiftKeys) mods)) ||
              special key = " " <> showComboEmacs x <> " " <> go xs
        go (x:xs) = showComboEmacs x <> go xs
        go [] = mempty

-- | Is the key special in some way?
special :: Key -> Bool
special (Plain{}) = False
special _ = True

-- | Show a key combination in Emacs form. Handles display shifted
-- keys properly.
showComboEmacs :: Combo -> String
showComboEmacs (Combo mods key) =
  concat (map showKeyEmacs
              (if any (flip S.member mods) shiftKeys
                  then case key of
                         Plain c ->
                           case lookup c shiftMapping of
                             Nothing -> normal
                             Just shifted ->
                               toList (foldl (\s k ->
                                                S.delete k s)
                                             mods
                                             shiftKeys) <>
                               [Plain shifted]
                         _ -> normal
                  else normal))
  where normal =
          toList mods <>
          [key]

-- | Shift keys.
shiftKeys :: [Key]
shiftKeys = [ShiftL,ShiftR]

-- | Modifier keys.
modifierKeys :: [Key]
modifierKeys =
  [CtrlL,CtrlR,AltL,AltR,SuperL,SuperR,ShiftL,ShiftR]

-- | Show a key for Emacs.
showKeyEmacs :: Key -> String
showKeyEmacs k =
  case k of
    CtrlL -> "C-"
    CtrlR -> "C-"
    AltL -> "M-"
    AltR -> "M-"
    Plain c ->
      if c == '<'
         then "< " -- Because notify-osd does not follow its
               -- own rules properly.
         else [c]
    ShiftL -> "S-"
    ShiftR -> "S-"
    RET -> "RET"
    SuperL -> "s-"
    SuperR -> "s-"
    SPC -> "SPC"
    F i -> "<f" <> show i <> ">"
    Escape -> "<escape>"
    CapsLock -> "<capslock>"
    Backspace -> "DEL"
    Insert -> "<insert>"
    Delete -> "<delete>"
    Home -> "<home>"
    Prior -> "<prior>"
    Next -> "<next>"
    End -> "<end>"
    UpArr -> "<up>"
    DownArr -> "<down>"
    LeftArr -> "<left>"
    RightArr -> "<right>"
    Menu -> "<menu>"
    PrintScreen -> "<printscreen>"
    TAB -> "TAB"
    Unknown (KeyCode i) -> "?" <> show i
