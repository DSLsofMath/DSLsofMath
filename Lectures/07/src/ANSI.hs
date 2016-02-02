-- | Some helper functions to form ANSI codes suitable for terminal
--   output.
module ANSI where

data Colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Eq,Show,Enum)

ansiClearScreen :: String
ansiClearScreen = "\ESC[2J"

ansiGoto :: Int -> Int -> String
ansiGoto x y    = "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

ansiColour :: Colour -> String -> String
ansiColour c s = "\ESC[3" ++ show (fromEnum c) ++ "m" ++ s ++ "\ESC[0m"

