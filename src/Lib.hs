module Lib
    ( showBoard
    ) where

import Data.List

showBoard :: IO ()
showBoard = print board

data Team = Red | Yellow
type Disc = Team
data Slot = Slot (Maybe Disc)
data Board = Board {
    slots :: [[Slot]],
    height :: Int,
    width :: Int
}

redDisc :: Disc
redDisc = Red

yellowDisc :: Disc
yellowDisc = Yellow

redSlot :: Slot
redSlot = Slot (Just redDisc)

yellowSlot :: Slot
yellowSlot = Slot (Just yellowDisc)

emptySlot :: Slot
emptySlot = Slot Nothing

s :: [[Slot]]
s = [ [emptySlot, emptySlot, emptySlot], [emptySlot, emptySlot, emptySlot], [emptySlot, redSlot, yellowSlot]]

board :: Board
board = Board {
    slots = s
}

instance Show Team where
    show Red = "R"
    show Yellow = "Y"

instance Show Slot where
    show (Slot Nothing) = " "
    show (Slot (Just disc)) = show disc

blankLine :: Int -> String
blankLine n = "|" ++ intercalate "|" (replicate n "---") ++ "|"

rowLine :: [Slot] -> String
rowLine s = "| " ++ intercalate " | " (map show slots) ++ " |\n" ++ blankLine (length slots)

instance Show Board where
    show (Board rows _ _) = blankLine 3 ++ "\n" ++ intercalate "\n" (map show rows)









