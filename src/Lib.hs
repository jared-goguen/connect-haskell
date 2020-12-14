module Lib
    ( showBoard
    ) where

import Data.List

showBoard :: IO ()
showBoard = print board

data Team = Red | Yellow
type Disc = Team
data Slot = Slot (Maybe Disc)
data Row = Row [Slot]
data Board = Board [Row] deriving (Show)

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

slots :: [Row]
slots = [Row [emptySlot, emptySlot, emptySlot], Row [emptySlot, emptySlot, emptySlot], Row [emptySlot, redSlot, yellowSlot]]

board :: Board
board = Board slots

instance Show Team where
    show Red = "R"
    show Yellow = "Y"

instance Show Slot where
    show (Slot Nothing) = "   "
    show (Slot (Just disc)) = " " ++ show disc ++ " "

instance Show Row where
    show (Row row) = "|" ++ unwords (map show row) ++ "|"
