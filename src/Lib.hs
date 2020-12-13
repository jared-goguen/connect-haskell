module Lib
    ( showBoard
    ) where

showBoard :: IO ()
showBoard = print board

data Team = Red | Yellow | Empty
type Disc = Team
type Slot = Disc
type Row = [Slot]
type Board = [Row]

redDisc :: Disc
redDisc = Red

yellowDisc :: Disc
yellowDisc = Yellow

emptyDisc :: Disc
emptyDisc = Empty

redSlot :: Slot
redSlot = redDisc

yellowSlot :: Slot
yellowSlot = yellowDisc

emptySlot :: Slot
emptySlot = emptyDisc

board :: Board
board = [[emptySlot, emptySlot, emptySlot], [emptySlot, emptySlot, emptySlot], [emptySlot, redSlot, yellowSlot]]

instance Show Team where
    show Red = "R"
    show Yellow = "Y"
    show Empty = " "
