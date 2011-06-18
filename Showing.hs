module Showing where

import Types
import Constructing

import Text.Printf
import qualified Data.Map as M

showTurn turn = "\n###### turn " ++ show turn

showPlayer Player0 = "\n*** player 0's turn, with slots:"
showPlayer Player1 = "\n*** player 1's turn, with slots:"

showSlots [] = "\n(slots {10000,I} are omitted)"
showSlots ((_, slot):xs) | slot == defaultSlot = showSlots xs
showSlots ((n, Slot v f):xs) = "\n" ++ show n ++ "={" ++ show v ++ "," ++ show f ++ "}" ++ showSlots xs


showCurrentPlayerSlots slots _ Player0 = showSlots $ M.toList slots
showCurrentPlayerSlots _ slots Player1 = showSlots $ M.toList slots


showGameState :: GameState -> String
showGameState (GameState slots1 slots2 curP turn) = showTurn turn ++ showPlayer curP ++ showCurrentPlayerSlots slots1 slots2 curP

showPlayerTurn :: PlayerTurn -> String
showPlayerTurn (1, slNo, card) = "\nLeftApp  (slot = " ++ show slNo ++ ", card = " ++ card
showPlayerTurn (2, slNo, card) = "\nRightApp (slot = " ++ show slNo ++ ", card = " ++ card
showPlayerTurn _ = "Unknown app type."