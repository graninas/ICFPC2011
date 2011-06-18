module Showing where

import Types
import Constructing

import Text.Printf
import qualified Data.Map as M

showTurn turn = "\n###### turn " ++ show turn

showPlayer Player0 = "\n*** player 0's turn, with slots:"
showPlayer Player1 = "\n*** player 1's turn, with slots:"

showFunc :: Function -> String
showFunc  I = "I"
showFunc  (Zero) = "zero"
showFunc  (Succ f) = "succ(" ++ showFunc f ++ ")"
showFunc  (Dbl  f)  = "dbl(" ++ showFunc f ++ ")"
showFunc  (Get f) = "get(" ++ showFunc f ++ ")"
showFunc  (Put f) = "put(" ++ showFunc f ++ ")"
showFunc  (S f1 f2 f3) = "S(" ++ showFunc f1 ++ ")(" ++ showFunc f2 ++ ")(" ++ showFunc f3 ++ ")"
showFunc  (K f1 f2) = "K(" ++ showFunc f1 ++ ")(" ++ showFunc f2 ++ ")"
showFunc  (Inc f) = "inc(" ++ showFunc f ++ ")"
showFunc  (Dec f) = "dec(" ++ showFunc f ++ ")"
showFunc  (Attack f1 f2 f3) = "attack(" ++ showFunc f1 ++ ")(" ++ showFunc f2 ++ ")(" ++ showFunc f3 ++ ")"
showFunc  (Help f1 f2 f3) = "help(" ++ showFunc f1 ++ ")(" ++ showFunc f2 ++ ")(" ++ showFunc f3 ++ ")"
showFunc  (Copy f)   = "copy(" ++ showFunc f ++ ")"
showFunc  (Revive f) = "revive(" ++ showFunc f ++ ")"
showFunc  (Zombie f1 f2) = "zombie(" ++ showFunc f1 ++ ")(" ++ showFunc f2 ++ ")"
showFunc  (FValue 0) = "zero"
showFunc  (FValue v) = show v
showFunc  _ = []

showSlots [] = "\n(slots {10000,I} are omitted)"
showSlots ((_, slot):xs) | slot == defaultSlot = showSlots xs
showSlots ((n, Slot v f):xs) = "\n" ++ show n ++ "={" ++ show v ++ "," ++ showFunc f ++ "}" ++ showSlots xs


showCurrentPlayerSlots slots _ Player0 = showSlots $ M.toList slots
showCurrentPlayerSlots _ slots Player1 = showSlots $ M.toList slots


showGameState :: GameState -> String
showGameState (GameState slots1 slots2 curP turn) = showTurn turn ++ showPlayer curP ++ showCurrentPlayerSlots slots1 slots2 curP

showPlayerTurn :: PlayerTurn -> String
showPlayerTurn (appT, slNo, card) = "\n" ++ show appT ++ ": (slot = " ++ show slNo ++ ", card = " ++ card ++ ")"