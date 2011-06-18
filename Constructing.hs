module Constructing where

import Types
import Tests

import qualified Data.Map as M

slot :: Vitality -> Field -> Slot
slot v f = Slot v f
defaultSlot = slot 10000 I

defaultSlotList n = zip ([0..n-1]) (replicate n defaultSlot)
defaultSlotMap  n = M.fromList $ defaultSlotList n

blankCard x | x == "I"      = I
			| x == "zero"   = Zero
			| x == "succ"   = Succ Undef
			| x == "dbl"    = Dbl Undef
			| x == "get"    = Get Undef
			| x == "put"    = Put Undef
			| x == "S"      = S Undef Undef Undef
			| x == "K"      = K Undef Undef
			| x == "inc"    = Inc Undef
			| x == "dec"    = Dec Undef
			| x == "attack" = Attack Undef Undef Undef
			| x == "help"   = Help Undef Undef Undef
			| x == "copy"   = Copy Undef
			| x == "revive" = Revive Undef
			| x == "zombie" = Zombie Undef Undef
blankCard _ = undefined  -- FIX ME: a parse error message.

initGameState = GameState (defaultSlotMap 256) (defaultSlotMap 256) Player0 1

playerSlots :: GameState -> Player -> Slots
playerSlots (GameState propSl opSl _ _) pl | pl == Player0 = propSl
										   | pl == Player1 = opSl
playerSlots _ _ = undefined

otherPlayer Player0 = Player1
otherPlayer Player1 = Player0

updateSlot i slot slots | isSlotNumberValid i slots = M.update (\x -> Just slot) i slots
						| otherwise = undefined