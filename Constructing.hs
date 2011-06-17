module Constructing where

import Types
import Applying
import Tests

import qualified Data.Map as M

slot :: Vitality -> Field -> Slot
slot v f = Slot v f
defaultSlot = slot 10000 (Func I)

defaultSlotList n = zip ([0..n-1]) (replicate n defaultSlot)
defaultSlotMap  n = M.fromList $defaultSlotList n

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

initGameState = GameState (defaultSlotMap 255) (defaultSlotMap 255) Player0 0