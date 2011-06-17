module Constructing where

import Types
import Applying
import Tests


slot :: Vitality -> Field -> Slot
slot v f = Slot v f
defaultSlot = slot 10000 (Function Id)

defaultSlotList n = zip ([0..n-1]) (replicate n defaultSlot)

blankCard x | x == "I"      = Id
			| x == "zero"   = Zero
			| x == "succ"   = Succ Id
			| x == "dbl"    = Dbl Id
			| x == "get"    = Get Id
			| x == "put"    = Put Id
			| x == "S"      = S Id Id Id
			| x == "K"      = K Id Id
			| x == "inc"    = Inc Id
			| x == "dec"    = Dec Id
			| x == "attack" = Attack Id Id Id
			| x == "help"   = Help Id Id Id
			| x == "copy"   = Copy Id
			| x == "revive" = Revive Id
			| x == "zombie" = Zombie Id Id
blankCard _ = undefined