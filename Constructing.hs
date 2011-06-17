module Constructing where

import Types
import Applying
import Tests


slot :: Vitality -> Function -> Slot
slot v f = Slot v f
defaultSlot = slot 10000 Id

defaultSlotList n = zip ([0..n-1]) (replicate n defaultSlot)

blankCard x | x == "I"      = Id
			| x == "zero"   = Zero
			| x == "succ"   = Succ Undefined
			| x == "dbl"    = Dbl Undefined
			| x == "get"    = Get Undefined
			| x == "put"    = Put Undefined
			| x == "S"      = S Undefined Undefined Undefined
			| x == "K"      = K Undefined Undefined
			| x == "inc"    = Int Undefined
			| x == "dec"    = Dec Undefined
			| x == "attack" = Attack Undefined Undefined Undefined
			| x == "help"   = Help Undefined Undefined Undefined
			| x == "copy"   = Copy Undefined
			| x == "revive" = Revive Undefined
			| x == "zombie" = Zombie Undefined Undefined
blankCard _ = undefined