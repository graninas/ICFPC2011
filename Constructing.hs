module Constructing where

import Types
import Cards
import Tests

slot :: Vitality -> FieldValue -> Slot
slot v f | isVitalityValid v && isFieldValueValid f = Slot v f
		 | otherwise = undefined

defaultSlot = slot 10000 cI

initSlots n = zip ([0..n-1]) (replicate n defaultSlot)



----------------------------------------------------------------------

slot' :: Vitality -> Function -> Slot'
slot' v f = Slot' v f
defaultSlot' = slot' 10000 Id

getBlankFunction' x | x == "I"      = Id
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
getBlankFunction _ = undefined