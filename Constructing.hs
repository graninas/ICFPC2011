module Constructing where

import Types
import Tests

import qualified Data.Map as M

appLimit :: Int
appLimit = 1000
appLimitStartValue :: Int
appLimitStartValue = 1
devaultVitality = 10000
defaultSlotCount = 256

slot :: Vitality -> Field -> Slot
slot v f = Slot v f
defaultSlot = slot devaultVitality I
--defaultSlot = slot (-1) (S ( S ( K (Inc Undef) Undef ) (K Zero Undef) Undef ) ( S ( K (Inc Undef) Undef ) (K Zero Undef) Undef ) Undef)

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
			| otherwise     = I

initGameState = GameState (defaultSlotMap defaultSlotCount) (defaultSlotMap defaultSlotCount) Player0 1

playerSlots :: GameState -> Player -> Slots
playerSlots (GameState propSl opSl _ _) pl | pl == Player0 = propSl
										   | pl == Player1 = opSl
--playerSlots _ _ = undefined

otherPlayer Player0 = Player1
otherPlayer Player1 = Player0

updateSlot i slot slots | isSlotNumberValid i slots = M.update (\x -> Just slot) i slots
						-- | otherwise = undefined

						
i = "I"
zero = "zero"
succc = "succ"
dbl  = "dbl"
get  = "get"
put  = "put"
s = "S"
k = "K"
inc = "inc"
dec = "dec"
attack = "attack"
help = "help"
copy = "copy"
revive = "revive"
zombie = "zombie"
