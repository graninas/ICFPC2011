module Applying where

import qualified Data.Map as M
import Text.Printf

import Types
import Tests
import Constructing

applyResult :: GameState -> ModifiedSlot -> Either String ApplicationResult
applyResult gs@(GameState slots _ _ _) ms@(Player0, i, sl) = Right $ (gs {propSlots = updateSlot i sl slots}, ms)
applyResult gs@(GameState _ slots _ _) ms@(Player1, i, sl) = Right $ (gs {opSlots   = updateSlot i sl slots}, ms)

modifyFunction :: ModifiedSlot -> Function -> ModifiedSlot
modifyFunction (pl, i, (Slot v _)) newF = (pl, i, (Slot v newF))

-- Может случиться, что функциями inc, attack и подобными мы изменили vitality слота, где выполняем функцию. Это необходимо учесть.
modifyMaybeThisSlot :: ModifiedSlot -> ModifiedSlot -> Function -> ModifiedSlot
modifyMaybeThisSlot mbThisSl@(pl1, i1, (Slot v1 _)) realSl@(pl, i, (Slot v _)) newF
		| i1 == i   = (pl, i, (Slot v1 newF))
		| otherwise = (pl, i, (Slot v  newF))

applyInc :: Player -> Int -> Slot -> ModifiedSlot
applyInc pl i (Slot v f) | v > 0 && v < 65535 = (pl, i, Slot (v + 1) f)
applyInc pl i sl@(Slot v f) | otherwise          = (pl, i, sl)

applyDec :: Player -> Int -> Slot -> ModifiedSlot
applyDec pl i (Slot v f) | v > 0     = (pl, i, Slot (v - 1) f)
applyDec pl i sl@(Slot v f) | otherwise = (pl, i, sl)

applyAttack :: Player -> Int -> Slot -> Int -> ModifiedSlot
applyAttack pl j sl@(Slot v f) n | isSlotAlive sl =
		let divided = div (n * 9) 10 in
			if v - divided < 0 then (pl, j, Slot 0 f)
							   else (pl, j, Slot (v - divided) f)
	
applyKickBack :: Player -> Int -> Slot -> Int -> ModifiedSlot	
applyKickBack pl i sl@(Slot v f) n | n <= v = (pl, i, Slot (v - n) f)
								   | otherwise = undefined            -- Probably it cannot to be.

applyHelp   pl j sl@(Slot v f) n | isSlotAlive sl =
		let divided = div (n * 11) 10 in
			if v + divided > 65536 then (pl, j, Slot 65536 f)
							   else (pl, j, Slot (v + divided) f)
applyDevote pl i sl@(Slot v f) n | n <= v = (pl, i, Slot (v - n) f)
								 | otherwise = undefined              -- Probably it cannot to be.

applyRevive pl i sl@(Slot v f) | v < 0     = (pl, i, Slot 1 f)
							   | otherwise = (pl, i, sl)

applyZombie pl i sl f | not $ isSlotAlive $ sl = (pl, i, Slot (-1) f)
					  | otherwise = (pl, i, sl)
							   
apply :: GameState -> ModifiedSlot -> Function -> Function -> Either String ApplicationResult
apply gs ms (I)          f      = applyResult gs (modifyFunction ms f)
apply gs ms (Succ Undef) (Zero) = applyResult gs (modifyFunction ms (FValue 1))
apply gs ms (Succ Undef) (FValue n)  | n < 65535 = applyResult gs (modifyFunction ms (FValue $ n+1))
							        | otherwise  = applyResult gs (modifyFunction ms (FValue 65535))

apply gs ms (Dbl Undef) (Zero)     = applyResult gs (modifyFunction ms (FValue 0))
apply gs ms (Dbl Undef) (FValue n)  | n < 32768 = applyResult gs (modifyFunction ms (FValue (n * 2)))
							        | otherwise = applyResult gs (modifyFunction ms (FValue 65535))

apply gs ms (Get Undef) (Zero) = apply gs ms (Get Undef) (FValue 0)
apply gs ms (Get Undef) (FValue i) =
		case M.lookup i (propSlots gs) of
			Just slot -> case isSlotAlive slot of
								True  -> case slot of
									(Slot v Zero) -> applyResult gs (modifyFunction ms Zero)
									(Slot v val@(FValue _)) -> applyResult gs (modifyFunction ms val)
									_ -> Left $ "Error of applying 'get' to " ++ show i ++ ": slot " ++ show i ++ " not a value."
								False -> Left $ "Error of applying 'get' to " ++ show i ++ ": slot " ++ show i ++ " is dead."
			Nothing -> Left $ "Error of applying 'get' to " ++ show i ++ ": invalid slot number."

apply gs ms (Put Undef) _     = applyResult gs (modifyFunction ms I)

apply gs ms (S Undef g x) arg = applyResult gs (modifyFunction ms (S arg g x))
apply gs ms (S f Undef x) arg = applyResult gs (modifyFunction ms (S f arg x))
apply gs ms (S f g Undef) arg = let
									Right (newGSh, modSlot1@(_, _, (Slot _ h))) = apply gs     ms       f arg				-- Maybe error.
									Right (newGSy, modSlot2@(_, _, (Slot _ y))) = apply newGSh modSlot1 g arg
									Right (newGSz, modSlot3@(_, _, (Slot _ z))) = apply newGSy modSlot2 h y
								in  Right (newGSz, modSlot3)

apply gs ms (K Undef y) arg    = applyResult gs (modifyFunction ms (K arg y))
apply gs ms (K x Undef) _      = applyResult gs (modifyFunction ms x)

-- inc app
apply gs                      ms (Inc Undef) (Zero)     = apply gs ms (Inc Undef) (FValue 0)
apply gs@(GameState _ _ pl _) ms (Inc Undef) (FValue i) =
		case M.lookup i (playerSlots gs pl) of
			Just slotToInc -> let
								 Right (newGSincremented, incrModSlot) = applyResult gs (applyInc pl i slotToInc)
								 Right (newGSidentified, modSlot)      = applyResult newGSincremented (modifyMaybeThisSlot incrModSlot ms I)
							  in Right (newGSidentified, modSlot)
			Nothing        -> Left $ "Error of applying 'inc' to " ++ show i ++ ": invalid slot number."

-- dec app
apply gs                      ms (Dec Undef) (Zero)     = apply gs ms (Dec Undef) (FValue 0)
apply gs@(GameState _ _ pl _) ms (Dec Undef) (FValue i) =
		let otherP = otherPlayer pl in
		case M.lookup (255 - i) (playerSlots gs otherP) of
			Just slotToDec -> let
								 Right (newGSdecremented, decrModSlot) = applyResult gs (applyDec otherP i slotToDec)
								 Right (newGSidentified, modSlot)      = applyResult newGSdecremented (modifyMaybeThisSlot decrModSlot ms I)
							  in Right (newGSidentified, modSlot)
			Nothing        -> Left $ "Error of applying 'dec' to " ++ show i ++ ": invalid slot number."

-- attack
apply gs ms (Attack Undef j n) (Zero)         = applyResult gs (modifyFunction ms (Attack (FValue 0) j n))
apply gs ms (Attack Undef j n) val@(FValue _) = applyResult gs (modifyFunction ms (Attack val j n))
apply gs ms (Attack i Undef n) (Zero)         = applyResult gs (modifyFunction ms (Attack i (FValue 0) n))
apply gs ms (Attack i Undef n) val@(FValue _) = applyResult gs (modifyFunction ms (Attack i val n))
apply gs ms f@(Attack i j Undef) (Zero)       = apply gs ms f (FValue 0)

apply gs@(GameState _ _ pl _) ms   (Attack (FValue i) (FValue j) Undef) (FValue n) =
		let otherP = otherPlayer pl in
		case M.lookup (255 - j) (playerSlots gs otherP) of
			Just slotToAttack ->
				case M.lookup i (playerSlots gs pl) of
					Just slotToKickBack@(Slot v _) ->
						case n <= v of
							True -> let
										 Right (newGSattacked, _)              = applyResult gs              (applyAttack   otherP j slotToAttack   n)
										 Right (newGSkickedBack, kickedBackMS) = applyResult newGSattacked   (applyKickBack pl     i slotToKickBack n)
										 Right (newGSidentified, modSlot)      = applyResult newGSkickedBack (modifyMaybeThisSlot kickedBackMS ms I)
									in Right (newGSidentified, modSlot)
							False -> Left $ "Error of applying 'attack' to " ++ show i ++ ": n > v."
					Nothing -> Left $ "Error of applying 'attack' to " ++ show i ++ ": invalid proponent slot number."
			Nothing -> Left $ "Error of applying 'attack' to " ++ show j ++ ": invalid opponent slot number."

-- help
apply gs ms (Help Undef j n) (Zero)         = applyResult gs (modifyFunction ms (Help (FValue 0) j n))
apply gs ms (Help Undef j n) val@(FValue _) = applyResult gs (modifyFunction ms (Help val j n))
apply gs ms (Help i Undef n) (Zero)         = applyResult gs (modifyFunction ms (Help i (FValue 0) n))
apply gs ms (Help i Undef n) val@(FValue _) = applyResult gs (modifyFunction ms (Help i val n))

apply gs@(GameState _ _ pl _) ms f@(Help i j Undef) (Zero)   = apply gs ms f (FValue 0)
apply gs@(GameState _ _ pl _) ms   (Help (FValue i) (FValue j) Undef) (FValue n) =
		let plSlots = playerSlots gs pl in
		case M.lookup i plSlots of
			Just slotToDevote@(Slot v _) ->
				case M.lookup j plSlots of
					Just slotToHelp@(Slot helpV helpF) ->
						case n <= v of
							True -> let
										 Right (newGSdevoted, devotedMS)  = applyResult gs           (applyDevote pl i slotToDevote n)
										 Right (newGShelped, helpedMS)    = applyResult newGSdevoted (modifyMaybeThisSlot devotedMS (applyHelp pl j slotToHelp n) helpF)
										 Right (newGSidentified, modSlot) = applyResult newGShelped  (modifyMaybeThisSlot helpedMS ms I)
									in Right (newGSidentified, modSlot)
							False -> Left $ "Error of applying 'help' to " ++ show i ++ ": n > v."
					Nothing -> Left $ "Error of applying 'help' to " ++ show j ++ ": invalid proponent slot number."
			Nothing -> Left $ "Error of applying 'help' to " ++ show i ++ ": invalid proponent slot number."

-- copy
apply gs                      ms (Copy Undef) (Zero)     = apply gs ms (Copy Undef) (FValue 0)
apply gs@(GameState _ _ pl _) ms (Copy Undef) (FValue i) =
		let otherPlSlots = playerSlots gs (otherPlayer pl) in
		case M.lookup i otherPlSlots of
			Just (Slot _ f) -> applyResult gs (modifyFunction ms f)
			Nothing -> Left $ "Error of applying 'copy' to " ++ show i ++ ": invalid opponent slot number."
			
-- revive
apply gs                      ms (Revive Undef) (Zero)     = apply gs ms (Revive Undef) (FValue 0)
apply gs@(GameState _ _ pl _) ms (Revive Undef) (FValue i) =
		let plSlots = playerSlots gs pl in
		case M.lookup i plSlots of
			Just slotToRevive@(Slot _ f) ->
								let
									Right (newGSrevived, revivedMS)  = applyResult gs           (applyRevive pl i slotToRevive)
									Right (newGSidentified, modSlot) = applyResult newGSrevived (modifyMaybeThisSlot revivedMS ms I)
								in  Right (newGSidentified, modSlot)
			Nothing -> Left $ "Error of applying 'revive' to " ++ show i ++ ": invalid proponent slot number."

apply gs ms (Zombie Undef x) (Zero)     = applyResult gs (modifyFunction ms (Zombie (FValue 0) x))
apply gs ms (Zombie Undef x) val@(FValue i) = applyResult gs (modifyFunction ms (Zombie val x))
apply gs@(GameState _ _ pl _) ms (Zombie (FValue i) Undef) f = 
		let
			otherP = otherPlayer pl
			otherPlSlots = playerSlots gs otherP
		in
		case M.lookup (255 - i) otherPlSlots of
			Just slotToZombie@(Slot _ f) ->
								let
									Right (newGSzombied, zombiedMS)  = applyResult gs (applyZombie otherP i slotToZombie f)
									Right (newGSidentified, modSlot) = applyResult newGSzombied (modifyFunction ms I)
								in  Right (newGSidentified, modSlot)
			Nothing -> Left $ "Error of applying 'revive' to " ++ show i ++ ": invalid proponent slot number."

apply _ _ f c = Left $ "Error of applying " ++ show f ++ " to " ++ show c ++ ": don't know how to apply."




-- FIX ME: final reducing of the function, may be failed.
rightApp' :: GameState -> ModifiedSlot -> Card -> Either String GameState
rightApp' gs ms@(pl, i, (Slot vit f)) card = apply gs ms f card >>= \(newGS, modifiedSlots) -> Right newGS

-- FIX ME: function to I if some error.
rightApp :: GameState -> ModifiedSlot -> Card -> Either String GameState
rightApp gs ms@(pl, i, slot) card | isSlotAlive slot && isFunctionField slot = rightApp' gs ms card
rightApp _ _ _ = Left "rightApp failed."

{-
			| S FieldFunction FieldFunction FieldFunction
			| K FieldFunction FieldFunction
			| Inc FieldFunction
			| Dec FieldFunction
			| Attack FieldFunction FieldFunction FieldFunction
			| Help FieldFunction FieldFunction FieldFunction
			| Copy FieldFunction
			| Revive FieldFunction
			| Zombie FieldFunction FieldFunction
			| FValue Int
			
updateSlots
{propSlots = updateSlot i (Slot v I) (updateSlot spI spSlot slots)}  -- We need to update 2 slots if we have functions like 'inc'.

applyResult :: GameState -> Slots -> Int -> Slot -> Function -> Either String GameState

applyResult gs(GameState slots _ curP _) curPSlots i (Slot v _) (SpecFuncSaveSlot Player0 spI spSlot) =
				Right $ updateSlots curP curPSlots 
				
applyResult gs(GameState _ slots _ _) i (Slot v _) (SpecFuncSaveSlot Player1 spI spSlot) =
				Right $ gs {opSlots   = updateSlot i (Slot v I) (updateSlot spI spSlot slots)}  -- We need to update 2 slots if we have functions like 'inc'.
				
applyResult gs(GameState slots _ Player0 _) i sl func = Right $ gs {}
-}