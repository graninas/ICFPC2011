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

applyInc :: Player -> Int -> Slot -> ModifiedSlot
applyInc pl i (Slot v f) | v > 0 && v < 65535 = (pl, i, Slot (v + 1) f)
applyInc pl i (Slot v f) | otherwise          = (pl, i, Slot v       f)

apply :: GameState -> ModifiedSlot -> Function -> Function -> Either String ApplicationResult
apply gs ms (I) f = applyResult gs (modifyFunction ms f)
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
apply gs ms (Inc Undef) (Zero) = apply gs ms (Inc Undef) (FValue 0)

apply gs@(GameState _ _ pl _) ms (Inc Undef) (FValue i) =
		case M.lookup i (playerSlots gs pl) of
			Just slotToInc -> let
								 Right (newGSincremented, _)      = applyResult gs (applyInc pl i slotToInc)
								 Right (newGSidentified, modSlot) = applyResult newGSincremented (modifyFunction ms I)
							  in Right (newGSidentified, modSlot)
			Nothing        -> Left $ "Error of applying 'inc' to " ++ show i ++ ": invalid slot number."

apply _ _ f c = Left $ "Error of applying " ++ show f ++ " to " ++ show c ++ ": don't know how to apply."

-- FIX ME: final reducing of the function, may be failed.
rightApp' :: GameState -> ModifiedSlot -> Card -> Either String GameState
rightApp' gs ms@(pl, i, (Slot vit f)) card = apply gs ms f card >>= \(newGS, modifiedSlots) -> Right newGS

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