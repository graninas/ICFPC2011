module Applying where

import qualified Data.Map as M
import Text.Printf

import Types
import Tests
import Constructing

applyResult :: GameState -> ModifiedSlot -> Either String GameState
applyResult gs@(GameState slots _ _ _) (Player0, i, sl) = Right $ gs {propSlots = updateSlot i sl slots} 
applyResult gs@(GameState _ slots _ _) (Player1, i, sl) = Right $ gs {opSlots   = updateSlot i sl slots}

applyResults :: GameState -> ModifiedSlots -> Either String GameState
applyResults gs [] = Right gs
applyResults gs (ms:mss) = applyResult gs ms >>= \x -> applyResults x mss


modifyFunction :: ModifiedSlot -> Function -> ModifiedSlot
modifyFunction (pl, i, (Slot v oldF)) newF = (pl, i, (Slot v newF))

applyInc :: Player -> Int -> Slot -> ModifiedSlot
applyInc pl i (Slot v f) | v > 0 && v < 65535 = (pl, i, Slot (v + 1) f)
applyInc pl i (Slot v f) | otherwise          = (pl, i, Slot v       f)

apply :: GameState -> ModifiedSlot -> Function -> Function -> Either String ModifiedSlots
apply _ ms (I) f = Right $ [modifyFunction ms f]
apply _ ms (Succ Undef) (Zero) = Right $ [modifyFunction ms (FValue 1)]
apply _ ms (Succ Undef) (FValue n)  | n < 65535 = Right [modifyFunction ms (FValue $ n+1)]
							        | otherwise = Right [modifyFunction ms (FValue 65535)]

apply _ ms (Dbl Undef) (Zero)     = Right [modifyFunction ms (FValue 0)]
apply _ ms (Dbl Undef) (FValue n)   | n < 32768 = Right [modifyFunction ms (FValue (n * 2))]
							        | otherwise = Right [modifyFunction ms (FValue 65535)]

apply gs ms (Get Undef) (Zero) = apply gs ms (Get Undef) (FValue 0)
apply gs ms (Get Undef) (FValue i) =
		case M.lookup i (propSlots gs) of
			Just slot -> case isSlotAlive slot of
								True  -> case slot of
									(Slot v Zero) -> Right [modifyFunction ms Zero]
									(Slot v val@(FValue _)) -> Right [modifyFunction ms val]
									_ -> Left $ "Error of applying 'get' to " ++ show i ++ ": slot " ++ show i ++ " not a value."
								False -> Left $ "Error of applying 'get' to " ++ show i ++ ": slot " ++ show i ++ " is dead."
			Nothing -> Left $ "Error of applying 'get' to " ++ show i ++ ": invalid slot number."

apply _ ms (Put Undef) _     = Right [modifyFunction ms I]

apply _ ms (S Undef g x) arg = Right [modifyFunction ms (S arg g x)]
apply _ ms (S f Undef x) arg = Right [modifyFunction ms (S f arg x)]
apply gs ms (S f g Undef) arg = let
									Right mh@(newSlh@(_, _, (Slot _ newFh)):msh) = apply gs ms f arg				-- Maybe error.
									Right my@(newSly@(_, _, (Slot _ newFy)):msy) = apply gs newSlh g arg
									Right mz = apply gs newSly newFh newFy
								in  Right $ mz ++ msh ++ msy

apply _ ms  (K Undef y)   arg = Right [modifyFunction ms (K arg y)]
apply _ ms  (K x Undef)   _   = Right [modifyFunction ms x]
apply gs ms (Inc Undef) (Zero) = apply gs ms (Inc Undef) (FValue 0)

apply gs@(GameState _ _ pl _) ms (Inc Undef) (FValue i) =
		case M.lookup i (playerSlots gs pl) of
			Just slotToInc -> Right $ [modifyFunction ms I] ++ [applyInc pl i slotToInc]
			Nothing        -> Left $ "Error of applying 'inc' to " ++ show i ++ ": invalid slot number."

apply _ _ f c = Left $ "Error of applying " ++ show f ++ " to " ++ show c ++ ": don't know how to apply."

-- FIX ME: final reducing of the function, may be failed.
rightApp' :: GameState -> ModifiedSlot -> Card -> Either String GameState
rightApp' gs ms@(pl, i, (Slot vit f)) card = apply gs ms f card >>= applyResults gs

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