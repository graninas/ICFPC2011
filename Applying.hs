module Applying where

import qualified Data.Map as M


import Types
import Tests

apply :: GameState -> Function -> Function -> Either String Function
apply _ (I) f = Right f
apply _ (Succ Undef) (Zero) = Right $ FValue 1
apply _ (Succ Undef) (FValue n)  | n < 65535 = (Right $ FValue $ n+1)
							     | otherwise = Right $ FValue 65535
apply _ (Dbl Undef) (Zero)     = Right $ FValue 0
apply _ (Dbl Undef) (FValue n)   | n < 32768 = Right $ FValue (n * 2)
							     | otherwise = Right $ FValue 65535
apply gs (Get Undef) (Zero) = apply gs (Get Undef) (FValue 0)
apply gs (Get Undef) (FValue i) =
		case M.lookup i (propSlots gs) of
			Just slot@(Slot _ (Func _))  -> Left $ "Error of applying 'get' to " ++ show i ++ ": field of slot " ++ show i ++ " not a value."
			Just slot@(Slot v (Val val)) -> case isSlotAlive slot of
												True  -> Right $ FValue val
												False -> Left $ "Error of applying 'get' to " ++ show i ++ ": slot " ++ show i ++ " is dead."
			Nothing -> Left $ "Error of applying 'get' to " ++ show i ++ ": invalid slot number."
apply _ (Put Undef) _ = Right I
apply _ (S Undef x y) f = Right $ S f x y
apply _ (S x Undef y) f = Right $ S x f y
apply _ (S x y Undef) f = Right $ S x y f
apply _ (K Undef x)   f = Right $ K f x
apply _ (K x Undef)   f = Right $ K x f
apply gs (Inc Undef) (Zero) = apply gs (Inc Undef) (FValue 0)
{-apply gs (Inc Undef) (FValue i) =
		case M.lookup i (propSlots gs) of
			Just (Slot v _) -> case v > 0 && v < 65535 of
									True -> 
			Nothing -> Left $ "Error of applying 'inc' to " ++ show i ++ ": invalid slot number."
-}
apply _ f c = Left $ "Error of applying " ++ show f ++ " to " ++ show c ++ ": don't know how to apply."

rightApp' :: GameState -> Slot -> Card -> Either String Slot
rightApp' gs (Slot vit (Func s)) card = apply gs s card >>= \x -> Right $ Slot vit (Func x)
rightApp' _ _ _ = Left "rightApp(') failed."


-- FIX ME: final reducing of the function, may be failed.
rightApp :: GameState -> Slot -> Card -> Either String Slot
rightApp gs slot@(Slot vit field) card | isSlotAlive slot && isFunction field = rightApp' gs slot card
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
-}