module Applying where

import qualified Data.Map as M


import Types
import Tests

apply :: Function -> Function -> Either String Function
apply (I) f = Right f
--apply (Zero) _ = Right $ FValue 0  -- It seems wrong.
apply (Succ Undef) (Zero) = Right $ FValue 1
apply (Succ Undef) (FValue n)  | n < 65535 = Right $ FValue $ n+1
							   | otherwise = Right $ FValue 65535
apply (Dbl Undef) (Zero)     = Right $ FValue 0
apply (Dbl Undef) (FValue n)   | n < 32768 = Right $ FValue (n * 2)
							   | otherwise = Right $ FValue 65535
-- apply (Get Undef) (Zero)     = ??? slots and gamestate needed.
-- other applicators.

apply (S Undef x y) f = Right $ S f x y
apply (S x Undef y) f = Right $ S x f y
apply (S x y Undef) f = Right $ S x y f

apply _ _ = Left "apply failed."

rightApp' :: Slot -> Card -> Either String Slot
rightApp' (Slot vit (Func s)) card = apply s card >>= \x -> Right $ Slot vit (Func x)
rightApp' _ _ = Left "rightApp(') failed."


-- FIX ME: final reducing of the function, may be failed.
rightApp :: Slot -> Card -> Either String Slot
rightApp slot@(Slot vit field) card | isSlotAlive slot && isFunction field = rightApp' slot card
rightApp _ _ = Left "rightApp failed"

{-

			| Get FieldFunction
			| Put FieldFunction
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