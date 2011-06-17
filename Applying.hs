module Applying where

import qualified Data.Map as M


import Types
import Tests

applyS :: Function -> Function -> Either String Function
applyS (S Undef x y) f = Right $ S f x y
applyS (S x Undef y) f = Right $ S x f y
applyS (S x y Undef) f = Right $ S x y f
applyS _ _ = Left "applyS failed."

rightApp' :: Slot -> Card -> Either String Slot
rightApp' (Slot vit (Func s)) card = applyS s card >>= \x -> Right $ Slot vit (Func x)
rightApp' _ _ = Left "rightApp(') failed."


-- FIX ME: final reducing of the lambda.
rightApp :: Slot -> Card -> Either String Slot
rightApp slot@(Slot vit field) card | isSlotAlive slot && isFunction field = rightApp' slot card
rightApp _ _ = Left "rightApp failed"

{-
			  Id
			| Zero
			| Succ FieldFunction
			| Dbl FieldFunction
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