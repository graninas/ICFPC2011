module Tests where

import qualified Data.Map as M

import Types

isVitalityValid :: Vitality -> Bool
isVitalityValid    v = v >= -1 && v <= 65535

isFieldValueValid :: Field -> Bool
isFieldValueValid (FValue v) = v >= 0 && v <= 65535
isFieldValueValid Zero       = True
isFieldValueValid _          = undefined  -- Maybe error.

isSlotNumberValid :: Int -> Slots -> Bool
isSlotNumberValid = M.member

isSlotAlive :: Slot -> Bool
isSlotAlive (Slot v _) = isVitalityValid v && v > 0

isFunctionField :: Slot -> Bool
isFunctionField (Slot _ (FValue _)) = False
isFunctionField (Slot _ Zero)       = False
isFunctionField (Slot _ Undef)      = undefined  -- Maybe errror.
isFunctionField _          = True
