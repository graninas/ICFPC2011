module Tests where

import qualified Data.Map as M

import Types

isVitalityValid :: Vitality -> Bool
isVitalityValid    v = v >= -1 && v <= 65535

isFieldValueValid :: Field -> Bool
isFieldValueValid (Value v) = v >= 0 && v <= 65535
isFieldValueValid _ = True

isSlotNumberValid :: Int -> Slots -> Bool
isSlotNumberValid i slots = M.member i slots

isSlotAlive :: Slot -> Bool
isSlotAlive (Slot v _) = isVitalityValid v && v > 0

isFunction :: Field -> Bool
isFunction (Value _) = False
isFunction _ = True