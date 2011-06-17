module Showing where

import Types
import Constructing

showField (Value v) = show v
showField (Func  f) = f

showSlots [] = "(slots {10000,I} are omitted)"
showSlots ((_, slot):xs) | slot == defaultSlot = showSlots xs
showSlots ((n, Slot v f):xs) = "\n" ++ show n ++ "={" ++ show v ++ "," ++ showField f ++ "}" ++ showSlots xs