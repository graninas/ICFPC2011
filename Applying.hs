module Applying where

import qualified Data.Map as M
import Text.Printf

import Types
import Tests

rightApp' slot@(Slot vit field) card = undefined

rightApp slot@(Slot vit field) card | isSlotAlive slot && isFunction field = rightApp' slot card
									| otherwise = undefined