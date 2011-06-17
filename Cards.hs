module Cards where

import qualified Data.Map as M
import Text.Printf

import Types
import Tests

iF x   = x
zeroF  = 0

succF :: Int -> FunctionResult
succF n | n < 65535 = Right $ Value $ n + 1
	    | otherwise = Right $ Value 65535
	   
dblF :: Int -> FunctionResult
dblF  n | n < 32768 = Right $ Value $ n * 2
        | otherwise = Right $ Value 65535
	   
getF :: Int -> Slots -> FunctionResult
getF i slots =
		case M.lookup i slots of
			Just slot@(Slot v fv) -> case isSlotAlive slot of
										True -> Right fv
										False -> Left $ printf "(get %d) Error: Slot is dead." i
			Nothing -> Left $ printf "(get %d) Error: i is invalid." i

putF _ = iF

			
cI    = Func "I" 
cZero = Func "zero"
cSucc = Func "succ"
cDbl  = Func "dbl"