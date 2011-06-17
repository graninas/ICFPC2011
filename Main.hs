module Main where

import qualified Data.Map as M

import Types
import Showing
import Constructing
import Applying


applicationType :: String -> ApplicationType
applicationType "1" = LeftApplication
applicationType "2" = RightApplication
applicationType _ = undefined

slotNo = read

rightApplication :: GameState -> Int -> Card -> (String, GameState)
rightApplication curGS@(slots1, slots2) slNo card =
			case M.lookup slNo slots1 of
				Nothing -> ("Invalid argument", curGS)
				Just slot -> case rightApp slot card of
					Just newSlot -> ("All ok.", curGS)     -- FIX ME: ("Applying success", newGS)
					Nothing -> ("Applying failed", curGS)  -- FIX ME: ("Applying failed",  newGS)

run :: GameState -> IO ()
run sl = do
	putStrLn "(1) apply card to slot, or (2) apply slot to card?"
	x <- getLine
	case applicationType x of
		LeftApplication -> undefined
		RightApplication -> do
			putStrLn "slot no?"
			y <- getLine
			putStrLn "card name?"
			z <- getLine
			let (str, gs) = rightApplication sl (slotNo y) (blankCard z)
			putStrLn str
			run gs




main :: IO ()
main = do 
	putStrLn "Lambda: The Gathering"
	run $ (M.fromList $defaultSlotList 255, M.fromList $ defaultSlotList 255)
