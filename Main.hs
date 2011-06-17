module Main where

import qualified Data.Map as M

import Types
import Showing
import Constructing
import Applying


applicationType :: String -> ApplicationType
applicationType "1" = LeftApplication
applicationType "2" = RightApplication
applicationType _ = undefined  -- FIX ME: parse error message

slotNo = read -- FIX ME: parse error message

rightApplication :: GameState -> Int -> Card -> (String, GameState)
rightApplication curGS@(GameState slots1 slots2 curP turn) slNo card =
			case M.lookup slNo slots1 of
				Nothing -> ("Invalid argument", curGS)
				Just slot -> case rightApp slot card of
					Right newSlot -> ("All ok.", curGS)     -- FIX ME: ("Applying success", newGS)
					Left str -> (str, curGS)

run :: GameState -> IO ()
run oldGS@(GameState slots1 slots2 curP turn) = do
	putStrLn $ showGameState oldGS
	putStrLn "(1) apply card to slot, or (2) apply slot to card?"
	x <- getLine
	case applicationType x of
		LeftApplication -> undefined
		RightApplication -> do
			putStrLn "slot no?"
			y <- getLine
			putStrLn "card name?"
			z <- getLine
			let (str, gs) = rightApplication oldGS (slotNo y) (blankCard z)
			putStrLn str
			run gs




main :: IO ()
main = do 
	putStrLn "Lambda: The Gathering"
	run $ initGameState
