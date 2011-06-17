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

rightApplication :: GameState -> Int -> Card -> String
rightApplication gs slNo card = "All Ok."

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
								putStrLn $ rightApplication sl (slotNo y) (blankCard z)
								
	
	



main :: IO ()
main = do 
	putStrLn "Lambda: The Gathering"
	run $ (M.fromList $defaultSlotList 255, M.fromList $ defaultSlotList 255)
