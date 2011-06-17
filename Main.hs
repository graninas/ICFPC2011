module Main where

import qualified Data.Map as M

import Types
import Showing
import Constructing
import Cards




run :: Slot' -> IO ()
run slot = do
	--putStrLn "(1) apply card to slot, or (2) apply slot to card?"
	putStrLn "Enter card name to apply slot to card:"

	
	



main :: IO ()
main = do 
	putStrLn "Lambda: The Gathering"
	run $ defaultSlot'
