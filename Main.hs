module Main where

import qualified Data.Map as M

import Types
import Showing
import Constructing
import Applying
import Templates
import Tests


applicationType :: String -> ApplicationType
applicationType "1" = LeftApplication
applicationType "2" = RightApplication
applicationType _ = undefined  -- FIX ME: parse error message

slotNo = read -- FIX ME: parse error message

rightApplication :: GameState -> Int -> Card -> (String, GameState)
rightApplication curGS slNo card =
			let curP = curPlayer curGS in
			case M.lookup slNo (playerSlots curGS curP) of
				Nothing -> ("Invalid argument", curGS)
				Just slot -> rightApp curGS (curP, slNo, slot) card

leftApplication :: GameState -> Int -> Card -> (String, GameState)
leftApplication curGS slNo card =
			let curP = curPlayer curGS in
			case M.lookup slNo (playerSlots curGS curP) of
				Nothing -> ("Invalid argument", curGS)
				Just slot -> leftApp curGS (curP, slNo, slot) card

run ::  String -> GameState -> IO ()
run "alt" oldGS@(GameState slots1 slots2 curP turn) = do
	putStrLn $ showGameState oldGS
	putStrLn "(1) apply card to slot, or (2) apply slot to card?"
	x <- getLine
	case applicationType x of
		LeftApplication -> do
			putStrLn "card name?"
			z <- getLine
			putStrLn "slot no?"
			y <- getLine
			let (str, gs) = leftApplication oldGS (slotNo y) (blankCard z)
			putStrLn str
			run "alt" (gs {curPlayer = otherPlayer (curPlayer gs)})
		RightApplication -> do
			putStrLn "slot no?"
			y <- getLine
			putStrLn "card name?"
			z <- getLine
			let (str, gs) = rightApplication oldGS (slotNo y) (blankCard z)
			putStrLn str
			run "alt" (gs {curPlayer = otherPlayer (curPlayer gs)})

getPlayerTurn :: SchemeEvaluating -> PlayerTurn
getPlayerTurn scheme@(SchemeEval templIdx templRepCnt plTIdx templates) = (snd (templates !! templIdx) !! plTIdx)
			
evalScheme :: SchemeEvaluating -> SchemeEvaluating
evalScheme (SchemeEval templIdx templRepCnt plTIdx templates) = 
	let (cnt, plTurns) = (templates !! templIdx) in
		case plTIdx + 1 == length plTurns of -- Достигли конца текущего шаблона.
			True  -> if cnt == -1 || templRepCnt < cnt then SchemeEval templIdx (templRepCnt+1) 0 templates
					 else case templIdx + 1 == length templates of
							True  -> SchemeEval 0 0 0 templates
							False -> SchemeEval (templIdx+1) 0 0 templates
			False -> SchemeEval templIdx templRepCnt (plTIdx+1) templates

evalPlayerTurn :: PlayerTurn -> GameState -> (String, GameState)
evalPlayerTurn (appType, slotNumber, cardName) oldGS@(GameState slots1 slots2 curP t) =
	case appType of
		1 -> leftApplication  oldGS slotNumber (blankCard cardName)
		2 -> rightApplication oldGS slotNumber (blankCard cardName)

runTest' :: SchemeEvaluating -> SchemeEvaluating -> (Bool, Bool) -> GameState -> IO ()
runTest' pl1SchemeEval pl2SchemeEval showPl@(showP1, showP2) oldGS@(GameState _ _ curP _) = do
	case curP of
		Player0 ->
			let
				playerTurn = getPlayerTurn pl1SchemeEval
				newSchemeEval = evalScheme pl1SchemeEval
				(msg, newGS)  = evalPlayerTurn playerTurn oldGS
			in do
					if showP1 then do
						putStrLn $ showGameState oldGS
						putStrLn $ showPlayerTurn $ playerTurn
						putStrLn msg
						appendFile "test.log" ((showGameState oldGS) ++ (showPlayerTurn $ playerTurn) ++ msg)
					else return ()
					runTest' newSchemeEval pl2SchemeEval showPl newGS {curPlayer = otherPlayer curP}
		Player1 ->
			let
				playerTurn = getPlayerTurn pl2SchemeEval
				newSchemeEval = evalScheme pl2SchemeEval
				(msg, newGS) = evalPlayerTurn playerTurn oldGS
			in do
					if showP2 then do
						putStrLn $ showGameState oldGS
						putStrLn $ showPlayerTurn $ playerTurn
						putStrLn msg
						appendFile "test.log" ((showGameState oldGS) ++ (showPlayerTurn $ playerTurn) ++ msg)
					else return ()
					runTest' pl1SchemeEval newSchemeEval showPl (newGS{turn = 1 + turn newGS}) {curPlayer = otherPlayer curP}

runTest pl1Scheme pl2Scheme  plShow = do
	writeFile "test.log" ("Testing scheme: " ++ show pl1Scheme)
	runTest' (SchemeEval 0 0 0 pl1Scheme) (SchemeEval 0 0 0 pl2Scheme) plShow initGameState

runTest1 = runTest fieldToFieldApp stupidScheme (True, False)
runTest2 = runTest fieldToFieldApp infiniteS    (True, False)

main :: IO ()
main = do 
	putStrLn "Lambda: The Gathering"
	run "alt" initGameState

