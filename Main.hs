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
--applicationType _ = undefined  -- FIX ME: parse error message

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

autoApplication :: GameState -> (String, GameState)
autoApplication curGS = autoApp curGS

playerMove mode newGS pl2Sch = do
			putStrLn "(1) apply card to slot, or (2) apply slot to card?"
			x <- getLine
			case applicationType x of
				LeftApplication -> do
					putStrLn "slot no?"
					y <- getLine
					putStrLn "card name?"
					z <- getLine
					let (str, gs) = leftApplication newGS (slotNo y) (blankCard z)
					putStrLn str
					appendFile "gamesession.txt" ("\nslot: " ++ y ++ "card: " ++ z ++ "\n" ++ str)
					run mode (gs {curPlayer = otherPlayer (curPlayer gs)}) pl2Sch
				RightApplication -> do
					putStrLn "slot no?"
					y <- getLine
					putStrLn "card name?"
					z <- getLine
					let (str, gs) = rightApplication newGS (slotNo y) (blankCard z)
					putStrLn str
					appendFile "gamesession.txt" ("\nslot: " ++ y ++ "card: " ++ z ++ "\n" ++ str)
					run mode (gs {curPlayer = otherPlayer (curPlayer gs)}) pl2Sch


run ::  String -> GameState -> SchemeEvaluating -> IO ()
run "alt" oldGS pl2Sch = do
	let (msg, newGS) = autoApplication oldGS
	putStrLn $ "\nAutoApplication results:\n" ++ msg
	putStrLn $ showGameState newGS
	appendFile "gamesession.txt" ("\nAutoApplication results:" ++ msg)
	appendFile "gamesession.txt" (showGameState newGS)
	putStrLn "(1) apply card to slot, or (2) apply slot to card?"
	x <- getLine
	case applicationType x of
		LeftApplication -> do
			putStrLn "card name?"
			z <- getLine
			putStrLn "slot no?"
			y <- getLine
			let (str, gs) = leftApplication newGS (slotNo y) (blankCard z)
			putStrLn str
			appendFile "gamesession.txt" ("\nslot: " ++ y ++ "card: " ++ z ++ "\n" ++ str)
			run "alt" (gs {curPlayer = otherPlayer (curPlayer gs)}) pl2Sch
		RightApplication -> do
			putStrLn "slot no?"
			y <- getLine
			putStrLn "card name?"
			z <- getLine
			let (str, gs) = rightApplication newGS (slotNo y) (blankCard z)
			putStrLn str
			appendFile "gamesession.txt" ("\nslot: " ++ y ++ "card: " ++ z ++ "\n" ++ str)
			run "alt" (gs {curPlayer = otherPlayer (curPlayer gs)}) pl2Sch

run "comp" oldGS@(GameState _ _ pl _) pl1SchEval = do
	let (msg, autoAppGS) = autoApplication oldGS
	putStrLn $ "\nAutoApplication results:\n" ++ msg
	putStrLn $ showGameState autoAppGS
	appendFile "gamesession.txt" ("\nAutoApplication results:" ++ msg)
	appendFile "gamesession.txt" (showGameState autoAppGS)
	case pl of
		Player0 -> playerMove "comp" autoAppGS pl1SchEval
		Player1 -> let
				playerTurn    = getPlayerTurn pl1SchEval
				newSchemeEval = evalScheme pl1SchEval
				(msg, newGS)  = evalPlayerTurn playerTurn autoAppGS
			in do
				putStrLn $ showPlayerTurn $ playerTurn
				putStrLn msg
				appendFile "gamesession.txt" (showPlayerTurn $ playerTurn)
				appendFile "gamesession.txt" (msg)
				run "comp" (newGS{turn = 1 + turn newGS, curPlayer = otherPlayer pl}) newSchemeEval

getPlayerTurn :: SchemeEvaluating -> PlayerTurn
getPlayerTurn scheme@(SchemeEval templIdx templRepCnt plTIdx templates) = (snd (templates !! templIdx) !! plTIdx)

evalScheme :: SchemeEvaluating -> SchemeEvaluating
evalScheme (SchemeEval templIdx templRepCnt plTIdx templates) = 
	let (cnt, plTurns) = (templates !! templIdx) in
		case plTIdx + 1 == length plTurns of -- Достигли конца текущего шаблона.
			True  -> if cnt == -1 || templRepCnt < (cnt - 1) then SchemeEval templIdx (templRepCnt+1) 0 templates
					 else case templIdx < (length templates) - 1 of
								False -> undefined --SchemeEval 0 0 0 templates
								True  -> SchemeEval (templIdx + 1) 0 0 templates
			False -> SchemeEval templIdx templRepCnt (plTIdx + 1) templates

evalPlayerTurn :: PlayerTurn -> GameState -> (String, GameState)
evalPlayerTurn (appType, slotNumber, cardName) oldGS@(GameState slots1 slots2 curP t) =
	case appType of
		1 -> leftApplication  oldGS slotNumber (blankCard cardName)
		2 -> rightApplication oldGS slotNumber (blankCard cardName)

runTest' :: SchemeEvaluating -> SchemeEvaluating -> (Bool, Bool) -> GameState -> IO ()
runTest' pl1SchemeEval pl2SchemeEval showPl@(showP1, showP2) oldGS@(GameState _ _ curP _) = do
	let (msg, autoAppGS) = autoApplication oldGS
	putStrLn $ "\nAutoApplication results:\n" ++ msg
	case curP of
		Player0 ->
			let
				playerTurn = getPlayerTurn pl1SchemeEval
				newSchemeEval = evalScheme pl1SchemeEval
				(msg, newGS)  = evalPlayerTurn playerTurn autoAppGS
			in do
					if showP1 then do
						putStrLn $ showGameState newGS
						putStrLn $ showPlayerTurn $ playerTurn
						putStrLn msg
						appendFile "test.log" ((showGameState newGS) ++ (showPlayerTurn $ playerTurn) ++ msg)
					else return ()
					runTest' newSchemeEval pl2SchemeEval showPl newGS {curPlayer = otherPlayer curP}
		Player1 ->
			let
				playerTurn = getPlayerTurn pl2SchemeEval
				newSchemeEval = evalScheme pl2SchemeEval
				(msg, newGS) = evalPlayerTurn playerTurn autoAppGS
			in do
					if showP2 then do
						putStrLn $ showGameState newGS
						putStrLn $ showPlayerTurn $ playerTurn
						putStrLn msg
						appendFile "test.log" ((showGameState newGS) ++ (showPlayerTurn $ playerTurn) ++ msg)
					else return ()
					runTest' pl1SchemeEval newSchemeEval showPl (newGS{turn = 1 + turn newGS}) {curPlayer = otherPlayer curP}

runTest pl1Scheme pl2Scheme  plShow = do
	writeFile "test.log" ("Testing scheme: " ++ show pl1Scheme)
	runTest' (SchemeEval 0 0 0 pl1Scheme) (SchemeEval 0 0 0 pl2Scheme) plShow initGameState

testMove :: IO ()
testMove = testMove' initGameState
	where testMove' oldGS = do
			putStrLn $ "Player0 slots:"
			putStrLn $ showPlayerSlots Player0 oldGS
			putStrLn $ "Player1 slots:"
			putStrLn $ showPlayerSlots Player1 oldGS
			putStrLn  "(1) apply card to slot, or (2) apply slot to card?"
			apT <- getLine
			putStrLn "Modified slot:"
			x <- getLine
			putStrLn "Func:"
			y <- getLine
			let (msg, gs) = tmLeftApp' x y apT
			putStrLn msg
			testMove' gs
			where
				tmLeftApp' x y apT = case reads x of
					[(readedX@(pl, _, _), _)] -> case reads y of 
						[(readedY, _)] -> if apT == "1" then leftApp (oldGS{curPlayer = pl}) readedX readedY else rightApp (oldGS{curPlayer = pl}) readedX readedY
						_ -> ("read X error", oldGS)
					_ -> ("read Y error", oldGS)

testMoveR :: ModifiedSlot -> Function -> IO ()
testMoveR ms f =
	let (msg, gs) = rightApp initGameState ms f in
		do
			putStrLn $ "Player0 slots:"
			putStrLn $ showPlayerSlots Player0 gs
			putStrLn $ "Player1 slots:"
			putStrLn $ showPlayerSlots Player1 gs
			putStrLn msg
testMoveL :: ModifiedSlot -> Function -> IO ()
testMoveL ms f =
	let (msg, gs) = leftApp initGameState ms f in
		do
			putStrLn $ "Player0 slots:"
			putStrLn $ showPlayerSlots Player0 gs
			putStrLn $ "Player1 slots:"
			putStrLn $ showPlayerSlots Player1 gs
			putStrLn msg

runTest1 = runTest fieldToFieldApp stupidScheme (True, False)
runTest2 = runTest fieldToFieldApp infiniteS    (True, False)
runTest3 = runTest infiniteLoop    infiniteS    (True, False)
runTestInc = runTest incTestScheme incTestScheme (True, False)
runTestDoubleInc = runTest doubleIncTestScheme doubleIncTestScheme (True, False)
runTestDec = runTest decTestScheme decTestScheme (True, False)
runTestDoubleDec = runTest doubleDecTestScheme doubleDecTestScheme (True, False)


runVladTest01 = runTest user0_test1				stupidScheme (True, False)
runVladTest02 = runTest user0_test2_long 		stupidScheme (True, False)
runVladTest03 = runTest user0_test3_loop_test 	stupidScheme (True, False)
runVladTest11 = runTest user1_test1_test		stupidScheme (True, False)

main :: IO ()
main = do 
	putStrLn "Lambda: The Gathering"
	writeFile "gamesession.txt" []
	run "comp" initGameState (SchemeEval 0 0 0 stupidScheme)

