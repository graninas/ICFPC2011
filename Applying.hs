module Applying where

import qualified Data.Map as M
import Text.Printf

import Types
import Tests
import Constructing

applyResult :: GameState -> ModifiedSlot -> Either String ApplicationResult
applyResult gs@(GameState slots _ _ _) ms@(Player0, i, sl) = Right $ (gs {propSlots = updateSlot i sl slots}, ms)
applyResult gs@(GameState _ slots _ _) ms@(Player1, i, sl) = Right $ (gs {opSlots   = updateSlot i sl slots}, ms)

modifyFunction :: ModifiedSlot -> Function -> ModifiedSlot
modifyFunction (pl, i, (Slot v _)) newF = (pl, i, (Slot v newF))

getRealSlot :: ModifiedSlot -> ModifiedSlot -> ModifiedSlot
getRealSlot ms@(pl1, slIdx1, sl1@(Slot v1 _)) mbThis@(pl2, slIdx2, sl2@(Slot v2 _))
		| pl1 == pl2 && slIdx1 == slIdx2 = mbThis
		| otherwise = ms

identifyFunction :: ModifiedSlot -> ModifiedSlot -> ModifiedSlot
identifyFunction ms@(pl1, slIdx1, sl1@(Slot v1 _)) mbThis@(pl2, slIdx2, sl2@(Slot v2 _)) | pl1 == pl2 && slIdx1 == slIdx2 = (pl1, slIdx1, (Slot v2 I))
																						 | otherwise = (pl1, slIdx1, (Slot v1 I))

applyInc :: Player -> Int -> Slot -> ModifiedSlot
applyInc pl i (Slot v f) | v > 0 && v < 65535 = (pl, i, Slot (v + 1) f)
applyInc pl i sl@(Slot v f) | otherwise          = (pl, i, sl)

applyDec :: Player -> Int -> Slot -> ModifiedSlot
applyDec pl i (Slot v f) | v > 0     = (pl, i, Slot (v - 1) f)
applyDec pl i sl@(Slot v f) | otherwise = (pl, i, sl)

applyAttack :: Player -> Int -> Slot -> Int -> ModifiedSlot
applyAttack pl j sl@(Slot v f) n | isSlotAlive sl =
		let divided = div (n * 9) 10 in
			if v - divided < 0 then (pl, j, Slot 0 f)
							   else (pl, j, Slot (v - divided) f)
	
applyKickBack :: Player -> Int -> Slot -> Int -> ModifiedSlot	
applyKickBack pl i sl@(Slot v f) n | n <= v = (pl, i, Slot (v - n) f)
								   -- | otherwise = undefined            -- Probably it cannot to be.

applyHelp   pl j sl@(Slot v f) n | isSlotAlive sl =
		let divided = div (n * 11) 10 in
			if v + divided > 65536 then (pl, j, Slot 65536 f)
							   else (pl, j, Slot (v + divided) f)

applyDevote pl i sl@(Slot v f) n | n <= v = (pl, i, Slot (v - n) f)
								 -- | otherwise = undefined              -- Probably it cannot to be.

applyRevive pl i sl@(Slot v f) | v < 0     = (pl, i, Slot 1 I)
							   | otherwise = (pl, i, sl)

applyZombie pl i sl f | not $ isSlotAlive $ sl = (pl, i, Slot (-1) I)
					  | otherwise = (pl, i, sl)
							   
apply :: Int -> GameState -> ModifiedSlot -> Function -> Function -> Either String ApplicationResult
-- I app
apply appCnt gs ms I f | appCnt <= appLimit = applyResult gs (modifyFunction ms f)

-- succ app
apply appCnt gs ms (Succ Undef) (Zero)      | appCnt <= appLimit = applyResult gs (modifyFunction ms (FValue 1))
apply appCnt gs ms (Succ Undef) (FValue n)  | n < 65535 && appCnt <= appLimit = applyResult gs (modifyFunction ms (FValue $ n+1))
							                | n >= 65535  = applyResult gs (modifyFunction ms (FValue 65535))

-- dbl app
apply appCnt gs ms (Dbl Undef) (Zero)      | appCnt <= appLimit = applyResult gs (modifyFunction ms (FValue 0))
apply appCnt gs ms (Dbl Undef) (FValue n)  | n < 32768 && appCnt <= appLimit  = applyResult gs (modifyFunction ms (FValue (n * 2)))
							               | n >= 65535 = applyResult gs (modifyFunction ms (FValue 65535))

-- get app
apply appCnt gs ms (Get Undef) (Zero) | appCnt <= appLimit = apply (appCnt+1) gs ms (Get Undef) (FValue 0)
apply appCnt gs@(GameState _ _ pl _) ms (Get Undef) (FValue i) | appCnt <= appLimit =
		case M.lookup i (playerSlots gs pl) of
			Just slot@(Slot v f) -> case isSlotAlive slot of
								True  -> applyResult gs (modifyFunction ms f)
								False -> Left $ "Error of applying 'get' to " ++ show i ++ ": slot " ++ show i ++ " is dead."
			Nothing -> Left $ "Error of applying 'get' to " ++ show i ++ ": invalid slot number."

-- put app
apply appCnt gs ms (Put Undef) _     | appCnt <= appLimit = applyResult gs (modifyFunction ms I)

-- S app
apply appCnt gs ms (S Undef g x) arg | appCnt <= appLimit = applyResult gs (modifyFunction ms (S arg g x))
apply appCnt gs ms (S f Undef x) arg | appCnt <= appLimit = applyResult gs (modifyFunction ms (S f arg x))
apply appCnt gs ms (S f g Undef) arg | appCnt <= appLimit = let
									Right (newGSh, modSlot1@(_, _, (Slot _ h))) = apply (appCnt+1) gs     ms       f arg
									Right (newGSy, modSlot2@(_, _, (Slot _ y))) = apply (appCnt+2) newGSh modSlot1 g arg
								in apply (appCnt+3) newGSy modSlot2 h y

-- K app
apply appCnt gs ms (K Undef y) arg    | appCnt <= appLimit = applyResult gs (modifyFunction ms (K arg y))
apply appCnt gs ms (K x Undef) _      | appCnt <= appLimit = applyResult gs (modifyFunction ms x)

-- inc app
apply appCnt gs                      ms (Inc Undef) (Zero)     | appCnt <= appLimit = apply (appCnt+1) gs ms (Inc Undef) (FValue 0)
apply appCnt gs@(GameState _ _ pl _) ms (Inc Undef) (FValue i) | appCnt <= appLimit =
		case M.lookup i (playerSlots gs pl) of
			Just slotToInc -> let
								 Right (newGSincremented, incrModSlot) = applyResult gs (applyInc pl i slotToInc)
								 Right (newGSidentified, modSlot)      = applyResult gs (identifyFunction ms incrModSlot)
							  in Right (newGSidentified, modSlot)
			Nothing        -> Left $ "Error of applying 'inc' to " ++ show i ++ ": invalid slot number."

-- dec app
apply appCnt gs                      ms (Dec Undef) (Zero)     | appCnt <= appLimit = apply (appCnt+1) gs ms (Dec Undef) (FValue 0)
apply appCnt gs@(GameState _ _ pl _) ms (Dec Undef) (FValue i) | appCnt <= appLimit =
		let otherP = otherPlayer pl in
		case M.lookup (255 - i) (playerSlots gs otherP) of
			Just slotToDec -> let
								 Right (newGSdecremented, decrModSlot) = applyResult gs (applyDec otherP (255 - i) slotToDec)
								 Right (newGSidentified, modSlot)      = applyResult newGSdecremented (identifyFunction ms decrModSlot)
							  in Right (newGSidentified, modSlot)
			Nothing        -> Left $ "Error of applying 'dec' to " ++ show i ++ ": invalid slot number."

-- attack app
apply appCnt gs ms (Attack Undef j n) (Zero)         | appCnt <= appLimit = applyResult gs (modifyFunction ms (Attack (FValue 0) j n))
apply appCnt gs ms (Attack Undef j n) val@(FValue _) | appCnt <= appLimit = applyResult gs (modifyFunction ms (Attack val j n))
apply appCnt gs ms (Attack i Undef n) (Zero)         | appCnt <= appLimit = applyResult gs (modifyFunction ms (Attack i (FValue 0) n))
apply appCnt gs ms (Attack i Undef n) val@(FValue _) | appCnt <= appLimit = applyResult gs (modifyFunction ms (Attack i val n))
apply appCnt gs ms f@(Attack i j Undef) (Zero)       | appCnt <= appLimit = apply (appCnt+1) gs ms f (FValue 0)

apply appCnt gs@(GameState _ _ pl _) ms   (Attack (FValue i) (FValue j) Undef) (FValue n) | appCnt <= appLimit =
		let otherP = otherPlayer pl in
		case M.lookup (255 - j) (playerSlots gs otherP) of
			Just slotToAttack ->
				case M.lookup i (playerSlots gs pl) of
					Just slotToKickBack@(Slot v _) ->
						case n <= v of
							True -> let
										 Right (newGSattacked, _)              = applyResult gs (applyAttack otherP (255 - j) slotToAttack n)
										 Right (newGSkickedBack, kickedBackMS) = applyResult newGSattacked   (applyKickBack pl i slotToKickBack n)
										 Right (newGSidentified, modSlot)      = applyResult newGSkickedBack (identifyFunction ms kickedBackMS)
									in Right (newGSidentified, modSlot)
							False -> Left $ "Error of applying 'attack' to " ++ show i ++ ": n > v."
					Nothing -> Left $ "Error of applying 'attack' to " ++ show i ++ ": invalid proponent slot number."
			Nothing -> Left $ "Error of applying 'attack' to " ++ show (255 - j) ++ ": invalid opponent slot number."

-- help app
apply appCnt gs ms (Help Undef j n) (Zero)         | appCnt <= appLimit = applyResult gs (modifyFunction ms (Help (FValue 0) j n))
apply appCnt gs ms (Help Undef j n) val@(FValue _) | appCnt <= appLimit = applyResult gs (modifyFunction ms (Help val j n))
apply appCnt gs ms (Help i Undef n) (Zero)         | appCnt <= appLimit = applyResult gs (modifyFunction ms (Help i (FValue 0) n))
apply appCnt gs ms (Help i Undef n) val@(FValue _) | appCnt <= appLimit = applyResult gs (modifyFunction ms (Help i val n))

apply appCnt gs@(GameState _ _ pl _) ms f@(Help i j Undef) (Zero)   | appCnt <= appLimit = apply (appCnt+1) gs ms f (FValue 0)
apply appCnt gs@(GameState _ _ pl _) ms   (Help (FValue i) (FValue j) Undef) (FValue n) | appCnt <= appLimit =
		let plSlots = playerSlots gs pl in
		case M.lookup i plSlots of
			Just slotToDevote@(Slot v _) ->
				case M.lookup j plSlots of
					Just slotToHelp@(Slot helpV helpF) ->
						case n <= v && isSlotAlive slotToHelp of
							True -> let
										 Right (newGSdevoted, devotedMS)  = applyResult gs (applyDevote pl i slotToDevote n)
										 (_, _, realSlotToHelp) = getRealSlot (pl, j, slotToHelp) devotedMS
										 Right (newGShelped,  helpedMS)   = applyResult newGSdevoted    (applyHelp pl j realSlotToHelp n)
										 Right (newGSidentified, modSlot) = applyResult newGShelped     (identifyFunction ms (getRealSlot (getRealSlot ms devotedMS) helpedMS))
									in Right (newGSidentified, modSlot)
							False -> Left $ "Error of applying 'help' to " ++ show i ++ ": n > v."
					Nothing -> Left $ "Error of applying 'help' to " ++ show j ++ ": invalid proponent slot number."
			Nothing -> Left $ "Error of applying 'help' to " ++ show i ++ ": invalid proponent slot number."

-- copy app
apply appCnt gs                      ms (Copy Undef) (Zero)     | appCnt <= appLimit = apply (appCnt+1) gs ms (Copy Undef) (FValue 0)
apply appCnt gs@(GameState _ _ pl _) ms (Copy Undef) (FValue i) | appCnt <= appLimit =
		let otherPlSlots = playerSlots gs (otherPlayer pl) in
		case M.lookup i otherPlSlots of
			Just (Slot _ f) -> applyResult gs (modifyFunction ms f)
			Nothing -> Left $ "Error of applying 'copy' to " ++ show i ++ ": invalid opponent slot number."
			
-- revive app
apply appCnt gs                      ms (Revive Undef) (Zero)     | appCnt <= appLimit = apply (appCnt+1) gs ms (Revive Undef) (FValue 0)
apply appCnt gs@(GameState _ _ pl _) ms (Revive Undef) (FValue i) | appCnt <= appLimit =
		let plSlots = playerSlots gs pl in
		case M.lookup i plSlots of
			Just slotToRevive@(Slot _ f) ->
								let
									Right (newGSrevived, revivedMS)  = applyResult gs           (applyRevive pl i slotToRevive)
									Right (newGSidentified, modSlot) = applyResult newGSrevived (identifyFunction ms revivedMS)
								in  Right (newGSidentified, modSlot)
			Nothing -> Left $ "Error of applying 'revive' to " ++ show i ++ ": invalid proponent slot number."

-- zombie app
apply appCnt gs ms (Zombie Undef x) (Zero)         | appCnt <= appLimit = applyResult gs (modifyFunction ms (Zombie (FValue 0) x))
apply appCnt gs ms (Zombie Undef x) val@(FValue i) | appCnt <= appLimit = applyResult gs (modifyFunction ms (Zombie val x))
apply appCnt gs@(GameState _ _ pl _) ms (Zombie (FValue i) Undef) f | appCnt <= appLimit = 
		let
			otherP = otherPlayer pl
			otherPlSlots = playerSlots gs otherP
		in
		case M.lookup (255 - i) otherPlSlots of
			Just slotToZombie@(Slot _ f) ->
								let
									Right (newGSzombied, zombiedMS)  = applyResult gs (applyZombie otherP (255 - i) slotToZombie f)
									Right (newGSidentified, modSlot) = applyResult newGSzombied (modifyFunction ms I)
								in  Right (newGSidentified, modSlot)
			Nothing -> Left $ "Error of applying 'revive' to " ++ show (255 - i) ++ ": invalid proponent slot number."

apply appCnt _ _ f c | appCnt > appLimit = Left $ "Error of applying " ++ show f ++ " to " ++ show c ++ ": application limit (" ++ show appLimit ++") exceeded."
apply _ _ _ f c = Left $ "Error of applying " ++ show f ++ " to " ++ show c ++ ": don't know how to apply."


rightApp :: GameState -> ModifiedSlot -> Card -> (String, GameState)
rightApp gs ms@(pl, _, slot@(Slot vit f)) card | isSlotAlive slot =
	case apply appLimitStartValue gs ms f card of
		Right (newGS, modifiedSlots) -> ("Right applying Ok.", newGS)
		Left msg -> case applyResult gs (modifyFunction ms I) of
						Right (newGS2, _) -> (msg, newGS2)
						Left msg2        -> (msg2, gs)
rightApp gs ms card | otherwise = case applyResult gs (modifyFunction ms I) of
						Right (newGS2, _) -> ("Slot is dead.", newGS2)
						Left msg2        -> (msg2, gs)

leftApp :: GameState -> ModifiedSlot -> Card -> (String, GameState)
leftApp gs ms@(pl, _, slot@(Slot vit f)) card | isSlotAlive slot =
	case apply appLimitStartValue gs ms card f of
		Right (newGS, modifiedSlots) -> ("Left applying Ok.", newGS)
		Left msg -> case applyResult gs (modifyFunction ms I) of
						Right (newGS2, _) -> (msg, newGS2)
						Left msg2        -> (msg2, gs)
leftApp gs ms card | otherwise = case applyResult gs (modifyFunction ms I) of
						Right (newGS2, _) -> ("Slot is dead.", newGS2)
						Left msg2        -> (msg2, gs)
						

autoApplySlot :: GameState -> ModifiedSlot -> Card -> (String, GameState)
autoApplySlot gs ms@(pl, slIdx, slot@(Slot vit f)) card | not $ isSlotAlive $ slot = let
																		(msg, rAppGs) = case apply appLimitStartValue gs ms f card of
																							Right (newGS, _) -> ([], newGS)
																							Left msg -> case applyResult gs (modifyFunction ms I) of
																											Right (newGS2, _) -> (msg, newGS2)
																											Left msg2         -> (msg2, gs)
																		Right (revivedSlGS, _) = applyResult rAppGs (pl, slIdx, (Slot 0 I))
																	in ([], revivedSlGS)
								        | otherwise = ([], gs)

autoApp' :: Int -> GameState -> ([String], GameState)
autoApp' slIdx gs@(GameState _ _ pl _)  | slIdx == defaultSlotCount = (["End of autoApp."], gs)
										| otherwise = let plSlots = playerSlots gs pl in
			case M.lookup slIdx plSlots of
				Just slot@(Slot v f) -> let
								(msg, slotAppliedGS) = autoApplySlot gs (pl, slIdx, (Slot v f)) I
								(newMsgs, newGS2)    = autoApp' (slIdx+1) slotAppliedGS
							 in (msg : newMsgs, newGS2)

autoApp :: GameState -> (String, GameState)
autoApp gs = let (msgs, newGS) = autoApp' 0 gs in (unlines . (filter (/= [])) $ msgs, newGS)
		
		