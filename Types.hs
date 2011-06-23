module Types where

import qualified Data.Map as M

type Vitality = Int

data Function =
			  I
			| Zero -- Zero may be deleted because we have FValue.
			| Succ Function
			| Dbl Function
			| Get Function
			| Put Function
			| S Function Function Function
			| K Function Function
			| Inc Function
			| Dec Function
			| Attack Function Function Function
			| Help Function Function Function
			| Copy Function
			| Revive Function
			| Zombie Function Function
			| FValue Int
			| Undef
	deriving (Eq, Show, Read)

type Field = Function
		
data Slot = Slot Vitality Field
	deriving (Eq, Show, Read)

type Card = Function

data ApplicationType = LeftApplication | RightApplication
type AppErrors = [String]

type Slots = M.Map Int Slot

data Player = Player0 | Player1
	deriving (Eq, Show, Read)

type ModifiedSlot  = (Player, Int, Slot)
type ModifiedSlots = [ModifiedSlot]
type ApplicationResult = (GameState, ModifiedSlot)
	
data GameState = GameState {
	propSlots :: Slots,
	opSlots   :: Slots,
	curPlayer :: Player,
	turn      :: Int
} deriving (Eq, Show, Read)

type IntAppType  = Int
type SlotNumber  = Int
type CardName    = String
type PlayerTurn  = (IntAppType, SlotNumber, CardName)
type PlayerTurns = [PlayerTurn]
type TemplateRepeatCount = Int
type Template = (TemplateRepeatCount, PlayerTurns)
type Scheme   = [Template]
type TemplateIndex   = Int
type PlayerTurnIndex = Int
data SchemeEvaluating = SchemeEval TemplateIndex TemplateRepeatCount PlayerTurnIndex Scheme
	deriving (Eq, Show, Read)