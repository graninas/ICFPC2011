module Types where

import qualified Data.Map as M

type Vitality = Int

data Function =
			  I
			| Zero
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

data Field =
			  Val  Int
			| Func Function
	deriving (Eq, Show, Read)
		
data Slot = Slot Vitality Field
	deriving (Eq, Show, Read)

type Card = Function

data ApplicationType = LeftApplication | RightApplication

type Slots = M.Map Int Slot

data Player = Player0 | Player1
	deriving (Eq, Show, Read)

data GameState = GameState {
	propSlots :: Slots,
	opSlots   :: Slots,
	curPlayer :: Player,
	turn      :: Int
} deriving (Eq, Show, Read)