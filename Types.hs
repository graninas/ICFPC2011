module Types where

import qualified Data.Map as M

type Vitality = Int

data FieldFunction =
			  Id
			| Zero
			| Succ FieldFunction
			| Dbl FieldFunction
			| Get FieldFunction
			| Put FieldFunction
			| S FieldFunction FieldFunction FieldFunction
			| K FieldFunction FieldFunction
			| Inc FieldFunction
			| Dec FieldFunction
			| Attack FieldFunction FieldFunction FieldFunction
			| Help FieldFunction FieldFunction FieldFunction
			| Copy FieldFunction
			| Revive FieldFunction
			| Zombie FieldFunction FieldFunction
			| FValue Int
	deriving (Eq, Show, Read)

data Field =
			  Value Int
			| Function FieldFunction
	deriving (Eq, Show, Read)
		
data Slot = Slot Vitality Field
	deriving (Eq, Show, Read)

type Card = FieldFunction

data ApplicationType = LeftApplication | RightApplication

type Slots = M.Map Int Slot

type GameState = (Slots, Slots)