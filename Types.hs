module Types where

import qualified Data.Map as M

type Vitality = Int

data FunctionArg =
			  Undefined
			| Function
	deriving (Eq, Show, Read)
	
data Function =
			  FieldValue Int
			| Id
			| Zero
			| Succ FunctionArg
			| Dbl FunctionArg
			| Get FunctionArg
			| Put FunctionArg
			| S FunctionArg FunctionArg FunctionArg
			| K FunctionArg FunctionArg
			| Int FunctionArg
			| Dec FunctionArg
			| Attack FunctionArg FunctionArg FunctionArg
			| Help FunctionArg FunctionArg FunctionArg
			| Copy FunctionArg
			| Revive FunctionArg
			| Zombie FunctionArg FunctionArg
	deriving (Eq, Show, Read)
		
type Field = Function
data Slot = Slot Vitality Field
	deriving (Eq, Show, Read)

type Card = Function

data ApplicationType = LeftApplication | RightApplication

type Slots = M.Map Int Slot

type GameState = (Slots, Slots)