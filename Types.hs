module Types where

import qualified Data.Map as M

type Vitality = Int

data FieldValue = 
		Value Int
		| Func String
	deriving (Eq, Show, Read)

data Slot = Slot Vitality FieldValue
	deriving (Eq, Show, Read)

type Slots = M.Map Int Slot

type FunctionResult = Either String FieldValue


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
		
data Slot' = Slot' Vitality Function
	deriving (Eq, Show, Read)

type Card = Function