module Templates where

import Types

defaultNothingTemplate :: Scheme
defaultNothingTemplate = [(-1, [(1, 0, "I")])]

stupidScheme :: Scheme
stupidScheme = [
	(-1, [(2, 0, "zero")])
	]

infiniteS :: Scheme
infiniteS = [
	(-1, [(1, 0, "S")])
	]

infiniteLoop :: Scheme
infiniteLoop = [
	(1, [
		(2, 0, "S"),
		(2, 0, "get"),
		(2, 0, "I"),
		(2, 0, "zero")])]
	
fieldToFieldApp :: Scheme
fieldToFieldApp = [
	(1, [
	  (2, 0, "help"),
	  (2, 0, "zero"),
	  (1, 0, "K"),
	  (1, 0, "S"),
	  (2, 0, "succ"),
	  (2, 0, "zero"),
	  (2, 1, "zero"),
	  (1, 1, "succ"),
	  (1, 1, "dbl"),
	  (1, 1, "dbl"),
	  (1, 1, "dbl"),
	  (1, 1, "dbl"),
	  (1, 0, "K"),
	  (1, 0, "S"),
	  (2, 0, "get"),
	  (1, 0, "K"),
	  (1, 0, "S"),
	  (2, 0, "succ"),
	  (2, 0, "zero")])
	 ]
	-- ++ defaultNothingTemplate
