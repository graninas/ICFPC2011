module Templates where

import Types

stupidScheme :: Scheme
stupidScheme = [
	(-1, [(2, 0, "zero")])
	]

infiniteS :: Scheme
infiniteS = [
	(-1, [(1, 0, "S")])
	]
	
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
	  (2, 0, "zero")])]
