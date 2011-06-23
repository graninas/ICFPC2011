module Templates where

import Types
import Constructing

----- Template Schemes ----------------------------------------------

g10 :: Scheme  -- Works!
g10 = [
	(1, [
		(2, 0, zero),
		(1, 0, succc),
		(1, 0, dbl),
		(1, 0, dbl),
		(1, 0, dbl),
		(1, 0, succc),
		(1, 0, succc)
		])]
		
g100 :: Scheme  -- Works!
g100 = g10 ++ [
	(1, [
		(1, 0, dbl),
		(1, 0, dbl),
		(1, 0, dbl)
		]),
	(20, [(1, 0, succc)])]
	
g1000 :: Scheme  -- Works!
g1000 = g100 ++ [
	(1, [
		(1, 0, dbl),
		(1, 0, dbl),
		(1, 0, dbl)
		]),
	(200, [(1, 0, succc)])]

g10000 :: Scheme  -- Works!
g10000 = g1000 ++ [
	(1, [
		(1, 0, dbl),
		(1, 0, dbl),
		(1, 0, dbl)
		]),
	(2000, [(1, 0, succc)])]
	
g255 :: Scheme  -- Works!
g255 = g100 ++ [
	(1, [
		(1, 0, dbl)
		]),
	(55, [(1, 0, succc)])]
	
gAny :: Int -> Scheme -- Work but not full
gAny n
	| n < 10 = [
		(1, take ( n + 1 ) [
			(2, 0, zero),
			(1, 0, succc),
			(1, 0, succc),
			(1, 0, succc),
			(1, 0, succc),
			(1, 0, succc),
			(1, 0, succc),
			(1, 0, succc),
			(1, 0, succc),
			(1, 0, succc)
			])]
	| n == 10 = g10
	| n == 20 =
		let (g10Sch:(cnt, cmds):g100SchP2) = g100 in
			g10Sch : [(1, [head cmds])]
	
----- Test schemes --------------------------------------------------
incTestScheme :: Scheme
incTestScheme = [
	(1, [
		(2, 0, zero),
		(1, 0, inc)])]
doubleIncTestScheme :: Scheme
doubleIncTestScheme = [
	(1, [
		(2, 0, s),
		(2, 0, inc),
		(2, 0, inc),
		(2, 0, zero)
		])]
decTestScheme :: Scheme
decTestScheme = [
	(1, [
		(2, 0, zero),
		(1, 0, dec)])]
doubleDecTestScheme :: Scheme
doubleDecTestScheme = [
	(1, [
		(2, 0, s),
		(2, 0, dec),
		(2, 0, dec),
		(2, 0, zero)
		])]

---  Example schemes ----------------------------------------------------
		
defaultNothingTemplate :: Scheme
defaultNothingTemplate = [(-1, [(1, 0, "I")])]

stupidScheme :: Scheme
stupidScheme = [
	(-1, [(2, 0, i)])
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

----------------- Vladimir's templates
user0_test1 :: Scheme
user0_test1 = [
	(1, [
		(2, 0, "zero"),
		(1, 0, "succ"),
		(1, 0, "succ"),
		(1, 0, "dbl"),
		(1, 0, "inc")
		])]
		
user0_test2_long :: Scheme -- same as fieldToFieldApp
user0_test2_long = [
	(1, [
		(2, 0, help),
		(2, 0, zero),
		(1, 0, k),
		(1, 0, s),
		(2, 0, succc),
		(2, 0, zero),
		(2, 1, zero),
		(1, 1, succc),
		(1, 1, dbl),
		(1, 1, dbl),
		(1, 1, dbl),
		(1, 1, dbl),
		(1, 0, k),
		(1, 0, s),
		(2, 0, get),
		(1, 0, k),
		(1, 0, s),
		(2, 0, succc),
		(2, 0, zero)])]

user0_test3_loop_test :: Scheme -- infiniteLoop
user0_test3_loop_test = [
	(-1, [
		(2, 0, s),
		(2, 0, get),
		(2, 0, i),
		(2, 0, zero)])]
	
user1_test1_test :: Scheme
user1_test1_test = [
	(1, [
		(2, 0, inc ),
		(2, 0, zero),
		(2, 0, dec),
		(2, 0, zero),
		(1, 0, succc)] )]

	
	

	
	