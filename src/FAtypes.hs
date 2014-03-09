module FAtypes

( BDFA(..)
, BNFA(..)
, TNFA(..)
, ETNFA(..)
, Auto

, emptyTNFA

, trinfo
, info

, sons
)

where

import Set
import FiniteMap

import Options

import TA

import Ids	-- provides instance Show Id

-- import Stuff	-- provides instances Show Set, Show FiniteMap

-- bottom up deterministic
data BDFA a = BDFA 
	TCons 		-- what algebra we're in
	(Set a)		-- all states
	(Set a)		-- accepting states
	(FiniteMap (STerm a) a)	-- transition table

	deriving (Eq, Show)


-- bottom up nondeterministic
data BNFA a = BNFA 
	TCons 		-- what algebra we're in
	(Set a)		-- all states
	(Set a)		-- accepting states
	(FiniteMap (STerm a) (Set a))	-- transition table

	deriving (Eq, Show)
	

-- top down non deterministic
data TNFA a = TNFA
	TCons		-- algebra
	(Set a)		-- all states
	(Set a)		-- start states
	(FiniteMap a (Set (STerm a)))	-- production rules

	deriving (Eq, Show)

emptyTNFA = TNFA emptySet emptySet emptySet emptyFM

-- this is what we normally use
type Auto = TNFA Int


-- top down non deterministic with epsilon moves
data ETNFA a = ETNFA
	TCons		-- algebra
	(Set a)		-- all states
	(Set a)		-- start states
	(FiniteMap a (Set (STerm a)))	-- production rules
	(FiniteMap a (Set a))		-- epsilon moves

	deriving (Eq, Show)

---------------------------------------------------------------

sons :: TNFA Int -> TCon -> Int -> [[Int]]
sons (TNFA cons all starts moves) con p =
    let
	ts = lookupWithDefaultFM moves (error "BackS.sons.ts") p
	lrs = 	[ stargs t
		| t <- setToList ts
		, stcon t == con
		]
    in
	lrs

---------------------------------------------------------------

info msg (TNFA cons all starts moves) =
    let sc = " cons: " ++ show cons
	sa = " |all|: " ++ show (cardinality all)
	sm = " |moves|: " ++ show (sizeFM moves)
    in  ("\n" ++ msg ++ sc ++ sa ++ sm)

trinfo opts msg aut = troff opts $ info msg aut

