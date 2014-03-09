module FAhom

( homBDFA
, homTNFA
, homETNFA
)

where

import Set
import FiniteMap

import Options

import Stuff

import TA
import FAtypes




homBDFA :: (Ord a, Ord b) => Opts -> (a -> b) -> (BDFA a -> BDFA b)
-- homomorphism: identifies some states
homBDFA opts f (BDFA cons all starts moves) =
    let 
	-- some paranoid checks first
	nostarts = all `minusSet` starts
	starts' = mapSet f starts
	nostarts' = mapSet f nostarts
	all' = starts' `unionSet` nostarts'

	moves' = addListToFM_C 
		(\ x y -> if x /= y 
			then error "bdfahom identifies incosistent ruleset"
			else x)
		emptyFM
		[ (mksterm (stcon t) (map f (stargs t)), f w)
		| (t, w) <- fmToList moves
		]
		
    in	if not (isEmptySet (starts' `intersectSet` nostarts'))
	then error "homBDFA identifies starts and nostarts"
	else	BDFA cons all' starts' moves'

---------------------------------------------------------------

homTNFA :: (Ord a, Ord b) => Opts -> (a -> b) -> (TNFA a -> TNFA b)
-- homomorphism: identifies some states
homTNFA opts f (TNFA cons all starts moves) =
    let 
	-- can't do paranoia checking here
	-- since rejecting states are not uniquely determined

	starts' = mapSet f starts
	all' = mapSet f all

	moves' = addListToFM_C 
		(\ x y -> if x /= y 
			then error "tnfahom identifies incosistent ruleset"
			else x)
		emptyFM
		[ ( f w 
		  , mapSet ( \ t -> mksterm (stcon t) (map f (stargs t))) ts )
		| (w, ts) <- fmToList moves
		]
		
    	g = TNFA cons all' starts' moves'

    in	
	trinfo opts "homTNFA" g $

	g
------------------------------------------------------------------------

homETNFA :: (Ord a) => Opts -> (a -> a) -> (ETNFA a -> ETNFA a)
-- homomorphism: identifies some states
-- don't change start states
-- but keeps identifications in epsilons
homETNFA opts f (ETNFA cons all starts moves eps) =
    let 
	moves' = addListToFM_C 
		(\ x y -> if x /= y 
			then error "etnfahom identifies incosistent ruleset"
			else x)
		emptyFM
		[ ( f w 
		  , mapSet ( \ t -> mksterm (stcon t) (map f (stargs t))) ts )
		| (w, ts) <- fmToList moves
		]
	eps' = addListToFM_C unionSet eps
	         [ (p, unitSet (f p)) | p <- setToList all ]
		
    	g = ETNFA cons all starts moves' eps'

    in	


	g


---------------------------------------------------------------
