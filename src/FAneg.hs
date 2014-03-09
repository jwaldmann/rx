module FAneg

( negTNFA
, completeBDFA
, negBDFA
)

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes

import FAconv

import FAdet

-------------------------------------------------------------------------

completeBDFA :: Opts -> BDFA Int -> BDFA Int
completeBDFA opts (BDFA cons all starts moves) =
    let
	sink = 1 + maximum (0 : setToList all)
	all1 = unitSet sink `unionSet` all
	moves1 = listToFM 
		[ (t, lookupWithDefaultFM moves sink t)
		| tc <- setToList cons, n <- [tconarity tc]
		, args <- setToList (insts (take n (repeat all1)))
		, t <- [ mksterm tc args ]
		]
    in 
       BDFA cons all1 starts moves1
    

negBDFA :: Opts -> BDFA Int -> BDFA Int
negBDFA opts aut =
    let
	BDFA cons all starts moves = completeBDFA opts aut
	starts1 = all `minusSet` starts
    	d = BDFA cons all starts1 moves
	u = bnfa2tnfa opts (bdfa2bnfa opts d)
    in	
	trinfo opts "neg" u $
	d


negTNFA :: Opts -> TNFA Int -> TNFA Int
negTNFA opts x =
    let d = tnfa2bdfa opts x
    	d' = negBDFA opts d
	u = bnfa2tnfa opts (bdfa2bnfa opts d')
    in	
	trinfo opts "neg" u $
	u

