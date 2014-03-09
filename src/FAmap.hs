module FAmap

( mapBDFA
, mapTNFA
, mapETNFA
)

where

import Set
import FiniteMap

import Options

import Stuff

import TA
import FAtypes

mapBDFA :: (Ord a, Ord b) => Opts -> (a -> b) -> BDFA a -> BDFA b
-- f must be injective
mapBDFA opts f (BDFA cons all starts moves) =
    let	h = listToFM [(a, f a)|a <- setToList all]
	r = lookupWithDefaultFM h (error "mapBDFA")
	all' = mapSet r all
	starts' = mapSet r starts
	moves' = listToFM [ (mksterm (stcon t) (map r (stargs t)), r v)
			  | (t, v) <- fmToList moves ]
    in	BDFA cons all' starts' moves'

mapTNFA :: (Ord a, Ord b) => Opts -> (a -> b) -> TNFA a -> TNFA b
-- f must be injective
mapTNFA opts f (TNFA cons all starts moves) =
    let	h = listToFM [ (a, f a) | a <- setToList all]
	r = f -- lookupWithDefaultFM h (error "mapTNFA")
	all' = mapSet r all
	starts' = mapSet r starts
	moves' = listToFM 
	    [ (r v, mapSet (\ t -> mksterm (stcon t) (map r (stargs t))) ts )
	    | (v, ts) <- fmToList moves ]
    in	TNFA cons all' starts' moves'

mapETNFA :: (Ord a, Ord b) => Opts -> (a -> b) -> ETNFA a -> ETNFA b
-- f must be injective
mapETNFA opts f (ETNFA cons all starts moves eps) =
    let	h = listToFM [ (a, f a) | a <- setToList all]
	r = f -- lookupWithDefaultFM h (error "mapETNFA")
	all' = mapSet r all
	starts' = mapSet r starts
	moves' = listToFM 
	    [ (r v, mapSet (\ t -> mksterm (stcon t) (map r (stargs t))) ts )
	    | (v, ts) <- fmToList moves ]
	eps' = listToFM
	     [ (r v, mapSet r ts)
	     | (v, ts) <- fmToList eps
	     ]
    in	ETNFA cons all' starts' moves' eps'

