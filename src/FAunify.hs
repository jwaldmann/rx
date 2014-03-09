module FAunify

( unifyTNFA
, unifyETNFA
)

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import FAhom
import FAcmpct

import FAconv

unifyTNFA :: Opts -> TNFA Int -> TNFA Int
unifyTNFA opts = fixpoint (same opts) where
	same opts b @ (TNFA cons all starts moves) =
	    let	-- this uses Ord on sets!
		c = collectFM (eltsFM moves)
		h = mapFM ( \ w ts -> 
		    lookupWithDefaultFM c 
			(error ("same.c cannot find " ++ show ts)) ts) moves
	    	d = homTNFA opts (\ x -> case lookupFM h x of
			Just y -> Right y; Nothing -> Left x) b 
		e = cmpctTNFA opts d
	    in

--		trace ("(* heurist *)") $

--		trace ("\nheuristic.same.b: " ++ show b) $
--		trace ("\nheuristic.same.c: " ++ show c) $
--		trace ("\nheuristic.same.h: " ++ show h) $
--		trace ("\nheuristic.same.d: " ++ show d) $
--		trace ("\nheuristic.same.e: " ++ show d) $

		trinfo opts "unify" e $

		e



unifyETNFA :: Opts -> ETNFA Int -> ETNFA Int
unifyETNFA opts e @ (ETNFA _ _ _ _ eps) = fixpoint (same opts) e' 
  where
    t @ (TNFA c a s m) = etnfa2tnfa opts e
    e' = ETNFA c a s m eps

    same opts t @ (ETNFA c a s m e) =
	 let
	     -- find equivalence classes
	     c = addListToFM_C unionSet emptyFM
		 [ (ts, unitSet p) | (p, ts) <- fmToList m ]
	     -- map each state to rep. of eq. class
	     r =addListToFM (listToFM [ (p, p) | p <- setToList a ])
			  [ (p, head $ setToList ps) 
			  | ps <- eltsFM c, p <- setToList ps
			  ]
	     d = homETNFA opts 
			  (lookupWithDefaultFM r (error "unifyETNFA")) t
			
	 in
	     d
	 
