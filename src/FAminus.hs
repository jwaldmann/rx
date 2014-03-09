module FAminus

( minusTNFA
, minusBDFA
)

where


import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import FAintersect

import FAneg
import FAconv

----------------------------------------------------------------------

minusTNFA :: Opts -> TNFA Int -> TNFA Int -> TNFA Int
minusTNFA opts
	  x1 @ (TNFA cons1 all1 starts1 moves1)
	  x2 @ (TNFA cons2 all2 starts2 moves2) =
    let	cons = cons1 `unionSet` cons2
	y2 = TNFA cons all2 starts2 moves2
    	z2 = negTNFA opts y2
	v  = intersectTNFA opts x1 z2
    in	
	trinfo opts "minus" v $
	v


minusBDFA :: Opts -> BDFA Int -> BDFA Int -> BDFA Int
minusBDFA opts
	  x1 @ (BDFA cons1 all1 starts1 moves1)
	  x2 @ (BDFA cons2 all2 starts2 moves2) =
    let	cons = cons1 `unionSet` cons2
	y2 = BDFA cons all2 starts2 moves2
    	z2 = negBDFA opts y2
	v  = intersectBDFA opts x1 z2
	u = d2t opts v
    in	
	trinfo opts "minus" u $
	v

