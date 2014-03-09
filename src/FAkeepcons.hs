module FAkeepcons

( keepconsBDFA

, keepconsTNFA	-- does usefulTNFA
, keepconsTNFA'	-- does not do usefulTNFA

, keepconsETNFA
)


where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import FAuseful

----------------------------------------------------------------------------

keepconsBDFA :: (Ord a, Show a) => Opts -> BDFA a -> TCons -> BDFA a
keepconsBDFA opts (BDFA cons all starts moves) cons0 =
    let cons1 = cons `intersectSet` cons0
	moves1 = filterFM (\ t _ -> stcon t `elementOf` cons1) moves
    	b = BDFA cons1 all starts moves1
	c = usefulBDFA opts b	-- todo: really useful doing this?
    in	c

---------------------------------------------------------------------------

keepconsTNFA' :: (Ord a, Show a) => Opts -> TNFA a -> TCons -> TNFA a
keepconsTNFA' opts (TNFA cons all starts moves) cons0 =
    let cons1 = cons `intersectSet` cons0
	moves1 = filterFM ( \ _ ts -> not (isEmptySet ts) ) $
		mapFM ( \ _ ts -> 
			filterSet ( \ t -> stcon t `elementOf` cons1) ts ) 
		moves
    	b = TNFA cons1 all starts moves1
    in
	b

keepconsTNFA :: (Ord a, Show a) => Opts -> TNFA a -> TCons -> TNFA a
keepconsTNFA opts a cons0 =
    let b = keepconsTNFA' opts a cons0
	c = usefulTNFA opts b	
    in	
	c

---------------------------------------------------------------------------

keepconsETNFA :: (Ord a, Show a) => Opts -> ETNFA a -> TCons -> ETNFA a
keepconsETNFA opts (ETNFA cons all starts moves eps) cons0 =
    let cons1 = cons `intersectSet` cons0
	moves1 = filterFM ( \ _ ts -> not (isEmptySet ts) ) $
		mapFM ( \ _ ts -> 
			filterSet ( \ t -> stcon t `elementOf` cons1) ts ) 
		moves
    	b = ETNFA cons1 all starts moves1 eps
	-- todo: usefulETNFA?
    in	b


---------------------------------------------------------------------------
