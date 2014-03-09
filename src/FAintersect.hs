module FAintersect

( intersectTNFA		-- does compaction
, intersectBDFA

, intersectTNFA'	-- produces TNFA (Int, Int), do not compact
)

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import FAcmpct
import FAdet
import FAconv
import FAkeepcons
import FAuseful

intersectTNFA :: Opts -> TNFA Int -> TNFA Int -> TNFA Int
intersectTNFA opts a1 a2 =
    let
	b = intersectTNFA' opts a1 a2
	c = usefulTNFA opts b
        d = cmpctTNFA opts c
    in
	d

intersectTNFA' :: (Show a, Show b, Ord a, Ord b) => 
	       Opts -> TNFA a -> TNFA b -> TNFA (a, b)
intersectTNFA' opts a1 @ (TNFA consa1 _ _ _) a2 @ (TNFA consa2 _ _ _) =
    let	cons = consa1 `intersectSet` consa2
	TNFA cons1 all1 starts1 moves1 = keepconsTNFA' opts a1 cons
	TNFA cons2 all2 starts2 moves2 = keepconsTNFA' opts a2 cons

        imoves1 = invert moves1
        imoves2 = invert moves2

	comb (w1, w2) = mkSet
		[ mksterm (stcon t1) (zippy (stargs t1) (stargs t2)) 
		| t1 <- setToList $ lookupset moves1 w1
		, stcon t1 `elementOf` cons

		, t2 <- setToList $ lookupset moves2 w2
		, stcon t2 `elementOf` cons

		, stcon t1 == stcon t2
		]

	moves = listToFM [ ( (w1, w2), cs)
		| w1 <- setToList all1, w2 <- setToList all2 
		, cs <- [ comb (w1, w2) ], not (isEmptySet cs)
		]
	starts3 = mkSet [ (x, y) 
		| x <- setToList starts1, y <- setToList starts2 ]

	all3 =  mkSet [ (x, y) 
		| x <- setToList all1, y <- setToList all2 ]
	b3 = TNFA cons all3 starts3 moves


    in 	
--	trace ("\nintersectTNFA.a1: " ++ show a1) $
--	trace ("\nintersectTNFA.a2: " ++ show a2) $
--	trace ("\nintersectTNFA.cons: " ++ show cons) $
--	trace ("\nintersectTNFA.moves: " ++ show moves) $
--	trace ("\nintersectTNFA.starts': " ++ show starts') $
--	trace ("\nintersectTNFA.all: " ++ show all) $
--	trace ("\nintersectTNFA.starts: " ++ show starts) $
--	trace ("\nintersectTNFA.b: " ++ show b) $

	trinfo opts "intersect" b3 $

	b3



------------------------------------------------------------------------------

intersectBDFA :: Opts -> BDFA Int -> BDFA Int -> BDFA Int
intersectBDFA opts d1 d2 =
    let	
	t1 = d2t opts d1
	t2 = d2t opts d2
	t = intersectTNFA opts t1 t2
	b = tnfa2bnfa opts t
	d = simplebnfa2bdfa opts b
    in
	d


