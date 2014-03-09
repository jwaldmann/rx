module FAmerge 

( mergeTNFA
, mminTNFA
)



where

import Set
import FiniteMap

import Stuff
import Sorters

import Defaults
import Options

import TA
import FAtypes
import Ids

import FAuseful
import FAunify
import FAmin
import FAminus

import Trace


-----------------------------------------------------------------------

mminTNFA :: Opts -> TNFA Int -> TNFA Int
mminTNFA opts t0 @ (TNFA _ _ _ m0) =
    let
	t3 = unifyTNFA opts t0
	t1 @ (TNFA _ _ _ m1) = mergeTNFA opts t3
	t2 @ (TNFA _ _ _ m2) = minTNFA opts t1
	t4 @ (TNFA _ _ _ m4) = mergeTNFA opts t2
    in

{-
	( if sizeFM m1 < sizeFM m0 
	  then 
	       trace ("\n!!!!! mminTNFA.t0.size = " ++ show (sizeFM m0)) .
	       trace ("\n!!!!! mminTNFA.t1.size = " ++ show (sizeFM m1)) .

	       trace ("\n!!!!! mminTNFA.t0 = " ++ show t0) .
	       trace ("\n!!!!! mminTNFA.t1 = " ++ show t1) 
	  else id
	) .
	( if sizeFM m2 < sizeFM m4
	  then 

	       trace ("\n!!!!! mminTNFA.t2.size = " ++ show (sizeFM m2)) .
	       trace ("\n!!!!! mminTNFA.t4.size = " ++ show (sizeFM m4)) .
	       trace ("\n!!!!! mminTNFA.t2 = " ++ show t2) .
	       trace ("\n!!!!! mminTNFA.t4 = " ++ show t4) 
	  else id
	) $
-}


	t4

-----------------------------------------------------------------------

mergeTNFA :: Opts -> TNFA Int -> TNFA Int
mergeTNFA opts = fixpoint same

same t @ (TNFA c a s m) =
    case -- msortwith (\ (TNFA _ _ _ m') -> sizeFM m' )
	 [ merge p q t'
	 | [p, q] <- candidates t
	 , let t' @ (TNFA c' a' s' m') = merge p q t
	 ,    sizeFM m' < sizeFM m 
	   || cardinality s' < cardinality s
	 ]
      of t' : _ -> t'
	 [] -> t

-----------------------------------------------------------------------

candidates :: TNFA Int -> [[Int]]
candidates t @ (TNFA cons all starts moves) =
    let 
    
	pairs :: [a] -> [(a,a)]
	pairs [] = []
	pairs (x : ys) = [ (x, y) | y <- ys ] ++ pairs ys

	cs = setToList . mkSet $
	     [ setToList . mkSet $ [ xs !! k, ys !! k ]
	     | (t, rs) <- fmToList moves
	     , c <- setToList cons
	     , let xys = [ stargs r | r <- setToList rs, stcon r == c ]
	     , (xs, ys) <- pairs xys
	     , k <- [ 0 .. length xs - 1 ]
	     , xs // (k, 0) == ys // (k, 0)
	     ]
    in
--	trace ("\ncandidates.t = " ++ show t) $
--	trace ("\ncandidates.cs = " ++ show cs) $
	
	cs

-----------------------------------------------------------------------


-- n = p `union` q
merge p q t @ (TNFA cons all starts moves) =
    let
	n = 1 + maximum (setToList all)
	m1 = listToFM 
	       [ (t, merges p q n rs) 
	       | (t, rs) <- fmToList moves
	       ]
	m2 = plusFM_C (error "FAmerge.merge.m2") m1
		  $ listToFM [ (n, lookupset m1 p `unionSet` lookupset m1 q) ]
	a2 = all `unionSet` unitSet n
	s2 = if (p `elementOf` starts) && (q `elementOf` starts)
	     then (starts `minusSet` mkSet [p, q]) `unionSet` unitSet n
	     else starts
	t2 = TNFA cons a2 s2 m2
	u2 = usefulTNFA opts0 t2
    in

{-
	trace ("\nmerge.(p,q,t) = " ++ show (p, q, t)) $
	trace ("\nmerge.t2 = " ++ show t2) $
	trace ("\nmerge.u2 = " ++ show u2) $
-}

	u2

(x : xs) // (0, y) = (y : xs)
(x : xs) // (k, y) = x : (xs // (k-1, y))

merges p q n rs = mkSet
       [ r'
       | r <- setToList rs
       , let con = stcon r
       , let args = stargs r
       , r' <- take 1 $
	            [ mksterm con (args // (k, n))
		    | k <- [0 .. length args-1]
		    , (x, y) <- [(p, q), (q, p)]
		    , (args !! k) == x 
		    , mksterm con (args // (k, y)) `elementOf` rs
		    ]
		    ++ [ r ]
       ]

    
