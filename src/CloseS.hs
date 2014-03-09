-- not working


module CloseS

( closeS
, closeSpublic
)

-- checks whether a given automaton is closed under S reduction
-- makes automaton deterministic first

-- this implementation is ugly ugly ugly
-- w.r.t. the rest of the system
-- the reduction rule of S is hardwired
-- as are the names of the constructors (S and @)

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import FAdet

import Reuse


import Trace

-- rationale: if there is a redex S x y z, we build x z (y z)
-- since the automaton is deterministic, the states for
-- xz, yz, xz(yz) should already be there,
-- and the state for Sxyz should conincide with that for xy(yz)
-- really (?)


redexes :: Opts -> BDFA Int -> [(Int, (Int, Int, Int, Int))]
redexes opts b @ (BDFA cons all starts moves) =
    let	
	ls = fmToList moves

	stname = tconname . stcon

	sxyzs = [ (stateSXYZ, (stateS, xr, yr, zr))
		| (h, stateS) <- ls, stname h == "S"

		, (i, stateSX) <- ls, stname i == "@"
		, let [xl, xr] = stargs i, xl == stateS

		, (j, stateSXY) <- ls, stname j == "@"
		, let [yl, yr] = stargs j, yl == stateSX

		, (k, stateSXYZ) <- ls, stname k == "@"
		, let [zl, zr] = stargs k, zl == stateSXY
		]

    in
	sxyzs 




checkredexes :: Opts -> BDFA Int 
	-> [(Int, (Int, Int, Int, Int))] -> [(Int, Int)]
-- produces list of epsilon moves that should be added
checkredexes opts (BDFA cons all starts moves) reds =
    let
	-- this is a bit ugly
	-- need to find the complete id information for the constructors
	-- we hope they are there
	ap = head [ con | con <- setToList cons, tconname con == "@" ]
	s  = head [ con | con <- setToList cons, tconname con == "S" ]
	
	checkred (top, (s, x, y, z)) =
	    let mflag = do
		xz <- lookupFM moves (mksterm ap [x, z])
		yz <- lookupFM moves (mksterm ap [y, z])
		xzyz <- lookupFM moves (mksterm ap [xz, yz])
		return (top, xzyz)
	    in
		exists mflag && the mflag == True

    in	and [ checkred r | r <- reds ]

	



closeS :: Opts -> TNFA Int -> TNFA Int
-- returns approximation to image under one-step reduction
closeS opts a @ (TNFA cons all starts moves) =
    let	
	b = tnfa2bdfa opts a

	reds = redexes opts b
	f = checkredexes opts b reds 

	r = if f then a else emptyTNFA

    in

	trace ("closeS.a: " ++ show a ++ "\n") $
	trace ("closeS.b: " ++ show b ++ "\n") $
	trace ("closeS.redexes: " ++ show reds ++ "\n") $


	trinfo opts "closeS" r $

	r


closeSpublic :: Opts -> [ TNFA Int ] -> TNFA Int

closeSpublic opts args =
    if length args /= 1 
    then error "closeSpublic.args"
    else 
	let [arg1] = args
	in  closeS opts arg1




