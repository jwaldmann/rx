module BackS

( backS
, backSpublic
)

-- check local rewrite closure:
-- for any state that produces reduct  x z(y z)
-- check that it also produces redex  S x y z.

-- if not, return empty automaton

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import Ids

import Reuse

import Monad ( guard )

-- | look for all matches of x z (y z) 
backS :: Opts -> TNFA Int -> TNFA Int
backS opts a @ (TNFA cons all starts moves) =
    let	
	-- this is a bit ugly
	-- need to find the complete id information for the constructors
	-- we hope they are there
	ap = head [ con | con <- setToList cons, tconname con == "@" ]
	s  = head [ con | con <- setToList cons, tconname con == "S" ]

	imoves = invert moves

	reducts_without_redexes = do
            t <- setToList all
            [l, r] <- sons a ap t
            [x, z] <- sons a ap l
	    [y, z'] <- sons a ap r
            guard $ z == z'	-- these are the two z's
            guard $ null $ do
                    s <- setToList $ lookupset imoves $ mksterm s []
                    sx <- setToList $ lookupset imoves $ mksterm ap [s, x]
                    sxy <- setToList $ lookupset imoves $ mksterm ap [sx, y]
                    sxyz <- setToList $ lookupset imoves $ mksterm ap [sxy, z]
                    guard $ t == sxyz
                    return ()
            return (t, x, y, z)

        r = if null reducts_without_redexes 
             then a 
             else error $ unlines $ "missing redexes for" : map show reducts_without_redexes
    in

	trinfo opts "backS" r r


backSpublic :: Opts -> [ TNFA Int ] -> TNFA Int

backSpublic opts args =
    if length args /= 1 
    then error "backSpublic.args"
    else 
	let [arg1] = args
	in  backS opts arg1



-- later:

-- iterate the backS operation
-- making the automaton deterministic and minimal
-- before and after each step
-- until process converges

-- making determin. should ensure that the two z's really "are the same"
