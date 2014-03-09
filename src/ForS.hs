module ForS

( forS
, forSpublic
)

-- check local rewrite closure ("syntactic")
-- for any state that produces redex  S x y z
-- check that it also produces reduct  x z(y z)

-- if not, error.

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

forS :: Opts -> TNFA Int -> TNFA Int
forS opts a @ (TNFA cons all starts moves) =
    let	
	-- this is a bit ugly
	-- need to find the complete id information for the constructors
	-- we hope they are there
	ap = head [ con | con <- setToList cons, tconname con == "@" ]
	s  = head [ con | con <- setToList cons, tconname con == "S" ]

	imoves = invert moves

        produce_s = mkSet $ do 
            (p, t) <- expand moves ; guard $ stcon t == s ; return p

	redexes_without_reducts = do
            top <- setToList all
            [sxy, z] <- sons a ap top
            [sx, y] <- sons a ap sxy
            [es, x] <- sons a ap sx
            guard $ es `elementOf` produce_s
            guard $ null $ do
                    xz <- setToList $ lookupset imoves $ mksterm ap [x, z]
                    yz <- setToList $ lookupset imoves $ mksterm ap [y, z]
                    let xzyzs = lookupset imoves $ mksterm ap [xz, yz]
                    guard $ top `elementOf` xzyzs
            return (es, x, y, z, top)

        r = if null redexes_without_reducts
             then a 
             else error $ unlines 
                        $ "missing reducts for" 
                        : map show redexes_without_reducts
    in

	trinfo opts "forS" r r


forSpublic :: Opts -> [ TNFA Int ] -> TNFA Int

forSpublic opts args =
    if length args /= 1 
    then error "forSpublic.args"
    else 
	let [arg1] = args
	in  forS opts arg1



-- later:

-- iterate the forS operation
-- making the automaton deterministic and minimal
-- before and after each step
-- until process converges

-- making determin. should ensure that the two z's really "are the same"
