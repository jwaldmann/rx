module Onestep

( onestep
, onesteppublic
)

-- compute exact image under one-step rewriting w.r.t. S x y z -> x z (y z)

where

import Set
import FiniteMap

import Stuff
import Options

import TA
import FAtypes
import FAmap
import FAnormalize

import Ids

import Reuse

import Monad ( guard )
import List ( inits, tails )

data State = Orig !Int | Join !Int !Int | Copy !Int
    deriving ( Eq, Ord, Show )

-- | look for all matches of x z (y z) 
onestep :: Opts -> TNFA Int -> TNFA Int
onestep opts a @ (TNFA cons all starts moves) =
    let	
	-- this is a bit ugly
	-- need to find the complete id information for the constructors
	-- we hope they are there
	apcon = head [ con | con <- setToList cons, tconname con == "@" ]
	scon  = head [ con | con <- setToList cons, tconname con == "S" ]

        produce_s = mkSet $ do (p, t) <- expand moves ; guard $ stcon t == scon ; return p

        imoves = invert moves

        join x y = case ( setToList $ lookupset imoves $ mksterm apcon [x,y] ) of
            [ ] -> ( Join x y, [ (Join x y,  mksterm apcon $ map Orig [ x, y ] ) ] )
            [q] -> ( Orig q, [] )
            qs  -> error $ "not deterministic " ++ show (x,y, qs)

	rewrite = do
            top <- setToList all
            [sxy, z] <- sons a apcon top
            [sx, y] <- sons a apcon sxy
            [s, x] <- sons a apcon sx
            -- [] <- sons a scon s
            guard $ s `elementOf` produce_s
            let ( xz , helpxz ) = join x z 
                ( yz , helpyz ) = join y z 
            ( Copy top , mksterm apcon [ xz, yz ] ) : helpxz ++ helpyz

        orig = do 
            ( p, t ) <- expand moves
            return ( Orig p
                   , mksterm ( stcon t ) $ map Orig $ stargs t
                   )

        link = do
            ( p, t ) <- expand moves
            ( pre, Orig q : post ) <- splits $ map Orig $ stargs t
            return ( Copy p
                   , mksterm ( stcon t ) $ pre ++ Copy q : post
                   )

        transitions = collect $ orig ++ link ++ rewrite

        b = TNFA cons
              ( allstates transitions )
              ( mapSet Copy starts )
              transitions

        r = normalizeTNFA opts b

    in

	trinfo opts "onestep" r r

allstates :: Ord a => FiniteMap a (Set ( STerm a )) -> Set a
allstates moves = mkSet $ do
    (x, t) <- expand moves
    x : stargs t

------------------------------------------------------------------

onesteppublic :: Opts -> [ TNFA Int ] -> TNFA Int

onesteppublic opts args =
    if length args /= 1 
    then error "onesteppublic.args"
    else 
	let [arg1] = args
	in  onestep opts arg1




