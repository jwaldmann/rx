-- finite automata on trees (arbitrary term algebras)

module FA

( Auto



, fids		-- identifiers
, hsTNFA	-- possible default operations

-- above this line, eveyrthing is fine, abstract, and so on
-- below is some stuff that is exported 
-- because the module structure isn't quite right


, TNFA(..)	-- todo: make abstract


)

where

import Set
import FiniteMap

import Options	-- may modify behaviour

import Sorters


import TA -- term algebra

import Ids
import Syntax

import Stuff

import FAtypes
import FAconv

import FAuseful
import FAnormalize
import FAunify
import FAmerge

import FAdet
import FAmin

import FAunion
import FAintersect
import FAcon
import FAminus

import FAtimes
import FAstar
import FArquotient
import FAlquotient


import BackS
import ForS
import Onestep
--import ForwardS
--import CForwardS
--import BackwardS
--import CBackwardS
--import SaturnS
--import Instance
--import CloseS

----------------------------------------------------------------------------



fids :: [ (Id, Opts -> [TNFA Int] -> TNFA Int) ]
fids =
 	[ 	( mkid "++" (Passive "++") (Just 2) Op Op (Just 30) Lft
		, \ opts -> foldl1 (unionTNFA opts) )

		-- cannot use "--" because that's a comment
	, 	( mkid "\\\\" (Passive "\\\\") (Just 2) Op Op (Just 40) Lft
		, \ opts -> foldr1 (minusTNFA opts) )

	, 	( mkid "&"  (Passive "&") (Just 2) Op Op (Just 50) Lft
		, \opts -> foldl1 (intersectTNFA opts) )

	, 	( mkid "->" (Passive "\\longrightarrow") (Just 2) Op Op (Just 20) Lft
		, error "never evaluate fids.(->)" )

	, 	( mkid ";"  (Passive ";") (Just 2) Op Op (Just 10) Lft
		-- todo: this is the wrong place
		, error "never evaluate (;)" )
	, 	( mkid "=" (Passive "=") (Just 2) Op Op (Just 15) Lft
		-- todo: this is the wrong place
		, error "never evaluate (=)" )

	,	( userfun 1 "det"
		, \ opts [x] -> detTNFA opts x )
	,	( userfun 1 "min"
		, \ opts [x] -> minTNFA opts x )
	,	( userfun 1 "useful" 
		, \ opts [x] -> usefulTNFA opts x )
	,	( userfun 1 "normalize" 
		, \ opts [x] -> normalizeTNFA opts x )
	,	( userfun 1  "unify"
		, \ opts [x] -> unifyTNFA opts x )
	,	( userfun 1  "merge"
		, \ opts [x] -> mergeTNFA opts x )

	,	( userfun 3 "times"
		, \ opts xs -> timesTNFApublic opts xs )
	,	( userfun 3 "star"
		, \ opts xs -> starTNFApublic opts xs )
	,	( userfun 3 "rquotient"
		, \ opts xs -> rquotientTNFApublic opts xs )
	,	( userfun 3 "lquotient"
		, \ opts xs -> lquotientTNFApublic opts xs )

	,	( userfun 1 "backS"
		, \ opts xs -> backSpublic opts xs )

	,	( userfun 1 "error"
		, \ opts xs -> error $ unlines $ map show xs )


	,	( userfun 1 "forS"
		, \ opts xs -> forSpublic opts xs )
	,	( userfun 1 "onestep"
		, \ opts xs -> onesteppublic opts xs )


{-
-- broken
	,	( userfun 1 "closeS"
		, \ opts xs -> closeSpublic opts xs )
-}


{-
	,	( userfun 1 "saturnS"
		, \ opts xs -> saturnSpublic opts xs )

	,	( userfun 1 "backwardS"
		, \ opts xs -> backwardSpublic opts xs )

	,	( userfun 1 "forwardS"
		, \ opts xs -> forwardSpublic opts xs )
	,	( userfun 1 "cforwardS"
		, \ opts xs -> cforwardSpublic opts xs )

	,	( userfun 1 "cbackwardS"
		, \ opts xs -> cbackwardSpublic opts xs )


	,	( userfun 1 "inst"
		, \ opts xs -> instpublic opts xs )

-}


	
	]

-- some transformations (that keep the meaning)
-- most imortant (costly) first
hsTNFA = ["merge","min","det","useful","unify"]



