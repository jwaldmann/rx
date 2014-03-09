module Reader

( drg 	-- reads a grammar from a string
	-- returns corresponding deterministic automaton
, trg	--                       non-
)

where

import ExpParse (pline)

import FiniteMap

import Options
import Defaults

import Ids
import IdStack
import Gen

import FA
import FAtypes
import FAdet
import Gram2FA

import Syntax
import Semantik

--------------------------------------------------------------------

trg :: String -> TNFA Int
trg cs = 
    let
	(Just x, _) = pline (opts0, genpid) cs
	g = docomp opts0 genenv x
		
    in
	g

drg = t2d opts0 . trg

