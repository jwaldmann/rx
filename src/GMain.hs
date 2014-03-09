module GMain 

( gmain
, main
)

where

import FA (hsTNFA)

import PrettyClass -- ??
import Syntax -- hbc wants this, to get the instance PrettyClass Exp 

import Heave 
import Write
import Loop

import Gen
import Defaults

gmain :: Write m => String -> m ()
-- give argument string, as you would on the command line
gmain args = gheave opts0 
	(expformat hsTNFA genval) (genpid, genenv) args

main :: IO ()
-- reads command line
main = heave opts0 
	(expformat hsTNFA genval) (genpid, genenv)

