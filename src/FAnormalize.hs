{-# language NoMonomorphismRestriction #-}

module FAnormalize 

( normalizeBDFA 
, normalizeTNFA
)

where


import FAmap
import FAtypes

import Ids
import Options

import FiniteMap
import Set

normalizeTNFA :: Ord a => Opts -> TNFA a -> TNFA Int
normalizeTNFA opts a @ (TNFA cons all starts moves) = 
    let  fm = listToFM $ zip ( setToList all ) [0 .. ]
         fun = lookupWithDefaultFM fm ( error "normalize" ) 
    in   mapTNFA opts fun a


normalizeBDFA :: Ord a => Opts -> BDFA a -> BDFA Int
normalizeBDFA opts a @ (BDFA cons all starts moves) = 
    let  fm = listToFM $ zip ( setToList all ) [0 .. ]
         fun = lookupWithDefaultFM fm ( error "normalize" ) 
    in   mapBDFA opts fun a


