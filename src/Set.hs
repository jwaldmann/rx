{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts -fallow-undecidable-instances -fallow-incoherent-instances -fno-monomorphism-restriction #-}

module Set

where

--   $Id: Set.hs,v 1.25 2006-10-22 22:04:22 joe Exp $

import qualified Data.Set 

type Set = Data.Set.Set

isEmptySet = Data.Set.null

emptySet = Data.Set.empty
unitSet = Data.Set.singleton

delFromSet = flip Data.Set.delete
addToSet = flip Data.Set.insert
elementOf = Data.Set.member

cardinality = Data.Set.size

unionSet = Data.Set.union
unionManySets = Data.Set.unions

intersectSet = Data.Set.intersection
intersectManySets = foldr1 Data.Set.intersection

minusSet = Data.Set.difference

mkSet :: Ord a => [a] -> Set a
mkSet = Data.Set.fromList

setToList :: Set a -> [a]
setToList = Data.Set.toAscList

subseteq :: Ord a => Set a -> Set a -> Bool
subseteq xs ys = Data.Set.null $ xs `Data.Set.difference` ys

mapSet :: (Ord a, Ord b) => (a -> b) -> (Set a -> Set b)
mapSet = Data.Set.map

filterSet :: Ord a => (a -> Bool) -> (Set a -> Set a)
filterSet = Data.Set.filter

nonempty :: Ord a => Set a -> Bool
nonempty = not . Data.Set.null 

cross :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
cross xs ys = Data.Set.fromList $ do 
    x <- Data.Set.toList xs; y <- Data.Set.toList ys; return (x, y)

 
 
