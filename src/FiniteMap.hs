{-# OPTIONS -fno-monomorphism-restriction #-}

-- | impedance matching:
-- provide old Data.FiniteMap interface
-- but use new Data.Map implementation

module FiniteMap

where

import Data.Map hiding ( foldr )

type FiniteMap k a = Map k a


emptyFM = empty
isEmptyFM = Data.Map.null
sizeFM = size
unitFM = singleton
eltsFM = elems
keysFM = keys
fmToList = assocs
listToFM = fromList
delFromFM = flip delete
foldFM = foldWithKey

-- | explicit signature is necessary
-- because the new type would not use Maybe but Monad m => m instead
lookupFM :: Ord k => FiniteMap k a -> k -> Maybe a
lookupFM = flip Data.Map.lookup

lookupWithDefaultFM f a k = findWithDefault a k f
elemFM = member
mapFM = mapWithKey
addToFM fm k a = insert k a fm
addToFM_C fun m k a = insertWith fun k a m


addListToFM m pairs = foldr (\ (k,a) -> insert k a) m pairs
addListToFM_C fun m pairs = foldr (\ (k, a) -> insertWith fun k a) m pairs

-- better performance according to <http://www.haskell.org/pipermail/libraries/2005-February/003341.html>
-- BUT these are not equivalent!
-- addListToFM m = union m . fromList
-- addListToFM_C fun m = unionWith fun m . fromList

plusFM = flip union -- watch out: Data.Map.union favours left side
plusFM_C = unionWith
filterFM = filterWithKey
intersectFM_C = intersectionWith
minusFM = Data.Map.difference
intersectFM = Data.Map.intersection
delListFromFM = foldr Data.Map.delete 
