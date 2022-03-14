module Stuff

( bind
, fixpoint
, sethull
, zippy
, lookupset
, invert
, packs
, insts
, collectFM

, exists
, the

, inits
, tails
, splits
, intersperse

, partition

, expand, collect
)

where

import Set
import FiniteMap

import List (inits, tails)

-- hbc complains

{-  # SPECIALIZE instance Eq (Set Int) #-}
{-  # SPECIALIZE instance Ord (Set Int) #-}

{-  # SPECIALIZE instance Eq (Set (Int, Int)) #-}
{-  # SPECIALIZE instance Ord (Set (Int, Int)) #-}


exists (Just _) = True
exists Nothing = False

the (Just x) = x
the _ = error "the"



bind :: Ord b => Set a -> (a -> Set b) -> Set b
-- looks familiar? could be a monad, eh?
s `bind` f = unionManySets (map f (setToList s))

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x = 
    let	y = f x
    in 	if x == y then x else fixpoint f y

zippy :: [a] -> [b] -> [(a,b)]
-- checks that args have equal length
zippy [] [] = []
zippy (x : xs) (y : ys) = (x,y) : zippy xs ys
zippy _ _ = error "zippy: unequal lengths"

sethull :: Ord a => (a -> Set a) -> Set a -> Set a
sethull f init = sh emptySet init 
    where
    	sh known unknown | isEmptySet unknown = known
	sh known unknown = 
	    let	xs = unknown `bind` f
		uk = known `unionSet` unknown
		ys = xs `minusSet` uk
	    in	sh uk ys

-- returns empty set as default
lookupset m x = lookupWithDefaultFM m emptySet x



invert :: (Ord a, Ord b) => FiniteMap a (Set b) -> FiniteMap b (Set a)
invert fab = 
    addListToFM_C unionSet emptyFM 
	[(y,unitSet x)|(x,ys) <- fmToList fab, y <- setToList ys]


partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p [] = ([], [])
partition p (x : xs) = 
    let (as, bs) = partition p xs
    in	if p x then (x : as, bs) else (as, x : bs)



packs :: Int -> Int -> [a] -> [a] -> [[a]]
-- packs n m xs ys = all list of length n 
-- whose elements are in xs ++ ys
-- with at least m from ys
packs 0 _ _ _   = [[]]
packs n m xs ys = [ h : t | n > m, t <- packs (n - 1) m       xs ys, h <- xs ]
	       ++ [ h : t |        t <- packs (n - 1) (m - 1) xs ys, h <- ys ]

insts :: Ord a => [Set a] -> Set [a]
-- all instances of a given list whose elements are sets
insts []       = unitSet []
insts (x : xs) = insts xs `bind` \ t -> mapSet (\ h -> h : t) x

-- intersperse sep inserts sep between the elements of its list argument.
-- e.g. intersperse ',' "abcde" == "a,b,c,d,e"
intersperse            :: a -> [a] -> [a]
intersperse sep []      = []
intersperse sep [x]     = [x]
intersperse sep (x:xs)  = x : sep : intersperse sep xs


collectFM :: Ord a => [a] -> FiniteMap a Int
-- collect elements, count them
-- duplicates get same number
-- but beware: numbers are not used contiguously
collectFM xs = addListToFM_C
	(\ x old -> old)	-- already there, don't overwrite
	emptyFM
	(zip xs [0..])		-- count them



------------------------------------------------------------------

expand :: ( Ord a, Ord b ) 
       => FiniteMap a (Set b) -> [(a,b)]
expand fm = do (x, ys) <- fmToList fm; y <- setToList ys; return (x,y)

collect ::  ( Ord a, Ord b ) 
       => [(a,b)] -> FiniteMap a (Set b)
collect xys = addListToFM_C unionSet emptyFM $ do 
    (x, y) <- xys ; return (x, unitSet y)


splits :: [a] -> [([a],[a])]
splits xs = zip (inits xs) ( tails xs )


