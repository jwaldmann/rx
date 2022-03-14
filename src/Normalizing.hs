module Normalizing 


where

import FAtypes
import TA

import FAdet
import FAconv
import FAminus
import FAunion
import FAmin
import FAuseful
import FAnormalize

import ForS
import Onestep

import Ids
import Syntax
import Stuff
import Options

import qualified Data.Map as M
import qualified Data.Set as S

import Set
import FiniteMap

import Control.Monad ( guard )

import Text.PrettyPrint.HughesPJ

import Data.Graph

components :: BDFA Int -> [SCC Int]
components a  = stronglyConnComp $ do
    let b @ (TNFA cons all starts moves) = bdfa2tnfa opts a
    ( p, ts ) <- fmToList moves
    return ( p, p, concat $ map stargs $ setToList ts )

{-
instance Show v => Show (SCC v) where
    show c = case c of
        AcyclicSCC v -> "A " ++ show v
        CyclicSCC vs -> "C " ++ show vs
-}

labelled :: BDFA Int -> Doc
labelled a @ (BDFA cons all starts moves) = vcat
    [ parens $ text "VAR x y z"
    , parens $ vcat $ text "RULES" : ( redex_reduct_pairs $ bdfa2tnfa opts a )
    ]


form :: BDFA Int -> Doc
form a @ (BDFA cons all starts moves) = vcat
    [ text "bottom up tree automaton"
    , nest 4 $ vcat 
        [ text "states" <+> equals <+> text ( show all )
        , text "final" <+> equals <+> text ( show starts )
        , text "trans" <+> equals <+> fsep  ( punctuate comma $ do
              ( t, q ) <- fmToList moves
              return $ hsep [ case ( tconname $ stcon t, stargs t ) of
                                ( "S" , [] )  -> text "S"
                                ( "@" , [x,y] ) ->
                                    hsep [ text ( show x), text ( show y) ]
                            , text "->"
                            , text ( show q )
                            ]
        ) ]
    ]


test1 = redexes_without_reducts min_convergent
test2 = redexes_without_reducts min_divergent


ah = usercon 2 "@"
es = usercon 0 "S"

s = mksterm es
a = mksterm ah

opts = listToOpts [ ("trace", "off") ]

min_convergent = normalizeTNFA opts $  minTNFA opts convergent
min_divergent  = normalizeTNFA opts $  minTNFA opts divergent

conv = normalizeBDFA opts $ minBDFA opts $ tnfa2bdfa opts convergent
divt = normalizeBDFA opts $ minBDFA opts $ tnfa2bdfa opts divergent

-----------------------------------------------------------------------------

write_sccs = sequence_ $ do
    ( k, CyclicSCC rules ) <- zip [0..] $ dp_sccs $ bdfa2tnfa opts conv 
    return $ do
        let fname = "cls-" ++ show k ++ ".scc"
            contents = show $ vcat
                [ parens $ text "VAR x y z"
                , parens $ text "RULES" <+> vcat ( map remit rules )
                ]
        writeFile fname contents
        putStrLn fname

emit_dp_sccs a = vcat $ do
    comp <- dp_sccs a
    return $ case comp of
        AcyclicSCC rule -> text "acyclic:" <+> remit rule
        CyclicSCC rules -> text "cyclic :" <+> vcat ( map remit rules )

remit :: ( Exp, Exp ) -> Doc
remit ( lhs, rhs ) = hsep [ emit lhs, text "->", emit rhs ]

emit :: Exp ->  Doc
emit (App fun args) = text ( show fun ) <> case args of
     [] -> empty
     ts -> parens $ hsep $ punctuate comma $ map emit args
     
dp_sccs a = stronglyConnComp $ do
    let dps = dependency_pairs a
	stcon (App fun args) = fun
        dp_by_lhs = addListToFM_C unionSet emptyFM $ do
            rule @ ( l, r ) <- dps
            return ( stcon l, mkSet [ rule ] )
        dp_by_rhs = addListToFM_C unionSet emptyFM $ do
            rule @ ( l, r ) <- dps
            return ( stcon r, mkSet [ rule ] )
        common = intersectSet ( mkSet $ keysFM dp_by_lhs )
                              ( mkSet $ keysFM dp_by_rhs )
        rel = addListToFM_C unionSet emptyFM $ do
                key <- setToList common
                rule1 <- setToList $ lookupset dp_by_rhs key
                rule2 <- setToList $ lookupset dp_by_lhs key
                return ( rule1, mkSet [ rule2 ]  )
    ( p, qs ) <- fmToList rel
    return ( p, p, setToList qs )

dependency_pairs a @ (TNFA cons all starts moves) = do
    let	
	ap = head [ con | con <- setToList cons, tconname con == "@" ]
	s  = head [ con | con <- setToList cons, tconname con == "S" ]

	imoves = invert moves

        produce_s = mkSet $ do 
            (p, t) <- expand moves ; guard $ stcon t == s ; return p

        join x y = case ( setToList $ lookupset imoves $ mksterm ap [x, y] ) of
            [ p ] -> p
            ps -> error $ "not deterministic or not complete " ++ show (x,y,ps)
    top <- setToList all
    [sxy, z] <- sons a ap top
    [sx, y] <- sons a ap sxy
    [es, x] <- sons a ap sx
    guard $ es `elementOf` produce_s
    let xz = join x z
        yz = join y z
        top' = join xz yz
    if top /= top' then error "huh" else return ()
    
    let mksterm = App
	stcon (App fun args) = fun
	app top l r = usercon 2 $ concat $ intersperse "_"
                    [ "A", show top, show l, show r ]
        mkvar x = mksterm ( uservar x ) []
    let lhs = mksterm ( app top sxy z )
                      [ mksterm ( app sxy sx y )        
                                [ mksterm ( app sx es x )
                                          [ mksterm ( usercon 0 "S" ) []
                                          , mkvar "x"
                                          ]
                                , mkvar "y"
                                ]
                      , mkvar "z"
                      ]
        rhsxz = mksterm ( app xz x z ) [ mkvar "x", mkvar "z" ]
        rhsyz = mksterm ( app yz y z ) [ mkvar "y", mkvar "z" ]
        rhs   = mksterm ( app top xz yz ) [ rhsxz, rhsyz ]
    rhs <- [ rhsxz, rhsyz, rhs ]
    return ( lhs, rhs )



redex_reduct_pairs a @ (TNFA cons all starts moves) = do
    let	
	ap = head [ con | con <- setToList cons, tconname con == "@" ]
	s  = head [ con | con <- setToList cons, tconname con == "S" ]

	imoves = invert moves

        produce_s = mkSet $ do 
            (p, t) <- expand moves ; guard $ stcon t == s ; return p

        join x y = case ( setToList $ lookupset imoves $ mksterm ap [x, y] ) of
            [ p ] -> p
            ps -> error $ "not deterministic or not complete " ++ show (x,y,ps)
    top <- setToList all
    [sxy, z] <- sons a ap top
    [sx, y] <- sons a ap sxy
    [es, x] <- sons a ap sx
    guard $ es `elementOf` produce_s
    let xz = join x z
        yz = join y z
        top' = join xz yz
    if top /= top' then error "huh" else return ()

    let patch p x = {- if p == es then text "S" else -} x
        mkapp top (p, l) (q, r) = hsep
            [ hcat $ punctuate ( text "_" ) 
                   $ map text [ "A", show top, show p, show q ]
            , parens $ hsep $ punctuate comma 
                     [ patch p l, patch q r ]
            ]
        mks = text "S" 
        mkvar x = text x

    return $ mkrule ( mkapp top
                            ( sxy, mkapp sxy
                                         ( sx, mkapp sx 
                                                     ( es , mks ) 
                                                     ( x, mkvar "x" ))
                                         ( y, mkvar "y" ))
                            ( z, mkvar "z" ))
                    ( mkapp top
                            ( xz, mkapp xz
                                        ( x, mkvar "x" )
                                        ( z, mkvar "z" ))
                            ( yz, mkapp yz
                                        ( y, mkvar "y" )
                                        ( z, mkvar "z" )) )

mkrule l r = hsep [ l, text "->", r ]


-----------------------------------------------------------------------------

redexes_without_reducts a @ (TNFA cons all starts moves) = do
    let	
	-- this is a bit ugly
	-- need to find the complete id information for the constructors
	-- we hope they are there
	ap = head [ con | con <- setToList cons, tconname con == "@" ]
	s  = head [ con | con <- setToList cons, tconname con == "S" ]

	imoves = invert moves

        produce_s = mkSet $ do 
            (p, t) <- expand moves ; guard $ stcon t == s ; return p

        join x y = case ( setToList $ lookupset imoves $ mksterm ap [x, y] ) of
            [ p ] -> p
            ps -> error $ "not deterministic or not complete " ++ show (x,y,ps)

    do
            top <- setToList all
            [sxy, z] <- sons a ap top
            [sx, y] <- sons a ap sxy
            [es, x] <- sons a ap sx
            guard $ es `elementOf` produce_s
            let xz = join x z
                yz = join y z
                top' = join xz yz
            guard $ top /= top'
            return (es, x, y, z, top, top')





divergent = usefulTNFA opts 
           $ bdfa2tnfa opts
           $ minusBDFA opts star normBD

star = BDFA ( S.fromList [ah,es] )
            ( S.fromList [0] )
            ( S.fromList [0] )
            ( M.fromList [ (s [], 0 ), (a [0,0], 0) ] )

normBD :: BDFA Int
normBD =  tnfa2bdfa undefined convergent

normalizingD :: TNFA Int
normalizingD = bdfa2tnfa undefined normBD





convergent :: TNFA Int
convergent = TNFA 
    (S.fromList [ah,es]) 
    (S.fromList [0..88]) 
    (S.fromList [0,1,2,3,7,14,16,22,42,58,79,88]) 
    (M.fromList [(0,S.fromList [a [13,82],a [25,85],a [68,13],a [78,68],a [82,86],s[]]),(1,S.fromList [a [13,82],a [68,13],a [78,68],a [82,86],s[]]),(2,S.fromList [a [68,13]]),(3,S.fromList [a [78,68]]),(4,S.fromList [a [4,85],a [84,61]]),(5,S.fromList [a [5,85],a [84,6]]),(6,S.fromList [a [5,85],a [84,6],a [85,85],s[]]),(7,S.fromList [a [13,82]]),(8,S.fromList [a [8,85],a [9,85],a [84,8]]),(9,S.fromList [a [11,85],a [84,9],a [84,85]]),(10,S.fromList [a [84,10],a [85,8],a [85,9],a [85,11],a [85,84]]),(11,S.fromList [a [84,11],a [84,84]]),(12,S.fromList [a [10,85],a [84,12]]),(13,S.fromList [a [20,87],a [51,13],a [57,84],a [76,85],a [85,86],s[]]),(14,S.fromList [a [25,85]]),(15,S.fromList [a [5,85],a [10,85],a [11,85],a [84,6],a [84,9],a [84,12],a [84,17],a [84,73],a [84,85],a [85,51],a [85,57],a [85,85],s[]]),(16,S.fromList [a [68,13],a [78,68],a [82,86],s[]]),(17,S.fromList [a [84,17],a [85,57]]),(18,S.fromList [a [84,18],a [85,57]]),(19,S.fromList [a [20,84],a [84,19]]),(20,S.fromList [a [85,80]]),(21,S.fromList [a [85,24]]),(22,S.fromList [a [26,84]]),(23,S.fromList [a [23,85],a [84,24]]),(24,S.fromList [a [23,85],a [84,24],a [85,85],s[]]),(25,S.fromList [a [21,25],a [27,85],a [36,85],a [59,76],a [60,73],a [62,84],a [64,66],a [67,70],a [71,73],a [72,73],a [74,80],a [75,61],a [84,37],a [85,88],s[]]),(26,S.fromList [a [15,85],a [18,84],a [51,26],a [57,87],a [85,86],s[]]),(27,S.fromList [a [28,85],a [34,85],a [45,49],a [50,84],a [71,53],a [72,55],a [84,27],a [84,35],a [85,56],a [85,76],a [85,85],s[]]),(28,S.fromList [a [29,85],a [32,85],a [41,44],a [71,74],a [72,74],a [84,28],a [84,33],a [85,39],a [85,40],a [85,85],s[]]),(29,S.fromList [a [30,85],a [41,84],a [84,29],a [84,31],a [85,74],a [85,85],s[]]),(30,S.fromList [a [30,85],a [84,31]]),(31,S.fromList [a [30,85],a [84,31],a [85,85],s[]]),(32,S.fromList [a [32,85],a [84,33]]),(33,S.fromList [a [32,85],a [84,33],a [85,85],s[]]),(34,S.fromList [a [34,85],a [84,35]]),(35,S.fromList [a [34,85],a [84,35],a [85,85],s[]]),(36,S.fromList [a [36,85],a [84,37]]),(37,S.fromList [a [36,85],a [84,37],a [85,85],s[]]),(38,S.fromList [a [38,85],a [84,39]]),(39,S.fromList [a [38,85],a [84,39],a [85,85],s[]]),(40,S.fromList [a [84,40],a [85,84]]),(41,S.fromList [a [85,74]]),(42,S.fromList [a [68,13],a [82,86],s[]]),(43,S.fromList [a [43,85],a [84,44]]),(44,S.fromList [a [43,85],a [84,44],a [85,85],s[]]),(45,S.fromList [a [85,47]]),(46,S.fromList [a [46,85],a [84,47]]),(47,S.fromList [a [46,85],a [84,47],a [85,85],s[]]),(48,S.fromList [a [48,85],a [84,49]]),(49,S.fromList [a [48,85],a [84,49],a [85,85],s[]]),(50,S.fromList [a [80,84]]),(51,S.fromList [a [85,84],a [85,85]]),(52,S.fromList [a [52,85],a [84,53]]),(53,S.fromList [a [52,85],a [84,53],a [85,85],s[]]),(54,S.fromList [a [54,85],a [84,55]]),(55,S.fromList [a [54,85],a [84,55],a [85,85],s[]]),(56,S.fromList [a [84,56],a [85,57]]),(57,S.fromList [a [5,85],a [84,6],a [84,73],a [85,51],a [85,85],s[]]),(58,S.fromList [a [82,86],s[]]),(59,S.fromList [a [85,73]]),(60,S.fromList [a [85,83]]),(61,S.fromList [a [4,85],a [84,61],a [85,85],s[]]),(62,S.fromList [a [20,84],a [77,85],a [84,19],a [84,83]]),(63,S.fromList [a [63,85],a [84,64]]),(64,S.fromList [a [63,85],a [84,64],a [85,85],s[]]),(65,S.fromList [a [65,85],a [84,66]]),(66,S.fromList [a [65,85],a [84,66],a [85,85],s[]]),(67,S.fromList [a [80,84],a [84,67]]),(68,S.fromList [a [73,85],a [84,68]]),(69,S.fromList [a [69,85],a [84,70]]),(70,S.fromList [a [69,85],a [84,70],a [85,85],s[]]),(71,S.fromList [a [74,85]]),(72,S.fromList [a [84,84]]),(73,S.fromList [a [84,73],a [85,51]]),(74,S.fromList [a [84,85]]),(75,S.fromList [a [84,75],a [85,76]]),(76,S.fromList [a [5,85],a [10,85],a [11,85],a [84,6],a [84,9],a [84,12],a [84,73],a [84,85],a [85,51],a [85,85],s[]]),(77,S.fromList [a [84,77],a [85,84]]),(78,S.fromList [a [80,80],a [81,84],a [83,85],a [84,78]]),(79,S.fromList [a [82,86]]),(80,S.fromList [a [85,84]]),(81,S.fromList [a [84,85],a [85,84]]),(82,S.fromList [a [61,85],a [84,82],a [85,86],a [87,84],s[]]),(83,S.fromList [a [77,85],a [84,83]]),(84,S.fromList [a [85,85]]),(85,S.fromList [s[]]),(86,S.fromList [a [13,82],a [25,85],a [26,84],a [68,13],a [78,68],a [82,86],s[]]),(87,S.fromList [a [85,85],s[]]),(88,S.fromList [a [13,82],a [25,85],a [26,84],a [68,13],a [78,68],a [82,86],s[]])])

