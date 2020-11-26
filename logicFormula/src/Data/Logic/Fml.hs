module Data.Logic.Fml (
  -- * Type
  Fml (..)

  -- * Querying
  , depth
  , vars

  -- * Formatting
  , prettyFormat

  -- * Transforming
  , toNNF
  , toCNF
  , toCCNF
  , toDNF
  , toUniversalNAnd
  , toUniversalNOr

  -- * Testing
  , isNNF
  , isCNF
  , isCCNF
  , isDNF
  --, isUniversalNAnd
  --, isUniversalNOr

) where

import qualified Data.Logic.Var as Var
import qualified Data.List      as L

data Fml a = And   (Fml a) (Fml a)
           | NAnd  (Fml a) (Fml a)
           | Or    (Fml a) (Fml a)
           | NOr   (Fml a) (Fml a)
           | XOr   (Fml a) (Fml a)
           | XNOr  (Fml a) (Fml a)
           | Imply (Fml a) (Fml a)
           | Equiv (Fml a) (Fml a)
           | Not   (Fml a)
           | Final (Var.Var a)
           deriving (Show)

-- ############  QUERYING  ############

-- |’prettyFormat’ @p@ return a string representation of the formula @p@.
-- and :        .
-- nand:        ~.
-- or:          +
-- nor:         ~+
-- xor:         x+
-- xnor:        x~+
-- imply:       =>
-- equivalence: <=>
-- not:         -

prettyFormat :: (Show a) => Fml a -> String
prettyFormat (And   p q) = "(" ++ prettyFormat p ++ " . "   ++ prettyFormat q ++ ")"
prettyFormat (NAnd  p q) = "(" ++ prettyFormat p ++ " ~. "  ++ prettyFormat q ++ ")"
prettyFormat (Or    p q) = "(" ++ prettyFormat p ++ " + "   ++ prettyFormat q ++ ")"
prettyFormat (NOr   p q) = "(" ++ prettyFormat p ++ " ~+ "  ++ prettyFormat q ++ ")"
prettyFormat (XOr   p q) = "(" ++ prettyFormat p ++ " x+ "  ++ prettyFormat q ++ ")"
prettyFormat (XNOr  p q) = "(" ++ prettyFormat p ++ " x~+ " ++ prettyFormat q ++ ")"
prettyFormat (Imply p q) = "(" ++ prettyFormat p ++ " => "  ++ prettyFormat q ++ ")"
prettyFormat (Equiv p q) = "(" ++ prettyFormat p ++ " <=> " ++ prettyFormat q ++ ")"
prettyFormat (Not   p)   = "-" ++ prettyFormat p
prettyFormat (Final v)   = show v

-- |’vars’ @p@ returns all variables that occur in formula @p@. Duplicate
--  occurrences are removed.
vars :: (Eq a) => Fml a -> [Var.Var a]
vars f = L.nub $ getVars f
  where
    getVars (Final p) = [p]
    getVars (Not   p)   = getVars p
    getVars (And   p q) = getVars p ++ getVars q
    getVars (NAnd  p q) = getVars p ++ getVars q
    getVars (Or    p q) = getVars p ++ getVars q
    getVars (NOr   p q) = getVars p ++ getVars q
    getVars (XOr   p q) = getVars p ++ getVars q
    getVars (XNOr  p q) = getVars p ++ getVars q
    getVars (Imply p q) = getVars p ++ getVars q
    getVars (Equiv p q) = getVars p ++ getVars q

-- |’depth’ @p@ returns the depth of fomula @p@.
depth :: (Num b, Ord b) => Fml a -> b
depth (Final p) = 0
depth (Not   p) = 1 + depth p
depth (And   p q) = 1 + max (depth p) (depth q)
depth (NAnd  p q) = 1 + max (depth p) (depth q)
depth (Or    p q) = 1 + max (depth p) (depth q)
depth (NOr   p q) = 1 + max (depth p) (depth q)
depth (XOr   p q) = 1 + max (depth p) (depth q)
depth (XNOr  p q) = 1 + max (depth p) (depth q)
depth (Imply p q) = 1 + max (depth p) (depth q)
depth (Equiv p q) = 1 + max (depth p) (depth q)

-- ############  TRANSFORMING  ############

-- |’toNNF’ @f@ converts the formula @f@ to NNF.
toNNF :: Fml a -> Fml a
toNNF = delDN . morgSimp . delCon

-- |’delCon’ @f@ simplify the formula @f@ so there is just Or, And, Not Fml.
-- NAND  p q = 'not' p 'or'  'not' q
-- NOR   p q = 'not' p 'and' 'not' q
-- XOR   p q = (p 'and' 'not' q) 'or' ('not' p 'and' q)
-- XNOR  p q = (p 'and' q) 'or' ('not' p 'and' 'not' q)
-- IMPLY p q = 'not' p 'or' q
-- EQUIV p q = p XNOR q
delCon :: Fml a -> Fml a
delCon f@(Final p) = f
delCon (Not   p)   = Not (delCon p)
delCon (And   p q) = And (delCon p) (delCon q)
delCon (Or    p q) = Or  (delCon p) (delCon q)
delCon (NAnd  p q) = Or  (Not (delCon p)) (Not (delCon q)) 
delCon (NOr   p q) = And (Not (delCon p)) (Not (delCon q)) 
delCon (XOr   p q) = Or (And (delCon p) (Not (delCon q))) (And (Not (delCon p)) (delCon q))
delCon (XNOr  p q) = Or (And (delCon p) (delCon q)) (And (Not (delCon p)) (Not (delCon q)))
delCon (Imply p q) = Or (Not (delCon p)) (delCon q)
delCon (Equiv p q) = delCon (XNOr p q)

-- |morgSimp @f@ simplify the formula with deMorgan rules.
-- 'not' (p 'and' q) = 'not' p 'or'  'not' q
-- 'not' (p 'or'  q) = 'not' p 'and' 'not' q
morgSimp :: Fml a -> Fml a
morgSimp (Not(And (p) (q))) = Or    (morgSimp (Not p)) (morgSimp (Not q))
morgSimp (Not(Or  (p) (q))) = And   (morgSimp (Not p)) (morgSimp (Not q))
morgSimp f@(Final p)        = f
morgSimp (Not   p)          = Not   (morgSimp p)
morgSimp (And   p q)        = And   (morgSimp p) (morgSimp q)
morgSimp (Or    p q)        = Or    (morgSimp p) (morgSimp q)
morgSimp (NAnd  p q)        = NAnd  (morgSimp p) (morgSimp q)
morgSimp (NOr   p q)        = NOr   (morgSimp p) (morgSimp q)
morgSimp (XOr   p q)        = XOr   (morgSimp p) (morgSimp q)
morgSimp (XNOr  p q)        = XNOr  (morgSimp p) (morgSimp q)
morgSimp (Imply p q)        = Imply (morgSimp p) (morgSimp q)
morgSimp (Equiv p q)        = Equiv (morgSimp p) (morgSimp q)

-- |delDN @f@ simplify the formula @f@ so there is no double negation anymore.
-- 'not' 'not' p = p
delDN :: Fml a -> Fml a
delDN f@(Final p)  = f
delDN (Not(Not p)) = delDN p
delDN (Not     p)  = Not   (delDN p)
delDN (And   p q)  = And   (delDN p) (delDN q)
delDN (Or    p q)  = Or    (delDN p) (delDN q)
delDN (NAnd  p q)  = NAnd  (delDN p) (delDN q)
delDN (NOr   p q)  = NOr   (delDN p) (delDN q)
delDN (XOr   p q)  = XOr   (delDN p) (delDN q)
delDN (XNOr  p q)  = XNOr  (delDN p) (delDN q)
delDN (Imply p q)  = Imply (delDN p) (delDN q)
delDN (Equiv p q)  = Equiv (delDN p) (delDN q)

-- |’toCNF’ @f@ converts the formula @f@ to CNF.
toCNF :: Fml a -> Fml a
toCNF = devConCNF . toNNF

-- |’toCNF’ @f@ converts the formula @f@ to CNF.
toDNF :: Fml a -> Fml a
toDNF = devConDNF . toNNF

-- |devConCNF @f@ develop the disjunction on conjunction connectors of the formula @f@.
-- left-associative
-- p 'or'  (q 'and' r) = (p 'or'  q) 'and' (p 'or'  r)
-- right-associative
-- (q 'and' r) 'or'  p = (q 'or'  p) 'and' (r 'or'  p)
devConCNF :: Fml a -> Fml a
devConCNF (Or  p (And q r))  = And (devConCNF (Or  p q)) (devConCNF (Or  p r))
devConCNF (Or  (And q r) p)  = And (devConCNF (Or  q p)) (devConCNF (Or  r p))
devConCNF f@(Final p)        = f
devConCNF (Not   p)          = Not   (devConCNF p)
devConCNF (And   p q)        = And   (devConCNF p) (devConCNF q)
devConCNF (Or    p q)        = Or    (devConCNF p) (devConCNF q)
devConCNF (NAnd  p q)        = NAnd  (devConCNF p) (devConCNF q)
devConCNF (NOr   p q)        = NOr   (devConCNF p) (devConCNF q)
devConCNF (XOr   p q)        = XOr   (devConCNF p) (devConCNF q)
devConCNF (XNOr  p q)        = XNOr  (devConCNF p) (devConCNF q)
devConCNF (Imply p q)        = Imply (devConCNF p) (devConCNF q)
devConCNF (Equiv p q)        = Equiv (devConCNF p) (devConCNF q)

-- |devConDNF @f@ develop the conjunction on disjunction connectors of the formula @f@.
-- left-associative
-- p 'and' (q 'or'  r) = (p 'and' q) 'or'  (p 'and' r)
-- right-associative
-- (q 'or'  r) 'and' p = (q 'and' p) 'or'  (r 'and' p)
devConDNF :: Fml a -> Fml a
devConDNF (And p (Or  q r)) = Or  (devConDNF (And p q)) (devConDNF (And p r))
devConDNF (And (Or  q r) p) = Or  (devConDNF (And q p)) (devConDNF (And r p))
devConDNF f@(Final p)       = f
devConDNF (Not   p)         = Not   (devConDNF p)
devConDNF (And   p q)       = And   (devConDNF p) (devConDNF q)
devConDNF (Or    p q)       = Or    (devConDNF p) (devConDNF q)
devConDNF (NAnd  p q)       = NAnd  (devConDNF p) (devConDNF q)
devConDNF (NOr   p q)       = NOr   (devConDNF p) (devConDNF q)
devConDNF (XOr   p q)       = XOr   (devConDNF p) (devConDNF q)
devConDNF (XNOr  p q)       = XNOr  (devConDNF p) (devConDNF q)
devConDNF (Imply p q)       = Imply (devConDNF p) (devConDNF q)
devConDNF (Equiv p q)       = Equiv (devConDNF p) (devConDNF q)

-- |’toCCNF’ @f@ converts the formula @f@ to CCNF.
toCCNF :: Fml a -> Fml a
toCCNF = toCNF

-- |factCNF @f@ factorise a CNF formula to CCNF @f@.
--factCNF :: Fml a -> Fml a
--factCNF (And   p q)

-- |’toUniversalNAnd’ @p@ returns a NAND-formula that is equivalent
-- to formula @p@.
--toUniversalNAnd :: Fml a -> Fml a
toUniversalNAnd :: Fml a -> Fml a
toUniversalNAnd f@(Final p) = f
-- NOT => ( A NAND A )
toUniversalNAnd (Not   p)   = NAnd (toUniversalNAnd p) (toUniversalNAnd p)
-- NAND => (A NAND B)
toUniversalNAnd (NAnd  p q) = NAnd (toUniversalNAnd p) (toUniversalNAnd q)
-- OR => ( A NAND A ) NAND ( B NAND B ) 
toUniversalNAnd (Or    p q) = NAnd (toUniversalNAnd (Not p)) (toUniversalNAnd (Not q))
-- AND => ( A NAND B ) NAND ( A NAND B ) 
toUniversalNAnd (And   p q) = NAnd (toUniversalNAnd(NAnd  p q)) (toUniversalNAnd(NAnd  p q))
-- NOR => [ ( A NAND A ) NAND ( B NAND B ) ] NAND [ ( A NAND A ) NAND ( B NAND B ) ] 
toUniversalNAnd (NOr   p q) = NAnd (NAnd (toUniversalNAnd(Not p)) (toUniversalNAnd(Not q))) (NAnd (toUniversalNAnd(Not p)) (toUniversalNAnd(Not q)))
-- XOR => [ A NAND ( A NAND B ) ] NAND [ B NAND ( A NAND B ) ]
toUniversalNAnd (XOr   p q) = NAnd (NAnd (toUniversalNAnd p) (toUniversalNAnd(NAnd  p q))) (NAnd (toUniversalNAnd q) (toUniversalNAnd(NAnd  p q)))
-- XNOR => [ ( A NAND A ) NAND ( B NAND B ) ] NAND ( A NAND B ) 
toUniversalNAnd (XNOr  p q) = NAnd (NAnd (toUniversalNAnd (Not p)) (toUniversalNAnd (Not q)))  (toUniversalNAnd(NAnd  p q))
-- IMPLY => NOT [ A AND (Not B) ]
toUniversalNAnd (Imply p q) = toUniversalNAnd(Not (toUniversalNAnd(And (p) (toUniversalNAnd(Not q)))))
-- EQUIV => [ ( A NAND A ) NAND ( B NAND B ) ] NAND ( A NAND B ) 
toUniversalNAnd (Equiv p q) = NAnd (NAnd (toUniversalNAnd(Not p)) (toUniversalNAnd (Not q))) (NAnd (toUniversalNAnd(p)) (toUniversalNAnd(q)))

-- |’toUniversalNOr’ @p@ returns a NOR-formula that is equivalent
-- to formula @p@.
--toUniversalNOr :: Fml a -> Fml a
toUniversalNOr :: Fml a -> Fml a
toUniversalNOr f@(Final p)  = f
-- NOT => ( A NOR A )
toUniversalNOr (Not   p)    = NOr (toUniversalNOr p) (toUniversalNOr p)
-- NOR => (A NOR B)
toUniversalNOr (NOr   p q)  = NOr (toUniversalNOr p) (toUniversalNOr q)
-- AND => ( A NOR A ) NOR ( B NOR B ) 
toUniversalNOr (And   p q)  = NOr (toUniversalNOr (Not p)) (toUniversalNOr (Not q))
-- OR => ( A NOR B ) NOR ( A NOR B ) 
toUniversalNOr (Or    p q)  = NOr (toUniversalNOr(NOr  p q)) (toUniversalNOr(NOr  p q))
-- NAND => [ ( A NOR A ) NOR ( B NOR B ) ] NOR [ ( A NOR A ) NOR ( B NOR B ) ] 
toUniversalNOr (NAnd  p q)  = NOr (toUniversalNOr (And p q)) (toUniversalNOr (And p q))
-- XOR => [ ( A NOR A ) NOR ( B NOR B ) ] NOR ( A NOR B ) 
toUniversalNOr (XOr   p q)  = NOr (toUniversalNOr (And p q)) (toUniversalNOr (NOr p q))
-- XNOR => [ A NOR ( A NOR B ) ] NOR [ B NOR ( A NOR B ) ]
toUniversalNOr (XNOr  p q)  = NOr (toUniversalNOr (NOr p (toUniversalNOr (NOr p q)))) (toUniversalNOr (NOr q (toUniversalNOr (NOr p q))))
-- IMPLY => NOT [ A AND (Not B) ]
toUniversalNOr (Imply p q)  = toUniversalNOr(Not (toUniversalNOr (And (p) (toUniversalNOr (Not q)))))
-- EQUIV => XNOR A B
toUniversalNOr (Equiv p q)  = toUniversalNOr (XNOr p q)

-- ############  TESTING  ############

-- |’isNNF’ @f@ returns true iff formula @f@ is NNF.
isNNF :: Fml a -> Bool
isNNF (Final p)       = True
isNNF (Not (Final p)) = True
isNNF (Not (f))       = False
isNNF (And   p q)     = isNNF p && isNNF q
isNNF (Or    p q)     = isNNF p && isNNF q
isNNF (NAnd  p q)     = isNNF p && isNNF q
isNNF (NOr   p q)     = isNNF p && isNNF q
isNNF (XOr   p q)     = isNNF p && isNNF q
isNNF (XNOr  p q)     = isNNF p && isNNF q
isNNF (Imply p q)     = isNNF p && isNNF q
isNNF (Equiv p q)     = isNNF p && isNNF q

-- |’isCNF’ @f@ returns true iff formula @f@ is CNF.
isCNF :: Fml a -> Bool
isCNF f = checkCNF f && isNNF f

-- |checkCNF @f@ returns true iff formula @f@ is a conjuction of disjunction.
checkCNF :: Fml a -> Bool
checkCNF (And   p q) = checkCNF p      && checkCNF q
checkCNF (Or    p q) = checkDisj p && checkDisj q
  where
    checkDisj (Or    p q)     = checkDisj p && checkDisj q
    checkDisj (Final p)       = True
    checkDisj (Not (Final p)) = True
    checkDisj f               = False
checkCNF (Final p)       = True
checkCNF (Not (Final p)) = True
checkCNF f               = False

-- |checkDisj @f@ returns true iff formula @f@ is a only composed of disjunction.
--checkDisj :: Fml a -> Bool

-- |’isDNF’ @f@ returns true iff formula @f@ is DNF.
isDNF :: Fml a -> Bool
isDNF f = checkDNF f && isNNF f

-- |checkDNF @f@ returns true iff formula @f@ is a disjunction of conjuction.
checkDNF :: Fml a -> Bool
checkDNF (Or    p q) = checkDNF p      && checkDNF q
checkDNF (And   p q) = checkConj p && checkConj q
  where
    checkConj (And    p q)    = checkConj p && checkConj q
    checkConj (Final p)       = True
    checkConj (Not (Final p)) = True
    checkConj f               = False
checkDNF (Final p)       = True
checkDNF (Not (Final p)) = True
checkDNF f               = False

-- |’isCCNF’ @f@ returns true iff formula @f@ is CCNF.
isCCNF :: Fml a -> Bool
isCCNF f = checkRec f && isCNF f

-- |checkRec @f@ returns true iff formula @f@ is recursively correct.
checkRec :: Fml a -> Bool
checkRec (Or  p q)        = True
checkRec (And (Or p q) r) = checkRec r
checkRec f                = False


-- |’isUniversalNAnd’ @p@ returns true iff formula @p@ uses only NAND
-- and variables.
--isUniversalNAnd :: Fml a -> Bool

-- |’isUniversalNOr’ @p@ returns true iff formula @p@ uses only NOR
-- and variables.
--isUniversalNOr :: Fml a -> Bool