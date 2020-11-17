module Data.Logic.Fml (
  -- * Type
  Fml (..)

  -- * Querying
  , depth
  , vars

  -- * Formatting
  , prettyFormat

  -- * Transforming
  --, toNNF
  --, toCNF
  --, toCCNF
  --, toDNF
  --, toUniversalNAnd

  -- * Testing
  --, isNNF
  --, isCNF
  --, isCCNF
  --, isDNF

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
  	getVars (Not   p)   = [] ++ getVars p
  	getVars (And   p q) = [] ++ getVars p ++ getVars q
  	getVars (NAnd  p q) = [] ++ getVars p ++ getVars q
  	getVars (Or    p q) = [] ++ getVars p ++ getVars q
  	getVars (NOr   p q) = [] ++ getVars p ++ getVars q
  	getVars (XOr   p q) = [] ++ getVars p ++ getVars q
  	getVars (XNOr  p q) = [] ++ getVars p ++ getVars q
  	getVars (Imply p q) = [] ++ getVars p ++ getVars q
  	getVars (Equiv p q) = [] ++ getVars p ++ getVars q

-- |’depth’ @p@ returns the depth of fomula @p@.
depth :: (Num b, Ord b) => Fml a -> b
depth (Final p) = 0
depth (Not   p) = 0
depth (And   p q) = 1 + max $ depth p $ depth q
depth (NAnd  p q) = 1 + max $ depth p $ depth q
depth (Or    p q) = 1 + max $ depth p $ depth q
depth (NOr   p q) = 1 + max $ depth p $ depth q
depth (XOr   p q) = 1 + max $ depth p $ depth q
depth (XNOr  p q) = 1 + max $ depth p $ depth q
depth (Imply p q) = 1 + max $ depth p $ depth q
depth (Equiv p q) = 1 + max $ depth p $ depth q