module Data.Logic.Fml.Combinator (
-- * Some functions on Fml

  multOr
, multAnd
, allOf
, noneOf
--, atLeast
, atLeastOne
--, atMost
--, atMostOne
--, exactly
--, exactlyOne
) where

import qualified Data.Logic.Var      as Var
import qualified Data.Logic.Fml      as Fml

import qualified Data.Logic.Var.Some as Vars

-- |’multOr’ @fs@ returns the disjunction of the formulas in @fs.
-- It returns @Nothing@ if @fs@ is the empty list.
--
-- >>> Combinator.multOr []
-- Nothing
-- >>> multOr [Fml.Final (Var.mk i) | i <- [1..4]]
-- Just (Or (Final 1) (Or (Final 2) (Or (Final 3) (Final 4))))
multOr :: [Fml.Fml a] -> Maybe(Fml.Fml a)
multOr [] = Nothing
multOr lst = Just (buildOr lst)

-- |’multAnd’ @fs@ returns the conjunction of the formulas in @fs.
-- It returns @Nothing@ if @fs@ is the empty list.
--
-- >>> Combinator.multAnd []
-- Nothing
-- multAnd [Fml.Final (Var.mk i) | i <- [1..4]]
-- Just (And (Final 1) (And (Final 2) (And (Final 3) (Final 4))))
multAnd :: [Fml.Fml a] -> Maybe (Fml.Fml a)
multAnd [] = Nothing
multAnd lst = Just (buildAnd lst)

-- |buildAnd is used to join the fmls with And (like in CCNF)
buildAnd :: [Fml.Fml a] -> Fml.Fml a
buildAnd lst = f lst
  where
    f [x]    = x
    f (x:xs) = Fml.And x $ f xs

-- |buildOr is used to join the fmls with Or
buildOr :: [Fml.Fml a] -> Fml.Fml a
buildOr lst = f lst
  where
    f [x]    = x
    f (x:xs) = Fml.Or x $ f xs

-- |’allOf’ @vs@ returns a formula that is satisfiable iff all variables
-- in @vs@ are true. The function returns @Nothing@ if @vs@ is the empty list.
allOf :: [Var.Var a] -> Maybe (Fml.Fml a)
allOf []  = Nothing
allOf lst = Just (buildAnd [Fml.Final elem | elem <- lst])

-- |’noneOf’ @vs@ returns a formula that is satisfiable iff no variable
-- in @vs@ is true. The function returns @Nothing@ if @vs@ is the empty list.
noneOf :: [Var.Var a] -> Maybe (Fml.Fml a)
noneOf []  = Nothing
noneOf lst = Just (buildAnd [Fml.Not (Fml.Final elem) | elem <- lst])

  -- |’atLeast’ @vs@ @k@ returns a formula that is satisfied iff at least @k@
  -- variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
  -- empty list or @k@ is non-positive or @k@ is larger than the number of
  -- variables in @vs@.
  -- atLeast :: [Var.Var a] -> Int -> Maybe (Fml.Fml a)

  -- |’atLeastOne’ @vs@ returns a formula that is satisfiable iff at least one
  -- variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
  -- empty list.
  atLeastOne :: [Var.Var a] -> Maybe (Fml.Fml a)
  atLeastOne []   = Nothing
  atLeastOne [x]  = Just (Fml.Final x)
  atLeastOne (xs) = Just (helper xs)
    where
      helper [x] = Fml.Final x
      helper (x:xs) = Fml.Or (Fml.Final x) (helper xs)

  -- |’atMost’ @vs@ @k@ returns a formula that is satisfiable iff at most @k@
  -- variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
  -- empty list or @k@ is non-positive or @k@ is larger than the number of
  -- variables in @vs@.
  -- atMost :: [Var.Var a] -> Int -> Maybe (Fml.Fml a)

  -- |’atMostOne’ @vs@ returns a formula that is satisfiable iff at most one
  -- variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
  -- empty list.
  -- atMostOne :: [Var.Var a] -> Maybe (Fml.Fml a)

  -- |’exactly’ @vs@ @k@ returns a formula that is satisfiable iff exactly @k@
  -- variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
  -- empty list or @k@ is non-positive or @k@ is larger than the number of
  -- variables in @vs@.
  -- exactly :: [Var.Var a] -> Int -> Maybe (Fml.Fml a)

  -- |’exactlyOne’ @vs@ returns a formula that is satisfiable iff exactly one
  -- variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
  -- empty list.
  -- exactlyOne :: [Var.Var a] -> Maybe (Fml.Fml a)