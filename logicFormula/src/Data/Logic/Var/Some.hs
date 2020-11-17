module Data.Logic.Var.Some (
  -- * Testing Var Int
  var1
) where

  import qualified Data.Logic.Var      as Var

  var1 :: Var.Var Int
  var1 = Var.mk 1