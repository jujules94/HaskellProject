module Data.Logic.Fml.Some (
  -- * Testing Var Int
  fml1
) where

  import qualified Data.Logic.Var      as Var
  import qualified Data.Logic.Fml      as Fml

  -- |Satisfiable empty CNF formula.
  --
  -- >>> fml1
  -- []
  fml1 :: Fml.Fml Int
  fml1 = Fml.Final $ Var.mk 1