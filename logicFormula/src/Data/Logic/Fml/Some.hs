module Data.Logic.Fml.Some (
  -- * Testing Var Int
  fml1
  , fml2
) where

  import qualified Data.Logic.Var      as Var
  import qualified Data.Logic.Fml      as Fml

  -- |Satisfiable empty CNF formula.
  --
  -- >>> fml1
  -- []
  fml1 :: Fml.Fml Int
  fml1 = Fml.Final $ Var.mk 1

  fml2 :: Fml.Fml String
  fml2 = f
    where
      f  = g `Fml.Imply` h
      g  = g1 `Fml.Equiv` g2
      g1 = x1 `Fml.NAnd` Fml.Not x2
      g2 = x3 `Fml.XOr` x4
      h  = Fml.Not (h1 `Fml.NOr` h2)
      h1 = x5 `Fml.Or` x6
      h2 = x7 `Fml.Equiv` Fml.Not x8