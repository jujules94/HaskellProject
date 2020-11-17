module Data.Logic.Fml.Some (
  -- * Testing Fml String
  fml1
  --, fml2
) where

  import qualified Data.Logic.Var      as Var
  import qualified Data.Logic.Var.Some as Vars
  import qualified Data.Logic.Fml      as Fml

  fml1 :: Fml.Fml String
  fml1 = Fml.Final Vars.x1

  --fml2 :: Fml.Fml String
  --fml2 = f
  --  where
  --    f  = g `Fml.Imply` h
  --    g  = g1 `Fml.Equiv` g2
  --    g1 = Vars.x1 `Fml.NAnd` (Fml.Not Vars.x2)
  --    g2 = Vars.x3 `Fml.XOr` Vars.x4
  --    h  = Fml.Not (h1 `Fml.NOr` h2)
  --    h1 = Vars.x5 `Fml.Or` Vars.x6
  --    h2 = Vars.x7 `Fml.Equiv` Fml.Not Vars.x8