module Data.Logic.Fml.Some (
  -- * Testing Fml String
  fml1
, fml2
, jfml1
, jfml2
, jfml3
, jfml4
, jfml5
, jfml6
, jfml7
, jfml8
, jfml9
, jfml10
, jfml11
, jfml12
, jfml13
, jfml14
, jfml15
, jfml16
) where

  import qualified Data.Logic.Var      as Var
  import qualified Data.Logic.Var.Some as Vars
  import qualified Data.Logic.Fml      as Fml

  fml1 :: Fml.Fml String
  fml1 = Fml.Final Vars.x1

  fml2 :: Fml.Fml String
  fml2 = f
    where
      f  = g `Fml.Imply` h
      g  = g1 `Fml.Equiv` g2
      g1 = (Fml.Final Vars.x1) `Fml.NAnd` (Fml.Not $ Fml.Final Vars.x2)
      g2 = (Fml.Final Vars.x3) `Fml.XOr` (Fml.Final Vars.x4)
      h  = Fml.Not (h1 `Fml.NOr` h2)
      h1 = (Fml.Final Vars.x5) `Fml.Or` (Fml.Final Vars.x6)
      h2 = (Fml.Final Vars.x7) `Fml.Equiv` (Fml.Not $ Fml.Final Vars.x8)

  -- A ∧ (¬A ∨ ((¬A ∨ B) ∧ ¬B))
  jfml1 :: Fml.Fml String
  jfml1 = f
    where
      f  = Fml.And (Fml.Final Vars.a) f1
      f1 = Fml.Or (Fml.Not $ Fml.Final Vars.a) f2
      f2 = Fml.And f3 (Fml.Not $ Fml.Final Vars.b)
      f3 = Fml.Or (Fml.Not $ Fml.Final Vars.a) (Fml.Final Vars.b)

  -- ¬A ∧ ¬B
  jfml2 :: Fml.Fml String
  jfml2 = Fml.And (Fml.Not $ Fml.Final Vars.a) (Fml.Not $ Fml.Final Vars.b)

  -- not NNF
  -- ¬(A ∨ B)
  jfml3 :: Fml.Fml String
  jfml3 = Fml.Not f
    where
      f = Fml.Or (Fml.Final Vars.a) (Fml.Final Vars.b)

  -- (¬(A ∧ ¬B) ∧ C) ≡ ((¬A ∨ B) ∧ C)
  jfml4 :: Fml.Fml String
  jfml4 = f
    where
      f = Fml.And f1 (Fml.Final Vars.c)
      f1 = Fml.Not (Fml.And (l1) (l2))
      l1 = Fml.Final Vars.a
      l2 = Fml.Not $ Fml.Final Vars.b

  -- (¬(A ∨ ¬B) ∧ C) ≡ ((¬A ∧ B) ∧ C)
  jfml5 :: Fml.Fml String
  jfml5 = f
    where
      f = Fml.And f1 (Fml.Final Vars.c)
      f1 = Fml.Not (Fml.Or (l1) (l2))
      l1 = Fml.Final Vars.a
      l2 = Fml.Not $ Fml.Final Vars.b

  -- ¬(A → (A ∧ B))
  jfml6 :: Fml.Fml String
  jfml6 = Fml.Not f
    where
      f = Fml.Imply (Fml.Final Vars.a) (f1)
      f1 = Fml.And (Fml.Final Vars.a) (Fml.Final Vars.b)

  -- A ↑ B (NAND)
  jfml7 :: Fml.Fml String
  jfml7 = Fml.NAnd (Fml.Final Vars.a) (Fml.Final Vars.b)
  -- A ↓ B (NOR)
  jfml8 :: Fml.Fml String
  jfml8 = Fml.NOr (Fml.Final Vars.a) (Fml.Final Vars.b)

  -- A ⊕ B (XOR)
  jfml9 :: Fml.Fml String
  jfml9 = Fml.XOr (Fml.Final Vars.a) (Fml.Final Vars.b)

  -- A XNOR B
  jfml10 :: Fml.Fml String
  jfml10 = Fml.XNOr (Fml.Final Vars.a) (Fml.Final Vars.b)

  -- A ⇒ B (IMPLY)
  jfml11 :: Fml.Fml String
  jfml11 = Fml.Imply (Fml.Final Vars.a) (Fml.Final Vars.b)

  -- A ⇔ B (EQUIV)
  jfml12 :: Fml.Fml String
  jfml12 = Fml.Equiv (Fml.Final Vars.a) (Fml.Final Vars.b)

  -- A ∨ (B ∧ C) ≡ (A ∨ B) ∧ (A ∨ C)
  jfml13 :: Fml.Fml String
  jfml13 = f
    where
      f = Fml.Or (Fml.Final Vars.a) f1
      f1 = Fml.And (Fml.Final Vars.b) (Fml.Final Vars.c)

  -- A ∧ (B ∨ C) ≡ (A ∧ B) ∨ (A ∧ C)
  jfml14 :: Fml.Fml String
  jfml14 = f
    where
      f = Fml.And (Fml.Final Vars.a) f1
      f1 = Fml.Or (Fml.Final Vars.b) (Fml.Final Vars.c)

  -- ((A ∧ B) ∧ C) ∨ (P ∧ Q) ≡ (A ∨ P) ∧ (B ∨ P) ∧ (C ∨ P) ∧ (A ∨ Q) ∧ (B ∨ Q) ∧ (C ∨ Q)
  jfml15 :: Fml.Fml String
  jfml15 = f
    where
      f = Fml.Or f1 f2
      f1 = Fml.And f3 (Fml.Final Vars.c)
      f2 = Fml.And (Fml.Final Vars.p) (Fml.Final Vars.q)
      f3 = Fml.And (Fml.Final Vars.a) (Fml.Final Vars.b)

  -- A → (B ∧ (¬(¬C ∧ P)) ≡ ¬A ∨ (B ∧ (C ∨ ¬P)) ≡ (¬A ∨ B) ∧ (¬A ∨ C ∨ ¬P) ≡ ¬A ∨ (B ∧ C) ∨ (B ∧ ¬P)
  --                        NNF                   CNF                        DNF
  jfml16 :: Fml.Fml String
  jfml16 = f
    where
      f = Fml.Imply (Fml.Final Vars.a) f1
      f1 = Fml.And (Fml.Final Vars.b) f2
      f2 = Fml.Not(f3)
      f3 = Fml.And (Fml.Not $ Fml.Final Vars.c) (Fml.Final Vars.p)
