module Data.Logic.Var.Some (
  -- * Testing Var String
  x1
, x2
, x3
, x4
, x5
, x6
, x7
, x8
, x9
, a
, b
, c
, p
, q
) where

import qualified Data.Logic.Var as Var

x1 :: Var.Var String
x1 = Var.mk "x1"

x2 :: Var.Var String
x2 = Var.mk "x2"

x3 :: Var.Var String
x3 = Var.mk "x3"

x4 :: Var.Var String
x4 = Var.mk "x4"

x5 :: Var.Var String
x5 = Var.mk "x5"

x6 :: Var.Var String
x6 = Var.mk "x6"

x7 :: Var.Var String
x7 = Var.mk "x7"

x8 :: Var.Var String
x8 = Var.mk "x9"

x9 :: Var.Var String
x9 = Var.mk "x9"

a :: Var.Var String
a = Var.mk "A"

b :: Var.Var String
b = Var.mk "B"

c :: Var.Var String
c = Var.mk "C"

p :: Var.Var String
p = Var.mk "P"

q :: Var.Var String
q = Var.mk "Q"