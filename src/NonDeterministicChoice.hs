
module NonDeterministicChoice where

import Common.Interpreter hiding (Term)

-- list monad
type L a = [a]
returnL a = [a]
m `bindL` k = [b | a <- m, b <- k a]
zeroL = []
l `plusL` m = l ++ m
showL m = [showval a | a <- m ]

-- add the new ops to Term
data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Fail
          | Amb Term Term

interp Fail e = zeroL
interp (Amb u v) e = interp u e `plusL` interp v e

-- TODO: solve problem of ambiguss interp by using ifdef 
