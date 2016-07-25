
module Common.Interpreter where

import Prelude hiding (return, lookup)

data M a

returnM :: a -> M a
returnM = undefined

bindM :: M a -> (a -> M b) -> M b
bindM = undefined

-- Interpretation in a monad

data Value = Wrong
           | Num Int
           | Fun (Value -> M Value)

showint :: Int -> String
showint i = show i

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = showint i
showval (Fun f) = "<function>"

type Name = String
data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
type Environment = [(Name, Value)]

lookup :: Name -> Environment -> M Value
lookup x [] = returnM Wrong
lookup x ((y,b):e) = if x == y then returnM b else lookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = returnM (Num (i + j))
add a b = returnM Wrong

apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply f a = returnM Wrong

interp :: Term -> Environment  -> M Value
interp (Var x) e = lookup x e
interp (Con i) e = returnM (Num i)
interp (Add u v) e = interp u e `bindM` (\a ->
                     interp v e `bindM` (\b ->
                     add a b))
interp (Lam x v) e = returnM (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e `bindM` (\f ->
                     interp u e `bindM` (\a ->
                       apply f a))





