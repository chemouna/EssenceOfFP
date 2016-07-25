
module Identity where

import Common.Interpreter
import Prelude hiding (return)


-- use our identity monad for interpreter
lookupI :: Name -> Environment -> Value
lookupI x [] = Wrong
lookupI x ((y,b):e) = if x == y then b else lookupI x e

addI :: Value -> Value -> Value
addI (Num i) (Num j) = Num (i + j)
addI a b = Wrong

applyI :: Value -> Value -> Value
applyI (Fun k) a = a -- not sure ? 
applyI f a = Wrong

interpI :: Term -> Environment -> Value
interpI (Var x) e = lookupI x e
interpI (Con i) e = Num i
interpI (Add u v) e = addI (interpI u e) (interpI v e)

--interpI (Lam x v) e = Fun (\a -> interpI v ((x,a):e))
interpI (App t u) e = applyI (interpI t e) (interpI u e)

