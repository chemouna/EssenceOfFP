
module Output where

import Common.Interpreter hiding (Term, interp)

type O a = (String, a)
returnO a = ("", a)
m `bindO` k = let (r,a) = m; (s,b) = k a in (r ++ s, b)
showO (s, a) = "Output: " ++ s ++ " Value: " ++ showval a

outO :: Value -> O ()
outO a = (showval a ++ "; ", ())

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Out Term

interp (Out u) e = interp u e `bindO` (\a ->
                    outO a `bindO` (\() ->
                    returnO a))

-- (Add (Out (Con 41)) (Out (Con 1)))
