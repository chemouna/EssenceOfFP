module State where

import Common.Interpreter

type State = Int 
type S a = State -> (a, State)

returnS a = \s0 -> (a, s0)
m `bindS` k = \s0 -> let (a, s1) = m s0
                         (b, s2) = k a s1
                      in (b, s2)

-- model count
showS m = let (a, s1) = m 0
              in " Value: "++ a ++ " ; " ++
                 "Count: "++ showint s1
tickS :: S ()
tickS = \s -> ((), s+1)

add (Num i) (Num j) = tickS `bindS` (\() -> returnS (Num (i + j)))
apply (Fun k) a = tickS `bindS` (\() -> returnS (k a)) -- not sure of returnS being here 
