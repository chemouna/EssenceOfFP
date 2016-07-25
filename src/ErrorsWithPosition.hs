
module ErrorsWithPosition where

import Common.Interpreter
import ErrorsMessages

-- data Term = .... | At Position Term

data Position
type P a = Position -> E a 

showpos p = show p
pos0 = 0

returnP a = \p -> returnE a
errorP s = \p -> errorE (showpos p ++ ": "++ s)

m `bindP` k = \p -> m p `bindE` \x -> k x p
showP m = showE (m pos0)

