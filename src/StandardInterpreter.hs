
module StandardInterpreter where

import Common.Interpreter

-- Variation 0 : Standard interpreter
type I a = a
returnI a = a
a `bindI` k = k a
showI a = showval a


