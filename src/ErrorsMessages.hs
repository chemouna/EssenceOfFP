
module ErrorsMessages where 

import Common.Interpreter
import Prelude hiding (return, lookup)


-- Variation 1 : Error messages
data E a = Success a | Error String
returnE a = Success a
errorE s = Error s

(Success a) `bindE` k = k a
(Error s) `bindE`  k = Error s

showE (Success a) = "Success: " ++ showval a
showE (Error s) = "Error: " ++ s

lookupE :: Name -> Environment -> E Value
lookupE x [] = error ("unbound variable "++ x)
lookupE x ((y,b):e) = if x == y then Success b else lookupE x e

addE :: Value -> Value -> E Value
addE (Num i) (Num j) = Success (Num (i + j))
addE a b = errorE ("should be numbers: "++ showval a ++ "," ++ showval b)

applyE :: Value -> Value -> E Value
applyE (Fun k) a = Success a -- not sure ? 
applyE f a = errorE ("should be a function: "++ showval f)

