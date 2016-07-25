
module MonadInterpreter where

data M a

returnM :: a -> M a
returnM = undefined

bindM :: M a -> (a -> M b) -> M b
bindM = undefined

-- Variation 0 : Standard interpreter

