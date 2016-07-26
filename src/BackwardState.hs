
module BackwardState where


m `bindS` k = \s2 -> let (a, s0) = m s1
                         (b, s1) = k a s2
                     in (b, s0)
