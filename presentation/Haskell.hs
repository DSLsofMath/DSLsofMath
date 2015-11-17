
kvadrat x    =  x^2

twice f      =  \x -> f(f(x))

upphöjtFyra  =  twice kvadrat

test         =  upphöjtFyra 3

list         =  [(1+), (2*), kvadrat, upphöjtFyra]

{-
kvadrat      :: Num a =>  a -> a
kvadrat x    =  x^2
twice        :: (a -> a) -> (a -> a)
twice f      =  \x -> f(f(x))
upphöjtFyra  :: Num a =>  a -> a
upphöjtFyra  =  twice kvadrat
test         :: Double
test         =  upphöjtFyra 3
list         :: Num a =>  [a -> a]
list         =  [(1+), (2*), kvadrat, upphöjtFyra]
-}
