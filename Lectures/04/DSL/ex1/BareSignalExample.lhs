To load:

stack ghci SignalShape:exe:ex1

> module Main where
> import System.Posix (usleep)
> import Hatlab.Plot

To model time-varying signals we need to decide on a type for time and
a type for signals. This will do to start with:

> type Time      = Double
> type Signal a  = Time -> a

Note that we have immediately generalised from the most common
mathematical use case of a real-valued signal to a polymorphic family
of types |Signal a| for all a.

Examples could be

> s1 :: Signal Double
> s1 = sin
> s2 :: Signal Bool
> s2 = (>=0) . s1

We can sample a signal at any time just using function application.

> test11 = s1 0
> test12 = s1 (pi/2)

We can lift values to (constant) signals:

> constS :: a -> Signal a

We can add signals:

> addS :: Num a => Signal a -> Signal a -> Signal a
> mulS :: Num a => Signal a -> Signal a -> Signal a
> speedUp :: Double -> Signal a -> Signal a

and we can map a function over a signal:

> mapS :: (a->b) -> Signal a -> Signal b

Examples:

s3(t) = 2 + sin t

> s3 = addS (constS 2) s1
> s4 = mulS (constS 0.4) (speedUp 5 s1)
> s5 = addS s3 s4


There is actually a general pattern of "lifting":

> lift0 ::  a        -> Signal a
> lift1 :: (a->b)    -> Signal a -> Signal b
> lift2 :: (a->b->c) -> Signal a -> Signal b -> Signal c

> lift0 = constS
> lift1 = mapS
> lift2 (+-) s1 s2 = \t ->  s1 t  +-  s2 t

> constS x    = \t -> x
> addS s1 s2  = \t -> s1 t + s2 t
> mulS s1 s2  = \t -> s1 t * s2 t
> mapS f s    = \t -> f (s t)
> speedUp k s = \t -> s (k*t)

> render :: Time -> Double -> Signal Double -> [String]
> render step maxi s = map (bar . s) [0,step..maxi]
>   where bar x = replicate (round (magnify*x)) '*'
> magnify = 10

> r = putStr . unlines . render 0.1 pi

> p s = plot [Fun (speedUp pi s) ""]

> w = usleep . (1000*)

> main = do p s1
>           w 1000
>           p s3
>           w 1000
>           p s4
>           w 1000
>           p s5
>           w 1000
