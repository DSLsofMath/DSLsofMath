> module BareSignalExample where

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
> constS = error "TBD"

We can add signals:

> addS :: Num a => Signal a -> Signal a -> Signal a
> addS = error "TBD"

and we can map a function over a signal:

> mapS :: (a->b) -> Signal a -> Signal b
> mapS = error "TBD"

There is actually a general pattern of "lifting":

> lift0 ::  a        -> Signal a
> lift1 :: (a->b)    -> Signal a -> Signal b
> lift2 :: (a->b->c) -> Signal a -> Signal b -> Signal c

> lift0 = constS
> lift1 = mapS
> lift2 (+-) f g = \t ->  f t  +-  g t
