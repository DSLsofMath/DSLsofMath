import Prelude hiding (exp,sin,cos)
z=zipWith
c∫f = c:z (/) f [1..]
exp = 1∫exp
sin = 0∫cos
cos = 1∫(-sin)
instance Num a => Num [a] where
  (+) = z (+)
  (-) = z (-)
  (*) = m
  fromInteger i = fromInteger i : repeat 0
m (x:xs) q@(y:ys) = x*y : (map (x*) ys) + xs*q
-- #DSLM

{- A tweet-sized version of today's live coding in the DSLsofMath course.
https://github.com/DSLsofMath/DSLsofMath
Test with
-}

one = 1 :: [Rational]
test = (sin^2+cos^2) =~ one
d cs = zipWith (*) [1..] (tail cs)
xs =~ ys = and (take 10 (z (==) xs ys))

test2 = exp =~ d exp
