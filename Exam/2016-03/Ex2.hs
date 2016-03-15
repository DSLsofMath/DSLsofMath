module Ex2 where

{-
2. [20pts] Consider the following text from Mac Lane's *Mathematics: Form and
   Function* (page 182):

    > In these cases one tries to find not the values of `x` which
    make a given function `y = f(x)` a minimum, but the values of a given
    function `f(x)` which make a given quantity a minimum.  Typically,
    that quantity is usually measured by an integral whose integrand is
    some expression `F` involving both `x`, values of the function `y =
    f(x)` at interest and the values of its derivatives - say an
    integral
    $$∫_a^b F(y, y', x)dx,\quad y = f(x).$$

    Give the types of the variables involved (`x`, `y`, `y'`, `f`, `F`,
    `a`, `b`) and the type of the four-argument integration operator:

    $$∫_.^. \cdot d\cdot$$
-}

data X   -- X must include the interval [a,b] of the reals
data Y   -- another subset of the reals

x :: X

y  :: X -> Y
y' :: X -> Y

f :: X -> Y

-- big f
ff :: (X -> Y) -> (X -> Y) -> X -> Y

a, b :: X

-- ∫-d seems to bind x, in Haskell it makes sense to use a function
-- to bind a variable. Thus we get a 3-argument operator `int`:

int :: X -> X -> (X -> Y) -> Y

-- ∫_a^b F(y, y', x)dx
integral = int a b (\x -> ff y y' x)
  -- where y  = f
  --       y' = D f

-- dummy values:
(x, y, y', f, ff, a, b, int) = undefined
