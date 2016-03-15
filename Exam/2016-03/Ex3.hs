module Ex3 where

import Lecture10
import Lecture11

{-
f'' t - 2 * f' t + f t = exp (2 * t), f 0 = 2, f' 0 = 3

    i. [10pts] Solve the equation assuming that `f` can be expressed
       by a power series `fs`, that is, use `deriv` and `integ` to compute
       `fs`.  What are the first three coefficients of `fs`?
-}

{-
f'' t = exp (2 * t) + 2 * f' t - f t
-}

f''s = expx2 + 2 * f's - fs

f's = integ f''s 3

fs = integ f's 2

firstCoeffs = takePoly 3 fs == fromList [2, 3, 2.5]

-- power series for exp (2 * t)
expx2 = integ (2 * expx2) 1

{-
    ii. [15pts] Solve the equation using the Laplace transform.  You
        should need only one formula (and linearity):

    $$ℒ\, (\lambda t.\, e^{α*t})\, s  = 1 / (s - α)$$
-}

{-
LHS:

  \t -> f'' t - 2 * f' t + f t

=> apply Laplace

  L (\t -> f'' t - 2 * f' t + f t) s

=> linearity of Laplace

  L f'' s - 2 * L f' s + L f s

=> Laplace of derivatives

  (s * L f' s - f' 0) - 2 * (s * L f s - f 0) + L f s

=> again

  (s * (s * L f s - f 0) - f' 0) - 2 * (s * L f s - f 0) + L f s

=> rearrange

  s^2 * L f s - s * f 0 - f' 0 - 2 * s * L f s + 2 * f 0 + L f s

=> collect

  (s^2 - 2 * s + 1) * L f s - s * f 0 - f' 0 + 2 * f 0

=> f 0 = 2, f' 0 = 3

  (s^2 - 2 * s + 1) * L f s - s * 2 - 3 + 4

=> simplify

  (s - 1)^2 * L f s - 2 * s + 1

RHS:

  \t -> exp (2 * t)

=> Laplace

  L (\t -> exp (2 * t)) s

=> Given definition

  1 / (s - 2)

Combine LHS and RHS

  (s - 1)^2 * L f s - 2 * s + 1 = 1 / (s - 2)

=> Solve for L f s

  L f s = (1 / (s - 2) + 2 * s - 1) / (s - 1)^2

Do something similar to lecture 13 to find a re-arrangment for which
we know the inverse Laplace:

  L f s = 1 / (s - 1) + 1 / (s - 2) + 0 / (s - 1)^2

(Fortunately the last term cancels out - otherwise we would have needed

  L (\t -> t * exp (α * t)) s = 1 / (s - α)^2

)

So,

  f = \t -> exp t + exp (2 * t)

-}
