The exponentiation operator source code is available in the |Algebra| module:

  https://github.com/DSLsofMath/DSLsofMath/blob/master/L/DSLsofMath/Algebra.hs

and below.

\begin{code}
import DSLsofMath.Algebra(Additive(..), AddGroup(..), Multiplicative(..), MulGroup(..))
import qualified Prelude
import Prelude (Double, Rational, Int, Integer, (==), (<), otherwise, error)
(^) :: MulGroup a => a -> Int -> a
a ^ b | b < 0      = recip (a ^+ (negate b))
      | otherwise  = (a ^+ b)

(^+) :: Multiplicative a => a -> Int -> a
x0 ^+ n0 = if n0 < 0  then  error "Algebra.Classes.^: negative exponent"
                      else  go n0 x0
  where go 0 _ = one
        go 1 x = x
        go n x = if r == 0 then y2 else x * y2
          where  (m,r) = n `Prelude.divMod` 2
                 y = go m x
                 y2 = y * y
\end{code}
