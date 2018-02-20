\begin{code}
module DSLsofMath.W05.Test where
import DSLsofMath.W05
import Test.QuickCheck

instance (Arbitrary a, Num a) => Arbitrary (Poly a) where
  arbitrary = fmap fromList arbitrary
checkDegreeMI p = checkDegreeM (p::Poly Integer)
checkMonoid0Maybe x = op x unit == x  &&  op unit x == x
  where dummy = x :: Maybe Integer
main = do  print checkDegree0
           quickCheck checkMonoid0Maybe
           quickCheck checkDegreeMI

\end{code}
