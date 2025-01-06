\begin{code}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module DSLsofMath.Live_7_1 where
import DSLsofMath.W07

type S = Double
class Vector v where
  zero   :: v

  add    :: v -> v -> v
  scale  :: S -> v -> v

  addInv :: v -> v
  addInv = scale (-1)

class Vector2 v where
  type Scalar v
  zeroV   :: v

  addV    :: v -> v -> v
  scaleV  :: Scalar v -> v -> v

addInvV :: (Vector2 v, Num (Scalar v)) => v -> v
addInvV = scaleV (-1)

\end{code}

* New algebraic structure: vector space.
  Two sets involved : S for scalars, V for vectors and
  Two main operations:
    addV and scaleV

* See also L/03/ExerciseSolutions/E3_6_simple.lhs  (Exam/2017-08/P2.lhs)
* Exam/2017-03/P5.lhs
* Exam/2016-Practice/MockE.hs
