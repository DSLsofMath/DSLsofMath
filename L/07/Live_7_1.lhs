
\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
module DSLsofMath.Live_7_1 where
import DSLsofMath.W07 hiding (Vector)

-- type S = Double
class Vector v where
  zero   :: v

  add    :: v -> v -> v
  scale  :: S -> (v -> v)

  addInv :: v -> v
  addInv = scale (-1)
\end{code}

* New algebraic structure: vector space.
  Two sets involved : S for scalars, V for vectors and
  Two main operations:
    addV and scaleV

* See also L/03/ExerciseSolutions/E3_6_simple.lhs  (Exam/2017-08/P2.lhs)
* Exam/2017-03/P5.lhs
* Exam/2016-Practice/MockE.hs

\begin{spec}
instance Num s => Vector s where
  zero = 0
  add = (+)
  scale = (*)
  addInv = negate
\end{spec}

\begin{code}
instance Vector Double where
  zero = 0
  add = (+)
  scale = (*)
  addInv = negate

instance Vector v => Vector (g->v) where
  zero = zeroF
  add  = addF
  scale = scaleF
  addInv = addInvF

type V s g = g -> s

zeroF :: Vector v => g->v
zeroF = const zero

addF :: Vector v => V v g -> V v g -> V v g
addF f g = \x -> add (f x) (g x)

scaleF :: Vector v => S -> V v g -> V v g
scaleF s f = \x -> scale s (f x)

addInvF :: Vector v => V v g -> V v g
addInvF f = \x -> addInv (f x)
\end{code}

Note that the instance declaration for |g->v| is parametrised over
another vector field. That means we can use it several times to get
"vectors of vectors of ... of scalars". This is not widely used in the
case of vectors, but more common for matrices, where it represents
(regular) block matrices. (A block matrix is a matrix whose "elements"
are smaller matrices.)

\begin{code}
type TwoD = Bool
type Vect2D = Bool -> S

data Day = Mon|Tue|Wed|Thu|Fri|Sat|Sun
  deriving (Enum, Show)
type WeekVect = Day -> REAL
\end{code}

ex2 :: WeekVect
List indexing has type |Int -> REAL|
We need |Day -> REAL|
Helpers from the Haskell Predude:
* |toEnum :: Int -> Day|
* |fromEnum :: Day -> Int|

\begin{code}
ex2 :: Day -> REAL
ex2 = ([12,3,12,34,19,2,7]!!) . fromEnum
-- ex2 represents website malfunctions
data Stat = Weekday | Weekend
\end{code}

Example: compute average weekday, and average weekend
malfunctions. (Given a vector of nuber of malfunctions per day.)

It is a homomorphism between vector spaces.
\begin{code}
stat :: (Day->REAL) -> (Stat->REAL)
stat = error "TODO"

-- base vector 0 = [1,0,0,0,0,0,0]
-- base vector 1 = [0,1,0,0,0,0,0]
-- base vector 2 = [0,0,1,0,0,0,0]
-- ...
-- base vector 6 = [0,0,0,0,0,0,1]
-- stat as a matrix (here a list of columns):
statM = [ [1/5,0],[1/5,0],[1/5,0],[1/5,0],[1/5,0], [0,1/2],[0,1/2]]

ex1 :: Vect2D
ex1 False  = 1
ex1 True   = 2

hej :: Vect2D
hej False  = 3
hej True   = 7
\end{code}
