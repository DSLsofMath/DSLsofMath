\begin{code}
module DSLsofMath.Live_7_1 where
import DSLsofMath.W07 hiding (Vector)
import qualified DSLsofMath.W07
type V = DSLsofMath.W07.Vector

type S = Double
class Vector v where
  zero   :: v

  add    :: v -> v -> v
  scale  :: S -> (v -> v)

  addInv :: v -> v
  addInv = scale (-1)    -- Prop: forall v.   v + addInv v == zero
\end{code}

* New algebraic structure: vector space.
  Two sets involved : S for scalars, v for vectors and
  Two main operations:
    addV and scaleV

\begin{code}
instance Vector Double where
  zero = 0
  add = (+)
  scale = (*)
  addInv = negate
\end{code}

Note that |Num.*  :: a -> a -> a|  -- symmetric type
and       |scale  :: S -> v -> v|  -- assymmetric type
but in this instance |S=v=a=Double|.

\begin{code}
v1, v2 :: Double
v1 = add 5 7
v2 = scale 0.5 v1

instance Vector v => Vector (g->v) where
  zero = zeroF
  add  = addF
  scale = scaleF

-- type Vec s g = g -> s   -- scalar set s and index set g

w0, w1, w2 :: Vec S Bool   -- (Bool -> S)  ≃  (S, S)  ≃  2D-vector
-- card Bool = 2

convert :: (S, S) -> (Bool -> S)
convert (f,t) = \b -> if b then t else f
\end{code}
Short side track: For a motivation behind convert, see the dual of the
tupling transform from (an exercise in) Chapter 1:

    s2p :: (Either b c -> a) -> (b->a, c->a)
    p2s :: (b->a, c->a) -> (Either b c -> a)

let a=b=1=() in Haskell

  (Either a b  -> c) <-> (a->c, b->c)
~=
  (Either 1 1  -> c) <-> (1->c, 1->c)
~=
  (1+1         -> c) <-> (   c,    c)
~=
  (2           -> c) <-> (   c,    c)
~=
  (Bool        -> c) <-> (   c,    c)

\begin{code}
w0 = convert (38, 17)
w1 = add w0 w0
w2 = scale 0.5 w1

zeroF :: Vector v => g->v
zeroF i = zero          -- not quite 0 (but close)

addF :: Vector v => Vec v g -> Vec v g -> Vec v g
addF v w = \i -> add (v i) (w i)      -- basically the same as   v + w  (FunNumInst)

scaleF :: Vector v => S -> Vec v g -> Vec v g
scaleF s v = \i -> scale s (v i)
\end{code}

Note that the instance declaration for |g->v| is parametrised over
another vector field. That means we can use it several times to get
"vectors of vectors of ... of scalars". This is not widely used in the
case of vectors, but more common for matrices, where it represents
(regular) block matrices. (A block matrix is a matrix whose "elements"
are smaller matrices.)

\begin{code}
data Day = Mon|Tue|Wed|Thu|Fri|Sat|Sun
  deriving (Eq, Enum, Bounded, Show)
type WeekVect = Day -> REAL
\end{code}

List indexing has type |Int -> REAL|
We need |Day -> REAL|
Helpers from the Haskell Predude:
* |toEnum :: Int -> Day|
* |fromEnum :: Day -> Int|

|ex2| represents website malfunctions as a vector of REALs indexed by Days.
\begin{code}
ex2 :: V REAL Day
ex2 = V (([12,3,12,34,19,2,7]!!) . fromEnum)
data Stat = Weekday | Weekend
  deriving (Eq, Enum, Bounded, Show)
\end{code}

Example: compute average weekday, and average weekend
malfunctions. (Given a vector of nuber of malfunctions per day.)

It is a homomorphism between vector spaces (a linear transformation).
\begin{code}
stat :: V REAL Day  ->  V REAL Stat
stat = mulMV statM
\end{code}

Base vectors in (Day -> REAL):
  e 0 ~= [1,0,0,0,0,0,0]
  e 1 ~= [0,1,0,0,0,0,0]
  e 2 ~= [0,0,1,0,0,0,0]
  ...
  e 6 ~= [0,0,0,0,0,0,1]

\begin{code}
makeMatrix :: (Enum g, Enum g') => [[s]] -> Matrix s g g'
makeMatrix css col = map makeRow css !! fromEnum col
-- Will crash if the list lengths don't match

makeRow :: Enum g => [s] -> V s g
makeRow cs = V (\col -> cs !! fromEnum col)

statM :: Matrix REAL Day Stat
statM = makeMatrix statM'

statM' = [ [ 1/5, 1/5, 1/5, 1/5, 1/5,   0,   0],
           [   0,   0,   0,   0,   0, 1/2, 1/2]]
\end{code}

Some other examples:

* See also L/03/ExerciseSolutions/E3_6_simple.lhs  (Exam/2017-08/P2.lhs)
* Exam/2017-03/P5.lhs
* Exam/2016-Practice/MockE.hs
