\begin{code}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DSLsofMath.NiBo_live where
import DSLsofMath.Algebra
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..),sum,fromIntegral)
import Data.List(nub)
type REAL = Double
type ℕ = Int
infixr 7 *^
\end{code}


+++ DSLsofMath week 7: Elements of Linear Algebra

  
+ Vectors and their spaces 

  
\begin{code}

class (Field s, AddGroup v) => VectorSpace v s where

  (*^) :: s -> v -> v

\end{code}


+ One dimensional spaces

\begin{code}

instance Field s => VectorSpace s s where (*^) = (*)

\end{code}


+ Bases and representations

\begin{spec}

  Forall v (Exists (s0, …, sn) (v == s0 *^ b0 + ... + sn *^ bn))


  b0 ... bn l.i.
    =
  (s0 *^ b0 + ... + sn *^ bn = 0) <=> (s0 = ... = sn = 0)

\end{spec}


+ Syntax for vectors


+ Exercise 7.1: Define a function which takes as input a vector |v| and
   a set of (non-canonical) basis vectors |b_i| and returns the
   coefficients of |v| in that basis.



++ 7.1 Representing vectors as functions

\begin{code}

newtype Vector s g = V (g -> s) deriving (Additive, AddGroup)

infix 9 !
(!) :: Vector s g -> g -> s
V f ! i = f i

type Finite g = (Bounded g, Enum g, Eq g)

finiteDomain :: Finite a => [a]

finiteDomain = [minBound..maxBound]

\end{code}

Instantiating |Enum| only requires defining |toEnum| and |fromEnum|:

\begin{code}

newtype G = G Int deriving (Eq, Show)

instance Bounded G  where  minBound = G 0;  maxBound = G 6

instance Enum G     where  toEnum = G;  fromEnum (G g)  =  g

\end{code}

but it provides a lot of methods: |succ|, |pred| and |enumFrom| ...

succ (G 0) = ?
pred (G 0) = ?
enumFrom (G 0)

\begin{code}

gs :: [G]
gs = finiteDomain

\end{code}

If |S| is a field, |Vector S G| and |S| form a vector space:

\begin{code}

instance Field s => VectorSpace (Vector s g) s where

  (*^) = scaleV

scaleV :: Multiplicative s => s -> Vector s g -> Vector s g

scaleV s (V a) = V (\i -> s * a i)

\end{code}

Why does this work? What guarantees that |Vector s g| is an
instance of |AddGroup|?

+ Exercise 7.2: Show that |Vector S G| satisfies the laws of vector
   spaces

\begin{code}

e :: (Eq g, Ring s) => g -> Vector s g
e i = V (\j -> i `is` j)

is :: (Eq g, Ring s) => g -> g -> s
is i j = if i == j then one else zero

\end{code}


\begin{code}

linComb :: (Finite g, VectorSpace v s) => (g -> s) -> (g -> v) -> v
linComb v e = sum (map (\j -> v j *^ e j) finiteDomain)

\end{code}


+ Exercise 7.3: Using the elements defined above, sketch the isomorphism between an
  abstract vector space and its representation. Recall the definition of
  isomorphism in Section 4.2.3.


++ 7.2 Linear transformations


\begin{code}
 
type Matrix s g g' = g' -> Vector s g

mulMV :: (Finite g, Field s) => Matrix s g g'  ->  Vector s g  ->  Vector s g'
mulMV m (V v)  =  linComb v (transpose m)

transpose :: Matrix s i j -> Matrix s j i
transpose m i = V (\j -> m j ! i)

\end{code}


++ 7.3 Inner products


\begin{code}

class VectorSpace v s => InnerSpace v s where
  inner :: v -> v -> s

sqNorm    ::  InnerSpace v s => v -> s
sqNorm v   =  inner v v

norm   ::  (InnerSpace v a, Algebraic a) => v -> a
norm v  =  sqrt (sqNorm v)

similarity :: (InnerSpace v a, Algebraic a) => v -> v -> a
similarity u v = inner u v / norm u / norm v

\end{code}


+ Dot product

\begin{code}

dot :: (Field s, Finite g) => Vector s g -> Vector s g -> s
dot (V v) (V w) = linComb v w

\end{code}


+ Orthogonal transformations


+ Exercise 7.4: Can you express this condition as a homomorphism
  condition?


+ Exercise 7.5: Prove that orthogonal operators form a monoid with
  multiplication as an operator.

+ Extra exercise



++ 7.4 Examples of matrix algebra


+ 7.4.1 Functions

\begin{code}

instance VectorSpace (REAL -> REAL) REAL where

  s *^ f = (s*) . f

\end{code}


+ 7.4.2 Polynomials and their derivatives


We have seen that

1) Polynomials of degree |n| can be represented by |Vector REAL {0, ..., n}|.

2) Functions |REAL -> REAL| form a vector space: 

     instance VectorSpace (REAL -> REAL) REAL where
       s *^ f = (s*) . f

The *specification* of |evalP|:

  evalP :: Vector REAL {0, ..., n} -> (REAL -> REAL)
  evalP (V v) x = sum (map (\ i -> v i * x^i) [0..n])

suggests that we can define |evalP| as a linear transformation by
defining how |evalP| acts on the canonical basis |e|:

Example (n = 3):

\begin{code}
 
newtype G3 = G3 Int deriving (Eq, Show)

instance Bounded G3  where  minBound = G3 0;  maxBound = G3 3

instance Enum G3     where  toEnum = G3;      fromEnum (G3 g) = g

-- how |evalP| acts on the canonical basis |e|:

evalP3e        ::  G3 -> (REAL -> REAL)
evalP3e (G3 g)  =  \ x -> x^g

evalP3       ::  Vector REAL G3 -> (REAL -> REAL) 
evalP3 (V v)  =  linComb v evalP3e

p3 :: Vector REAL G3
p3  =  (1.0 :: REAL) *^ e (G3 0) +
       (2.0 :: REAL) *^ e (G3 1) +
       (3.0 :: REAL) *^ e (G3 2)

f3 :: REAL -> REAL
f3  = evalP3 p3

\end{code}

Can we define the matrix associated to |evalP|? Why not? What about
derivation?

\begin{code}

newtype G2 = G2 Int deriving (Eq, Show)

instance Bounded G2  where  minBound = G2 0;  maxBound = G2 2

instance Enum G2     where  toEnum = G2;      fromEnum (G2 g) = g

derP3e :: G3 -> Vector REAL G2
derP3e (G3 0) = zero
derP3e (G3 1) = (1.0 :: REAL) *^ e (G2 0)
derP3e (G3 2) = (2.0 :: REAL) *^ e (G2 1)
derP3e (G3 3) = (3.0 :: REAL) *^ e (G2 2)  

derP3 :: Vector REAL G3 -> Vector REAL G2
derP3  (V v) = linComb v derP3e

p2 :: Vector REAL G2
p2  = derP3 p3

\end{code}

Matrix associated to |derP3| is then |transpose derP3e|.

λ> showCols (transpose derP3e) -- should be "[[0.0,0.0,0.0],[1.0,0.0,0.0],[0.0,2.0,0.0],[0.0,0.0,3.0]]"


+ Exercise 7.12: write the (infinite-dimensional) matrix representing
  |D| for power series.

\begin{code}

type PS =  Vector REAL Integer 

derPSe :: Integer -> Vector REAL Integer
derPSe 0 = zero
derPSe n = s *^ e (n - 1)
  where s :: REAL
        s  = fromInteger n

mDerPS :: Matrix REAL Integer Integer  -- note: infinite-dimensional matrix!
mDerPS  = transpose derPSe

\end{code}

λ> take 10 (toLL (col mDerPS 3)) -- shoudl be [0.0,0.0,3.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]


+ Exercise 7.13: compute the matrix |In| associated with integration of
  polynomials.


+ 7.4.3 Inner product for functions and Fourier series (extra material)}

Representation of Fourier series. The *index type* (the type |G| in
|Vector S G|) is

\begin{code}

data Periodic where
  Sin  :: Positive  -> Periodic
  Cos  :: Natural   -> Periodic
  deriving Eq

\end{code}

where |Positive| and |Natural| are aliases for |Integer|:

\begin{code}

type Positive  = Integer
type Natural   = Integer

\end{code}

Notice that |Periodic| is not bounded, thus |Vector REAL Periodic| is
not finite. |Vector REAL Periodic| can represent all functions with
period $2 \pi$.

For example, the function |f x = 3*sin x + cos (2*x) - 1| can be
represented by the vector

\begin{code}

v :: Vector REAL Periodic
v = (3 :: REAL) *^ e (Sin 1) + e (Cos 2) - e (Cos 0)

\end{code}  

+ Exercise 7.7: Derive |derive| for this representation.
    
Like for the case of polynomial functions, we define |derive| from a
specification

\begin{spec}

eval (derive (e (Sin i))) = D (eval (e (Sin i)))
eval (derive (e (Cos i))) = D (eval (e (Cos i)))

\end{spec}

where |eval :: Vector REAL Periodic -> (REAL -> REAL)| fulfils the specification:

\begin{code}
evalPerSpec :: (Transcendental s, Eq s) => (Vector s Periodic -> s -> s) -> Periodic -> s -> Bool
evalPerSpec eval (Cos k) =  eval (e (Cos k)) === \ x -> cos (fromInteger k * x)
evalPerSpec eval (Sin k) =  eval (e (Sin k)) === \ x -> sin (fromInteger k * x)

(===) :: Eq b => (a->b) -> (a->b) -> (a -> Bool)
f === g = \x -> f x == g x
\end{code}

\begin{spec}

eval (s *^ e (Sin k)) = \ x -> s * sin (k * x)
eval (s *^ e (Cos k)) = \ x -> s * cos (k * x)

\end{spec}

We have

\begin{spec}

eval (derive (Sin i))       =  {- spec. of |derive| -}
D (eval (Sin i)))           =  {- spec. of |eval| -}
D (\ x -> sin (i * x))      =  {- derivative of |sin|, chain rule -}
\ x -> i * cos (i * x)      =  {- by def. of |eval|, |(*^)| -}
eval (i *^ (Cos i))

\end{spec}

and similarly for |Cos i|. Thus, the specification for |derive| is
fulfilled by

\begin{spec}

  derive (Sin i)  =    i *^ (Cos i)
  derive (Cos i)  =  - i *^ (Sin i)

\end{spec}

This suggests defining |derive| as

\begin{code}

derive :: Vector REAL Periodic -> Vector REAL Periodic
derive (V v) = V v' where
  v' (Sin i) = - (fromInteger i) * v (Cos i)
  v' (Cos i) =   (fromInteger i) * v (Sin i)

\end{code}

We are not able to evaluate representations at this point: we have not
implemented |eval| and we cannot implement it, do you see why? But we
can apply |derive|. For example, to compute |derive v|. Because |v| is
the representation of |3 * sin x + cos (2 * x) - 1|, we expect

\begin{code}

v' :: Vector REAL Periodic
v' = derive v

\end{code}

to be the representation of |3 * cos x - 2 * sin (2 * x)| and test |v'|
for correctness by inspecting some of its coefficients:

λ> v' ! (Cos 1) --  3.0

λ> v' ! (Sin 2) -- -2.0


7.4.4 Simple deterministic systems (transition systems)


\begin{code}

next1 :: G -> G
next1 (G 0)  =  G 1
next1 (G 1)  =  G 3
next1 (G 2)  =  G 5
next1 (G 3)  =  G 6
next1 (G 4)  =  G 6
next1 (G 5)  =  G 4
next1 (G 6)  =  G 5

m1 :: Matrix REAL G G
m1 = transpose (e . next1)

\end{code}

λ> showCols m1 -- should be equal to M from page 191

λ> mulMV m1 (e (G 3) + e (G 4)) -- [0.0,0.0,0.0,0.0,0.0,0.0,2.0] !

\begin{code}

poss :: (Finite g, Field s) => Int -> Matrix s g g -> Vector s g -> [Vector s g]
poss n m v = take (n + 1) (iterate (mulMV m) v)

\end{code}

λ> poss k m1 (e (G 3) + e (G 4)) 

----

\begin{code}

-- next1 :: G -> (G -> REAL)
-- next1 (G 0) (G 1)  =  1.0
-- next1 (G 0) _      =  0.0
-- next1 (G 1) (G 3)  =  1.0
-- next1 (G 1) _      =  0.0
-- next1 (G 2) (G 5)  =  1.0
-- next1 (G 2) _      =  0.0
-- next1 (G 3) (G 6)  =  1.0
-- next1 (G 3) _      =  0.0
-- next1 (G 4) (G 6)  =  1.0
-- next1 (G 4) _      =  0.0
-- next1 (G 5) (G 4)  =  1.0
-- next1 (G 5) _      =  0.0
-- next1 (G 6) (G 5)  =  1.0
-- next1 (G 6) _      =  0.0

-- m1 :: Matrix REAL G G
-- m1 = transpose (V . next1)

\end{code}

----


7.4.5 Non-deterministic systems


+ Exercise 7.14 start with |e 2 + e 3| and iterate a number of times, to
  get a feeling for the possible evolutions.

-- \paragraph*{Remark (Nicola, lecture + book):} TODO 7.14


\begin{code}

next2 :: G -> (G -> Bool)
next2 (G 0) (G g)      =   g == 1 || g == 2
next2 (G 1) (G g)      =   g == 3
next2 (G 2) (G g)      =   g == 4 || g == 5
next2 (G 3) (G g)      =   g == 6
next2 (G 4) (G g)      =   g == 1 || g == 6
next2 (G 5) (G g)      =   g == 4
next2 (G 6) (G g)      =   False

m2 :: Matrix Bool G G
m2 = transpose (V . next2)

\end{code}

λ> showCols m2 -- should be equal to M from page 194 as Bool

λ> mulMV m1 (e (G 3) + e (G 4)) -- [0.0,0.0,0.0,0.0,0.0,0.0,2.0] !

λ> mulMV m2 (e (G 2) + e (G 3)) -- !

\begin{code}

instance Additive        Bool where  zero    =  False;  (+)     =  (||)
instance Multiplicative  Bool where  one     =  True;   (*)     =  (&&)

instance AddGroup  Bool where  negate  =  error "negate: not used"
instance MulGroup  Bool where  recip   =  id

\end{code}

λ> poss k m2 (e (G 3) + e (G 4))

Extra: what if we define |next| in terms of lists instead of "vectors"?

\begin{code}

next2L :: G -> [G]
next2L (G 0) = [G 1, G 2]
next2L (G 1) = [G 3]
next2L (G 2) = [G 4, G 5]
next2L (G 3) = [G 6]
next2L (G 4) = [G 1, G 6]
next2L (G 5) = [G 4]
next2L (G 6) = []

possL :: (Eq a) => Int -> (a -> [a]) -> [a] -> [[a]]
possL n next xs = take (n + 1) (iterate (nub . concat . (map next)) xs)

-- poss n m v = take (n + 1) (iterate (mulMV m) v)

\end{code}

λ> possL 1 next2L [G 3, G 4]


7.4.6 Stochastic systems

We can try to follow the same approach as for deterministic and
 non-deterministic systems:

1) Define the transition function |next| (Exercise 7.15).

2) Apply |next| to define a l.t. |f (V v) = linComb c (f . e)| by
   defining how |f| transforms the vectors of the canonical basis.

\begin{code}

next3 :: G -> (G -> REAL)
 
next3 (G 0) (G 1)  =  0.4
next3 (G 0) (G 2)  =  0.6
next3 (G 0) (G _)  =  0.0

next3 (G 1) (G 3)  =  1.0
next3 (G 1) (G _)  =  0.0
  
next3 (G 2) (G 4)  =  0.7
next3 (G 2) (G 5)  =  0.3  
next3 (G 2) (G _)  =  0.0

next3 (G 3) (G 6)  =  1.0
next3 (G 3) (G _)  =  0.0

next3 (G 4) (G 1)  =  0.5
next3 (G 4) (G 6)  =  0.5  
next3 (G 4) (G _)  =  0.0

next3 (G 5) (G 4)  =  1.0
next3 (G 5) (G _)  =  0.0

next3 (G 6) (G 6)  =  1.0
next3 (G 6) (G _)  =  0.0  

m3 :: G -> Vector REAL G
m3 = transpose (V . next3)


\end{code}

λ> col m3 (G 0) -- should be the first column of |M| at page 196

Now we can iterate matrix-vector multiplication with our |poss|
combinator (Exercise 7.15):

λ> poss k m3 (e (G 0))
  
Extra exercise: what if we had tried to define |next3 : G -> [(G,
E REAL)]|?


\begin{code}

next3L :: G -> [(G, REAL)]
next3L (G 0) = [(G 1, 0.4), (G 2, 0.6)]
next3L (G 1) = [(G 3, 1.0)]
next3L (G 2) = [(G 4, 0.7), (G 5, 0.3)]
next3L (G 3) = [(G 6, 1.0)]
next3L (G 4) = [(G 1, 0.5), (G 6, 0.5)]
next3L (G 5) = [(G 4, 1.0)]
next3L (G 6) = [(G 6, 1.0)]

\end{code}

λ> possL 2 next3L [G 3, G 4] -- !

We cannot apply |possL| to |next3L|, in order to "iterate" |nex3L|, we
W have to implement another function:

\begin{code}

poss3L :: (Eq a) => Int -> (a -> [(a, REAL)]) -> [(a, REAL)] -> [[(a, REAL)]]
poss3L n next xps = take (n + 1) (iterate (step next) xps)

step :: (Eq a) => (a -> [(a, REAL)]) -> [(a, REAL)] -> [(a, REAL)]
step sys aps = concat (map g (map f aps))
  where f (a, p) = (sys a, p)
        g (aps, p) = map (\ (a', p') -> (a', p' * p)) aps

\end{code}

What are the types of |f| and |g|? 

\begin{code}

prob :: (Eq a) => a -> [(a, REAL)] -> REAL
prob a [] = 0
prob a ((x,p) : xps) = if a == x
                       then p + prob a xps
                       else     prob a xps

\end{code}
  
What does |prob| compute? Evaluate

λ> prob (G 6) (last (poss3L k next3L [(G 0, 1.0)]))

for |k| = 1, 2, ... What do you get?

            
+ 7.4.7 Quantum Systems (extra material) 



++ 7.5 Monadic dynamical systems (extra material)

+ Exercise 7.8 (extra material, hard): Write |Monad| instances for |Id|,
  |Powerset|, |Prob|, |Super|.


+ 7.5.1 The monad of linear algebra (extra material)

The idea is that vectors on finite types are finite functors and monads:

\begin{code}

class FinFunc f where
  func    ::  (Finite g, Finite g') => (g -> g') -> f g -> f g'

class FinMon f where
  embed   ::  Finite g => g -> f g
  bind    ::  Finite g => f g -> (g -> f g') -> f g'


instance Field s => FinFunc (Vector s) where
  func   =  funcFinFunc

instance Field s => FinMon  (Vector s) where
  embed  =  embedFinM
  bind   =  bindFinM

funcFinFunc :: (Field s, Finite g, Finite g') => (g -> g') -> Vector s g -> Vector s g'
funcFinFunc = mulMV . invImage

embedFinM :: (Eq g, Ring s) => g -> Vector s g
embedFinM = e

bindFinM :: (Field s, Finite g) => Vector s g -> (g -> Vector s g') -> Vector s g'
bindFinM v m  = mulMV (transpose m) v   

\end{code}

where |invImage f g'| is the function that computes the inverse image of
|g'| through |f| as a characteristic function: |invImage f g' g = one|
if |f g == g'| and |zero| otherwise: 

\begin{code}

invImage :: (Field s, Eq g') => (g -> g') -> g' -> Vector s g
invImage f = \ g' -> V (is g' . f)  

\end{code}

It is worth pointing out the role of |f| in |func f v|:

\begin{code}

f :: G2 -> G3
f (G2 0) = G3 1
f (G2 1) = G3 1
f (G2 2) = G3 1

u :: Vector REAL G2
u  =  e (G2 0) + e (G2 1) + e (G2 2)

\end{code}

λ> funcFinFunc f (e (G2 0))

λ> funcFinFunc f u

---------------


-- % We can rewrite the |g'|th component of |func f v| in terms of the dot
-- % product
-- % %
-- % \begin{spec}
-- %   dot v (V (\ g -> is g' (f g)))
-- % =
-- %   dot v (V (is g' . f))
-- % \end{spec}
-- % %
-- % This shows that the role of |f| in |func f v| is that of
-- % re-distributing the values of |v| onto the new vector.
-- %
-- % \begin{exercise}
-- %   show that if |w = func f v| then the sum of the components of |w| is
-- %   equal to the sum of the components of |v|.
-- % \end{exercise}
-- %
-- %
-- % \begin{exercise}
-- % \begin{enumerate}
-- % \item Prove that the functor laws hold, i.e.
-- % %
-- % \begin{spec}
-- % func id       =  id
-- % func (g . f)  =  func g . func f
-- % \end{spec}
-- %
-- % \item Prove that the monad laws hold, i.e.
-- % %
-- % \begin{spec}
-- % bind v return      =  v
-- % bind (return g) f  =  f g
-- % bind (bind v f) h  =  bind v (\ g' -> bind (f g') h)
-- % \end{spec}
-- %
-- % \item What properties of |S| have you used to prove these properties?
-- % %
-- %   Define a new type class |GoodClass| that accounts for these (and
-- %   only these) properties.
-- % \end{enumerate}
-- % \end{exercise}

-- %*TODO: Proving that |func| preserves composition and that |bind|
-- % associates gets very messy if one directly operates with their
-- % definitions.
-- % %
-- % It is probably simpler to go back to standard linear algebra notation
-- % (with indexed |sum|, etc.) which in a sense seems to defeat the purpose
-- % of focusing on the syntax.


++ 7.6 Associated code

Conversions and |Show| functions so that we can actually see our
C vectors.

\begin{code}

col :: Matrix s g g' -> g -> Vector s g'
col m g = V (\ g' -> m g' ! g)

toL :: Finite g => Vector s g -> [s]
toL (V v) = map v finiteDomain

toLL :: Vector s Integer -> [s]
toLL (V v) = map v [0..]

showVector :: (Finite g, Show s) => Vector s g -> String
showVector = show . toL

instance (Finite g, Show s) => Show (Vector s g) where
  show = showVector

showRows :: (Finite g, Finite g', Show s) => Matrix s g g' -> String
showRows = showFun

showCols :: (Finite g, Finite g', Show s) => Matrix s g g' -> String
showCols m = showRows (transpose m)

showFun :: (Finite a, Show b) => (a -> b) -> String
showFun f = show (map f finiteDomain)

-- instance (Finite g, Show s) => Show (g->s)        where  show = showFun

\end{code}

-- %*TODO: perhaps convert to using the |newtype Vector|.
-- %*TODO: Perhaps include parts of the below (after rewriting)
-- %if False
-- The scalar product of two vectors is a good building block for matrix
-- multiplication:
-- %
-- %
-- \begin{code}
-- dot' ::  (Finite g, Ring s) =>
--          (g->s) -> (g->s) -> s
-- dot' v w = sum (map (v * w) finiteDomain)
-- \end{code}
-- %
-- Note that |v * w :: g -> s| is using the function instance of |Multiplicative|.

-- Using it we can shorten the definition of |mulMV|

-- \begin{spec}
--   mulMV m v g'
-- = -- Classical definition
--   sum [m g' g * v g | g <- finiteDomain]
-- = -- replace list comprehension with |map|
--   sum (map (\g -> m g' g * v g) finiteDomain)
-- = -- use |FunNumInst| for |(*)|
--   sum (map (m g' * v) finiteDomain)
-- = -- Def. of |dot'|
--   dot' (m g') v
-- \end{spec}
-- %
-- Thus, we can define matrix-vector multiplication by
-- %
-- \begin{spec}
-- mulMV m v g' =  dot' (m g') v
-- \end{spec}
-- %
-- We can even go one step further:
-- %
-- \begin{spec}
--   mulMV m v
-- = -- Def.
--   \g' -> dot' (m g') v
-- = -- |dot'| is commutative
--   \g' -> dot' v (m g')
-- = -- Def. of |(.)|
--   dot' v . m
-- \end{spec}
-- %
-- to end up at
-- %
-- \begin{code}
-- mulMV' ::  (Finite g, Ring s) =>
--            Mat s g g' ->  Vec s g  ->  Vec s g'
-- mulMV' m v  =  dot' v . m

-- \end{code}

-- \begin{code}
-- type Mat s r c = c -> r -> s
-- type Vec s r = r -> s

-- linComb' :: (Finite g, VectorSpace v s) => (g->s) -> (g->v) -> v
-- linComb' a v = sum (map (\j -> a j *^ v j) finiteDomain)

-- mulMV'' ::  (Finite g, Field s) =>
--            Mat s g g' ->  Vec s g  ->  Vec s g'
-- mulMV'' m v  =  linComb' v . m

-- checkTypes3 :: (Finite b, Field s) => Mat s a b -> Mat s b c -> a -> [Vec s c]
-- checkTypes3 m1 m2 i =
--   [ getCol (mulM m2 m1) i
--   , evalMV m2 (getCol m1 i)
--   ]

-- mulM :: (Finite b, Field s) => Mat s b c -> Mat s a b -> Mat s a c
-- mulM m2 m1 = flip (evalMV m2 . flip m1)

-- evalMV :: (Finite a, Field s) => Mat s a b -> Vec s a -> Vec s b
-- evalMV m v = linComb v . m

-- \end{code}

-- Similarly, we can define matrix-matrix multiplication:
-- %
-- \begin{code}
-- mulMM' ::  (Finite b, Ring s) =>
--            Mat s b c   ->  Mat s a b  ->  Mat s a c
-- mulMM' m1 m2 = \r c -> mulMV' m1 (getCol m2 c) r

-- transpos :: Mat s g g' -> Mat s g' g
-- transpos m i j = m j i

-- getCol :: Mat s g g' -> g -> Vec s g'
-- getCol = transpos

-- getRow :: Mat s g g' -> g' -> Vec s g
-- getRow = id
-- \end{code}
-- %endif

-- % -- Specification: (a * b) * v == a * (M * v)
-- % -- Specification: mulMV (mulMM a b) v == mulMV a (mulMV b v)
-- % -- Specification: mulMV (mulMM a b) == (mulMV a) . (mulMV b)
-- % -- Specification: eval (mulMM a b) == (eval a) . (eval b)
-- %
-- %   eval (mulMM a b)
-- % = -- spec.
-- %   (eval a) . (eval b)
-- % = -- def. of |eval|
-- %   (\w -> dot w . a) . (\v -> dot v . b)
-- % = -- def. of |(.)|
-- %   \v -> (\w -> dot w . a) ((\v -> dot v . b) v)
-- % = -- simplification
-- %   \v -> (\w -> dot w . a) (dot v . b)
-- % = -- simplification
-- %   \v -> (dot (dot v . b) . a)
-- % ... gets too complicated for this chapter

-- %%include E7.lhs

-- %endif
