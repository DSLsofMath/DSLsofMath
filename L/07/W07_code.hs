{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DSLsofMath.W07 where
import DSLsofMath.Algebra
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..),sum)
import Data.List(nub)
type REAL = Double
type ℕ = Int

infixr 7 *^

class (Field s, AddGroup v) => VectorSpace v s where
  (*^) :: s -> v -> v

instance Field s => VectorSpace s s where (*^) = (*)

newtype Vector s g    = V (g -> s) deriving (Additive, AddGroup)

infix 9 !
(!) :: Vector s g -> g -> s
V f ! i = f i

type Finite g = (Bounded g, Enum g, Eq g)
finiteDomain :: Finite a => [a]
finiteDomain = [minBound..maxBound]

instance Field s => VectorSpace (Vector s g) s  where  (*^) = scaleV

scaleV :: Multiplicative s => s -> Vector s g -> Vector s g
scaleV s (V a) = V (\i -> s * a i)

e :: (Eq g, Ring s) => g -> Vector s g
e i = V (\j -> i `is` j)

is :: (Eq g, Ring s) => g -> g -> s
is i j = if i == j then one else zero

linComb :: (Finite g, VectorSpace v s) => (g->s) -> (g->v) -> v
linComb v e = sum (map (\j -> v j *^ e j) finiteDomain)

linComb1 :: (Finite g, Ring s) => (g->s) -> (g->s) -> s
linComb1 as vs = sum [as j * vs j | j <- finiteDomain]

linComb2 :: (Finite g, VectorSpace v s) => (g->s) -> (g->v) -> v
linComb2 as vs = sum [as j *^ vs j | j <- finiteDomain]

linLaw :: (Finite g,
           Eq v',
           VectorSpace v s,
           VectorSpace v' s) =>
  (g -> v) -> (v -> v') -> (g -> s) -> Bool
linLaw e f v = f (linComb v e) == linComb v (f . e)

linLaw' ::
  (Finite g, Field s, VectorSpace v' s, Eq v') =>
  (Vector s g -> v') -> (g -> s) -> Bool
linLaw' f v = linLaw e f v

linLawS ::
  (Finite g, Field s, Eq s) =>
  (Vector s g -> s) -> (g -> s) -> Bool
linLawS = linLaw'

linLawV ::
  (Finite g, Field s, Eq (Vector s g')) =>
  (Vector s g -> Vector s g') -> (g -> s) -> Bool
linLawV = linLaw'

checkTypes :: (Finite g, Field s, Eq s) =>
  (Vector s g -> Vector s g') -> Vector s g -> g' -> [s]
checkTypes f (V v) g' =
  let m = f . e in
  [   f (V v) ! g'
  ,   (linComb v m) ! g'
  ,   linComb v (\g -> m g ! g')
  ,   linComb v (\g -> f (e g) ! g')
  ]


checkTypes2 :: (Finite g, Field s) => Matrix s g g -> g -> g -> [s]
checkTypes2 m k i = let V ek = e k in
  [ (mulMV m (V ek)) ! i
  , (linComb ek (transpose m)) ! i
  , m i ! k
  ]


type Matrix s g g' = g' -> Vector s g

mulMV :: (Finite g, Field s) => Matrix s g g'  ->  Vector s g  ->  Vector s g'
mulMV m (V v)  =  linComb v (transpose m)

transpose :: Matrix s i j -> Matrix s j i
transpose m i = V (\j -> m j ! i)

class VectorSpace v s => InnerSpace v s where
  inner :: v -> v -> s

sqNorm :: InnerSpace v s => v -> s
sqNorm v = inner v v

norm :: (InnerSpace v a, Algebraic a) => v -> a
norm v = sqrt (sqNorm v)

similarity u v = inner u v / norm u / norm v

dot :: (Field s, Finite g) => Vector s g -> Vector s g -> s
dot (V v) (V w) = linComb v w

instance VectorSpace (REAL->REAL) REAL where
   s *^ f = (s*) .f

evalM :: G -> (REAL -> REAL)
evalM (G i) = \x -> x^i

evalP, evalP' :: Vector REAL G -> (REAL -> REAL)
evalP (V v) x = sum (map (\i -> v i * evalM i x) finiteDomain)

evalP' (V v) = linComb v evalM

data Periodic where
  Sin  :: Positive  -> Periodic
  Cos  :: Natural   -> Periodic
  deriving Eq

type Positive  = Integer
type Natural   = Integer

testf x = 3*sin x + cos (2*x) - 1
testv :: Vector REAL Periodic
testv = (3::REAL) *^ e (Sin 1) + e (Cos 2) - e (Cos 0)

v :: Vector REAL Periodic
v = V vf
  where  vf (Sin  1)  = 3
         vf (Cos  2)  = 1
         vf (Cos  0)  = -1
         vf _         = 0

newtype G = G Int deriving (Eq, Show)

instance Bounded G  where  minBound = G 0;  maxBound = G 6

instance Enum G     where  toEnum = G;  fromEnum (G g)  =  g

next1 :: G -> G
next1 (G 0)  = G 1;  {-"\qquad"-}  next1 (G 1) =  G 3;
next1 (G 2)  = G 5;                next1 (G 3) =  G 6;
next1 (G 4)  = G 6;                next1 (G 5) =  G 4;
next1 (G 6)  = G 5

proofSteps m g' =
  let  f :: Vector REAL G -> Vector REAL G
       f (V v) = linComb v (e . next1)
  in
  [ m g'                                   {- |m| is the matrix version of |f| -}
  , V (\ g -> (f (e g))           ! g')    {- by the spec. of |f| -}
  , V (\ g -> (e (next1 g))       ! g')    {- by def. of |e| -}
  , V (\ g -> (V (is (next1 g)))  ! g')    {- by def. of |(!)| -}
  , V (\ g -> is (next1 g) g')
  , V (\ g -> is g' (next1 g))
  , V (is g' . next1)
  ]

testProofSteps = proofSteps m1

m1 :: Ring s => G -> Vector s G
m1 g' = V (is g' . next1)

t1' :: Vector REAL G
t1'  = mulMV m1 (e (G 3) + e (G 4))
t1   = toL t1'               -- |[0,0,0,0,0,0,2]|

poss :: (Finite g, Field s) => Int -> Matrix s g g -> Vector s g -> [Vector s g]
poss n m v = take (n + 1) (iterate (mulMV m) v)

testPoss0 :: [Vector REAL G]
testPoss0 = poss 6 m1 (e (G 3) + e (G 4))

poss1 :: Int -> (a -> a) -> [a] -> [[a]]
poss1 n next xs  = take (n + 1) (iterate (map next) xs)

testPoss1 = poss1 6 next1 [G 3, G 4]

poss1' :: (Eq a) => Int -> (a -> a) -> [a] -> [[a]]
poss1' n next xs = take (n + 1) (iterate (nub . map next) xs)

testPoss1' = poss1' 6 next1 [G 3, G 4]

f2 :: G -> (G -> Bool)
f2 (G 0) (G g)      =   g == 1 || g == 2
f2 (G 1) (G g)      =   g == 3
f2 (G 2) (G g)      =   g == 4 || g == 5
f2 (G 3) (G g)      =   g == 6
f2 (G 4) (G g)      =   g == 1 || g == 6
f2 (G 5) (G g)      =   g == 4
f2 (G 6) (G g)      =   False

m2 g' = V (\ g -> f2 g g')

instance Additive        Bool where  zero    =  False;  (+)     =  (||)
instance Multiplicative  Bool where  one     =  True;   (*)     =  (&&)

instance AddGroup  Bool where  negate  =  error "negate: not used"
instance MulGroup  Bool where  recip   =  id

t2' = mulMV m2 (e (G 3) + e (G 4))
t2 = toL t2'  -- |[False,True,False,False,False,False,True]|

f3 :: G -> Vector REAL G

m3 ::  G -> Vector REAL G

mkV :: Eq g => [(g,s)] -> s -> Vector s g
mkV tab def = V (\g -> maybe def id (lookup g tab))

-- f3 :: G -> Vector REAL G
f3 (G 0) = mkV [(G 1, 0.4), (G 2, 0.6)] 0
f3 (G 1) = mkV [(G 3, 1.0)] 0
f3 (G 2) = mkV [(G 4, 0.7), (G 5, 0.3)] 0
f3 (G 3) = mkV [(G 6, 1.0)] 0
f3 (G 4) = mkV [(G 1, 0.5), (G 6, 0.5)] 0
f3 (G 5) = mkV [(G 4, 1.0)] 0
f3 (G 6) = mkV [(G 6, 1.0)] 0

f3' :: G -> Vector REAL G
f3' (G 0) = mkV [(G 1, 0.4), (G 2, 0.6)] 0.0
f3' (G 1) = mkV [(G 3, 1.0)] 0.0
f3' (G 2) = mkV [(G 5, 1.0)] 0.0
f3' (G 3) = mkV [(G 6, 1.0)] 0.0
f3' (G 4) = mkV [(G 1, 0.4), (G 6, 0.4), (G 2, 0.2)] 0.0
f3' (G 5) = mkV [(G 4, 1.0)] 0.0
f3' (G 6) = mkV [(G 6, 1.0)] 0.0

m3 g' = V (\ g -> (f3 g) ! g')

class FinFunc f where
  func :: (Finite a, Finite b) => (a -> b) -> f a -> f b

class FinMon f where
  embed   ::  Finite a => a -> f a
  bind    ::  (Finite a, Finite b) => f a -> (a -> f b) -> f b

instance Ring s => FinFunc (Vector s) where
  func f (V v) = V (\ g' -> sum [v g | g <- finiteDomain, g' == f g])

instance Field s => FinMon (Vector s) where
  embed  = embedFinM
  bind   = bindFinM

embedFinM :: (Eq a, Ring s) => a -> Vector s a
embedFinM g = V (is g)

bindFinM :: (Field s, Finite a) => Vector s a -> (a -> Vector s b) -> Vector s b
bindFinM (V v) f  =  V (\ g' -> linComb v (\g -> f g ! g'))

testBindCalc1 :: (Finite i, Field s) => Matrix s i j -> Vector s i -> [Vector s j]
testBindCalc1 m (V v) =
  [ mulMV m (V v)
  , linComb v (transpose m)
  ]
testBindCalc2 m (V v) =
  [ mulMV m (V v)
  , linComb v (transpose m)
  , linComb v (\i -> V (\j -> m j ! i))
  , linComb v (\i -> V (\j -> m j ! i))
  , V (\ j -> linComb v (\i -> f i ! j))
  ]
  where f = transpose m

-- ``poor man's proofs'' of the |linComb|-|V| lemma
qq :: (Finite j, Field s) => (j -> s) -> (j -> Vector s i) -> [Vector s i]
qq a v = let v0 = linComb a v in map (\v -> v - v0) $
  [ linComb a v                                             -- def. |linComb|
  , sum (map (\j -> a j *^ v j) finiteDomain)               -- def. |(*^)|
  , sum (map (\j -> V (\i -> a j * v j ! i)) finiteDomain)  -- lemma |qq2|
  ]
  ++
    qq2 a (\j i -> v j ! i) finiteDomain
  ++
  [ V (\i -> sum (map (\j -> a j *  v j ! i) finiteDomain)) -- def. |(*^)| for |REAL|
  , V (\i -> sum (map (\j -> a j *^ v j ! i) finiteDomain)) -- def. |linComb|
  , V (\i -> linComb a (\j -> v j ! i))
  ]

qq2 :: Ring s => (j->s) -> (j->i->s) -> [j] -> [Vector s i]
qq2 a f js = let y j = \i -> a j * f j i in
  [ sum (map (\j -> V (\i -> a j * f j i))  js)  -- introduce shorthand |y|
  , sum (map (\j -> V (y j))                js)  -- def. map for (:)
  , V (\i -> sum (map (\j -> y j i)         js)) -- def. |y|
  , V (\i -> sum (map (\j -> a j * f j i)   js))
  ]

qq3 :: Additive s => (j -> i -> s) -> [j] -> [Vector s i]
-- qq3 y js =
--       sum (map (\j -> V (y j)) js)
--    == V (\i -> sum (map (\j -> y j i) js))
qq3 y [] = let q1 j = V (\i -> y j i)
               q2 i = \j -> y j i      in
  [ sum (map q1 [])                -- def. |map| for |[]|
  , sum []                         -- def. |sum| for |[]|
  , zero                           -- def. |zero| for |Vector|
  , V zero                         -- def. |zero| for functions
  , V (\i -> zero)                 -- def. |sum| for |[]|
  , V (\i -> sum [])               -- def. |map| for |[]|
  , V (\i -> sum (map (q2 i) []))  -- def. |map| for |[]|
  ]
qq3 y (j:js) =
  [ sum (map (\j -> V (y j)) (j:js))                -- def. |map| for |(:)|
  , sum (V (y j) : map (\j -> V (y j)) js)          -- def. |sum| for |(:)|
  , V (y j) + sum (map (\j -> V (y j)) js)          -- ind. hyp
  , V (y j) + V (\i -> sum (map (\j -> y j i) js))  -- def. |(+)| for Vector
  , V (y j  +  \i -> sum (map (\j -> y j i) js))    -- def. |(+)| for functions
  , V (\i -> y j i + sum (map (\j -> y j i) js))    -- def. |sum| for |(:)|
  , V (\i -> sum (y j i : map (\j -> y j i) js))    -- def. |map| for |(:)|
  , V (\i -> sum (map (\j -> y j i) (j:js)))
  ]

toL :: Finite g => Vector s g -> [s]
toL (V v) = map v finiteDomain

instance (Finite g, Show s) => Show (g->s)        where  show = showFun
instance (Finite g, Show s) => Show (Vector s g)  where  show = showVec

showVec :: (Finite g, Show s) => Vector s g -> String
showVec (V v) = showFun v

showFun :: (Finite a, Show b) => (a->b) -> String
showFun f = show (map f finiteDomain)

dot' ::  (Finite g, Ring s) =>
         (g->s) -> (g->s) -> s
dot' v w = sum (map (v * w) finiteDomain)

mulMV' ::  (Finite g, Ring s) =>
           Mat s g g' ->  Vec s g  ->  Vec s g'
mulMV' m v  =  dot' v . m


type Mat s r c = c -> r -> s
type Vec s r = r -> s

linComb' :: (Finite g, VectorSpace v s) => (g->s) -> (g->v) -> v
linComb' a v = sum (map (\j -> a j *^ v j) finiteDomain)

mulMV'' ::  (Finite g, Field s) =>
           Mat s g g' ->  Vec s g  ->  Vec s g'
mulMV'' m v  =  linComb' v . m

checkTypes3 :: (Finite b, Field s) => Mat s a b -> Mat s b c -> a -> [Vec s c]
checkTypes3 m1 m2 i =
  [ getCol (mulM m2 m1) i
  , evalMV m2 (getCol m1 i)
  ]

mulM :: (Finite b, Field s) => Mat s b c -> Mat s a b -> Mat s a c
mulM m2 m1 = flip (evalMV m2 . flip m1)

evalMV :: (Finite a, Field s) => Mat s a b -> Vec s a -> Vec s b
evalMV m v = linComb v . m


mulMM' ::  (Finite b, Ring s) =>
           Mat s b c   ->  Mat s a b  ->  Mat s a c
mulMM' m1 m2 = \r c -> mulMV' m1 (getCol m2 c) r

transpos :: Mat s g g' -> Mat s g' g
transpos m i j = m j i

getCol :: Mat s g g' -> g -> Vec s g'
getCol = transpos

getRow :: Mat s g g' -> g' -> Vec s g
getRow = id

