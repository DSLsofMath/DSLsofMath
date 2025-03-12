\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module DSLsofMath.Live_7_2 where
import DSLsofMath.FunNumInst
type REAL = Double
\end{code}

Define vectors and matrices as functions:

\begin{code}
type Vec s c = c -> s
type Mat s c r = r -> Vec s c -- = r -> c -> s

dot ::  (Finite g, Num s) =>
        Vec s g -> Vec s g -> s
dot v w = sum (map (v * w) finiteDomain)
  -- (*) :: Vec s c -> Vec s c -> Vec s c
  -- v * w = \i -> v i * w i   -- using the Num (x->s) instance

mulMatVec ::  (Finite c, Finite r, Num s) =>
              Mat s c r -> Vec s c -> Vec s r
mulMatVec m v = \r -> dot (m r) v
  -- m r :: Vec s c
  -- v   :: Vec s c

eval ::  (Finite g, Finite g', Num s) =>
         Mat s g g'  ->  (Vec s g -> Vec s g')
eval = mulMatVec
\end{code}

Matrix multiplication specification

  |H2(eval,     mulMatMat,(.))|

which is the same as

  |H2(mulMatVec,mulMatMat,(.))|

\begin{code}
main_theorem ::  (Finite b, Finite c, Finite a, Num s) =>
                 Mat s b c -> Mat s a b ->
                 [Vec s a -> Vec s c]
main_theorem m1 m2 =
  [ eval (mulMatMat m1 m2)
  , eval m1 . eval m2
  ]
\end{code}

Matrix multiplication definition:

\begin{code}
mulMatMat' ::
  (Num s, Finite a, Finite b, Finite c) =>
  Mat s b c -> Mat s a b -> Mat s a c
mulMatMat' m1 m2 = \r c -> mulMatVec m1 (getCol m2 c) r

class Enum a => Finite a where
  finiteDomain :: [a]

newtype G = G Int  -- Here used to represent the set {0..6}
  deriving Show

instance Enum G where
  toEnum = G
  fromEnum (G i) = i

instance Finite G where
  finiteDomain = map G [0..6]

tabulate2 :: (Finite t1, Finite t2) => (t1 -> t2 -> s) -> (t1 -> t2 -> s)
tabulate2 fun = \r c -> (table!!fromEnum r) !! fromEnum c
  where table = map (\r -> map (\c -> fun r c) finiteDomain) finiteDomain

mulMatMat ::
  (Num s, Finite a, Finite b, Finite c) =>
  Mat s b c -> Mat s a b -> Mat s a c
mulMatMat m1 m2 = tabulate2 (mulMatMat' m1 m2)
\end{code}


Example matrix:

\begin{code}

m :: Mat REAL G G
m (G 1) (G 0) = 0.9
m (G 2) (G 0) = 0.1
m (G 0) (G 1) = 0.9
m (G 3) (G 1) = 0.1
m (G 3) (G 3) = 0.9
m (G 0) (G 2) = 1
m r c = 0

square :: (Finite g, Num s) => Mat s g g -> Mat s g g
square m = mulMatMat m m

ms :: [Mat REAL G G]
ms = iterate square m
test = ms!!2
\end{code}

Helper code:

\begin{code}
is i j = if i==j then 1 else 0

transpose :: Mat s g g' -> Mat s g' g
transpose m i j = m j i
getCol :: Mat s g g' -> g -> Vec s g'
getCol = transpose
getRow :: Mat s g g' -> g' -> Vec s g
getRow = id

type D2 = Bool
type D3 = Maybe Bool
instance Finite Bool where finiteDomain = [False,True]
instance Enum a => Enum (Maybe a) where
  fromEnum Nothing = 0
  fromEnum (Just a) = 1+fromEnum a
  toEnum 0 = Nothing
  toEnum n = Just (toEnum (n-1))

instance Finite a => Finite (Maybe a) where
  finiteDomain = Nothing : map Just finiteDomain

fromLL :: (Enum i, Enum j) => [[s]] -> Mat s i j
fromLL xss r c = (xss!!fromEnum c)!!fromEnum r

toLL :: (Finite i, Finite j) => Mat s i j -> [[s]]
toLL m = [[m i j | i<- finiteDomain] | j <- finiteDomain]

testm1 :: Mat REAL D2 D3
testm1 = fromLL [ [1,0,1], [2,1,0] ]

testm2 :: Mat REAL D3 D2
testm2 = fromLL [ [1,0], [0,1], [1,1] ]

testm3 :: Mat REAL D2 D2
testm3 = mulMatMat testm2 testm1

instance (Finite a, Finite b, Show s) => Show (Mat s a b) where
  show = showMat

showMat :: (Finite a, Finite b, Show s) => Mat s a b -> String
showMat m = unlines $ map (showVec.m) finiteDomain

showVec :: (Finite a, Show s) => Vec s a -> String
showVec v = unwords $ map (show.v) finiteDomain

\end{code}
----------------------------------------------------------------

"Poor man's proof" of the main theorem

  H2(eval,mulMatMat,(.))  where eval=mulMatVec
or equivalently:
  forall m1, m2. eval (mulMatMat m1 m2) =L= eval m1 . eval m2

Both the LHS and RHS are linear transformations (from a vector space V to W), thus to compare them we need to apply them to vectors in V. In general
  h1 =L= h2 iff forall v. h1 v =V= h2 v
It is enough to check this for the basis vectors (is r).
The resulting vectors then need to be compared:
  v1 =V= v2 iff forall i. v1 i == v2 i

This can be combined to
  h1 =L= h2 iff forall r, c.  h1 (is r) c == h2 (is r) c
or in our specific case:
  forall m1, m2, r, c.
    eval (mulMatMat m1 m2) (is r) c == (eval m1 . eval m2) (is r) c



\begin{code}
main_proof m1 m2 r c =
  [ eval (mulMatMat m1 m2) (is r) c
  , -- def. eval
    mulMatVec (mulMatMat m1 m2) (is r) c
  , -- def. mulMatMat
    mulMatVec (\r c -> mulMatVec m1 (getCol m2 c) r) (is r) c
  , -- Lemma: mulMatVec m (is j) i == m i j
    (\r c -> mulMatVec m1 (getCol m2 c) r) c r
  , -- simplify
    mulMatVec m1 (getCol m2 r) c
  , -- def. eval, getCol
    eval m1 (\x->m2 x r) c
  , -- Lemma: mulMatVec m (is j) i == m i j
    eval m1 (mulMatVec m2 (is r)) c
  , -- def. (.), eval
    (eval m1 . eval m2) (is r) c
  ]

test0 = main_proof testm2 testm1
test0' = \i j -> map toRational (test0 i j)
test0'' = \i j -> map round (test0 i j)
\end{code}
