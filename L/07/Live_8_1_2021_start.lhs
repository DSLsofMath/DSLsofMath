\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
module DSLsofMath.Live_8_1 where
import DSLsofMath.Algebra
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..),sum)
import Text.Printf
type REAL = Double

class Finite a where
  finiteDomain :: [a]

newtype G = G Int  -- Here used to represent the set {0..6}
  deriving (Eq, Show)

instance Finite G where
  finiteDomain = map G [0..6]
\end{code}

Define vectors and matrices as functions:

\begin{code}
fromList :: [a] -> Vec a G
fromList xs = \(G g) -> xs!!g

v1 :: Vec REAL G   -- G -> REAL
v1 = fromList [1,7,3,8,0,0,0]

type Vec s c = c -> s
type Mat s c r = r -> Vec s c     -- = r -> c -> s

dot ::  (Finite g, Ring s) =>
        Vec s g -> Vec s g -> s
dot v w = error "TODO"

mulMatVec ::  (Finite c, Finite r, Ring s) =>
              Mat s c r -> Vec s c -> Vec s r
mulMatVec = error "TODO"

eval ::  (Finite a, Finite b, Ring s) =>
         Mat s a b  ->  (Vec s a -> Vec s b)
--       Syntax      ->  Semantik (en linjär transformation)
eval = mulMatVec
\end{code}


Matrix multiplication specification

  |H2(eval,     mulMatMat,(.))|

which is the same as

  |H2(mulMatVec,mulMatMat,(.))|

\begin{code}
main_theorem ::  (Finite b, Finite c, Finite a, Ring s) =>
                 Mat s b c -> Mat s a b ->
                 [Vec s a -> Vec s c]
main_theorem m1 m2 =
  [ eval (mulMatMat m1 m2)
  , eval m1 . eval m2
  ]
\end{code}

Matrix multiplication definition:
  (calculation done in lecture 7.2)
\begin{code}
mulMatMat ::
   (Ring s, Finite a, Finite b, Finite c) =>
   Mat s b c -> Mat s a b -> Mat s a c
mulMatMat m1 m2 = flip (mulMatVec m1 . flip m2)
\end{code}

Example matrix:

\begin{code}
mkV :: Eq a => [(a,s)] -> s -> Vec s a
mkV tab def = \i -> maybe def id (lookup i tab)

m3 :: Field s => Mat s G G
m3 (G 0) = mkV [(G 1, 0.4), (G 2, 0.6)] 0
m3 (G 1) = mkV [(G 3, 1.0)] 0
m3 (G 2) = mkV [(G 4, 0.7), (G 5, 0.3)] 0
m3 (G 3) = mkV [(G 6, 1.0)] 0
m3 (G 4) = mkV [(G 1, 0.5), (G 6, 0.5)] 0
m3 (G 5) = mkV [(G 4, 1.0)] 0
m3 (G 6) = mkV [(G 6, 1.0)] 0

square :: (Finite g, Ring s) => Mat s g g -> Mat s g g
square m = mulMatMat m m

ms :: [Mat Fixed5 G G]
ms = iterate square m3
test = ms!!2
\end{code}

Helper code:

\begin{code}
is :: (Eq a, Ring s) => Mat s a a
is i j = if i==j then one else zero

transpose :: Mat s a b -> Mat s b a
transpose m i j = m j i
getCol :: Mat s a b -> a -> Vec s b
getCol = transpose
getRow :: Mat s a b -> b -> Vec s a
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

testm1 :: Ring s => Mat s D2 D3
testm1 = fromLL [ [1,0,1], [2,1,0] ]

testm2 :: Ring s => Mat s D3 D2
testm2 = fromLL [ [1,0], [0,1], [1,1] ]

testm3 :: Ring s => Mat s D2 D2
testm3 = mulMatMat testm2 testm1

instance (Finite a, Finite b, Show s) => Show (Mat s a b) where
  show = showMat

showMat :: (Finite a, Finite b, Show s) => Mat s a b -> String
showMat m = unlines $ map (showVec.m) finiteDomain

showVec :: (Finite a, Show s) => Vec s a -> String
showVec v = unwords $ map (show.v) finiteDomain

newtype Fixed5 = F5 REAL deriving (Additive, AddGroup, Multiplicative, MulGroup)
instance Show Fixed5 where
  show (F5 r) = printf "%.5f" r
\end{code}
----------------------------------------------------------------

"Poor man's proof" of the main theorem

  H2(eval,mulMatMat,(.))  where eval=mulMatVec
or equivalently:
  forall m1, m2. eval (mulMatMat m1 m2) =L= eval m1 . eval m2

Both the LHS and RHS are linear transformations (from a vector space
V to W), thus to compare them we need to apply them to vectors in V.
In general

  h1 =L= h2 iff forall v. h1 v =V= h2 v

It is enough to check this for the base vectors |is r|.
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
    mulMatVec (flip (mulMatVec m1 . flip m2)) (is r) c
  , -- Lemma: mulMatVec m (is j) i == m i j
    flip (mulMatVec m1 . flip m2) c r
  , -- def. of flip
    (mulMatVec m1 . flip m2) r c
  , -- def. of (.)
    (mulMatVec m1 (flip m2 r)) c
  , -- def. of flip m j i = m i j, thus flip m2 r = \i -> m2 i r
    mulMatVec m1 (\i -> m2 i r) c
  , -- def. eval
    eval m1 (\i -> m2 i r) c
  , -- Lemma: mulMatVec m (is j) i == m i j, backwards
    eval m1 (\i -> mulMatVec m2 (is r) i) c
  , -- simplify, def. of eval
    eval m1 (eval m2 (is r)) c
  , -- def. (.)
    (eval m1 . eval m2) (is r) c
  ]

test0 :: Ring s => Mat [s] D2 D2
test0 = main_proof testm2 testm1
\end{code}
