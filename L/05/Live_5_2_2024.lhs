\begin{code}
module Live_5_2 where
import qualified Prelude
import Prelude (Int, Double, Maybe(Nothing, Just), error, reverse, (.), dropWhile, (==), length, zipWith)
import DSLsofMath.Algebra
\end{code}
Domain-Specific Languages of Mathematics course
Chapter 5 (Week 5), Lecture 2, Live coding part.

The Ring of polynomials (as lists of coefficients).

0. Define the DSL (types for syntax & semantics, eval)
1. Define the Ring methods for polynomials (& Power Series)
2. Defined methods for derivative and integral

----------------
0. Define the DSL (types for syntax & semantics, eval)
\begin{code}
\end{code}

----------------
1. Define (some of) the Ring methods for polynomials

Ring a = (Additive a, AddGroup a, Multiplicative a)
          (+), zero,  negate,     (*), one

\begin{code}
\end{code}

----------------
2. Define methods for derivative and integral

Spec. of derP: H₁(evalP, derP, D)
 which expands to 
  forall cs. evalP (derP cs) = D (evalP cs)

\begin{code}
derP :: [REAL] -> [REAL]
derP [] = []
derP (_a:as) = zipWith (*) [1..] as
-- eval [2,1,4] x = 2 + x + 4*x^2
-- eval (derP [2,1,4]) x = 1 + 2*4*x = eval [1*1,2*4]
\end{code}

Specification of integ:
  forall c, cs. derP (integP c cs) = cs

\begin{code}
\end{code}

Example: Power Series and the exponential

\begin{code}
\end{code}

Degree variations
\begin{code}
type Nat = Int
type REAL =Double
degr :: [REAL] -> Maybe Nat
degr = degre . normalize

degre :: Poly REAL -> Maybe Nat
degre [] = Nothing
degre as = Just (length as - one)
-- forall m. z = z + m
-- forall m. -1 = -1 + m

-- eval . normalize = eval
type Poly a = [a]
normalize :: [REAL] -> Poly REAL
normalize = reverse . dropWhile (0==) . reverse
  
addMaybe :: Maybe Nat -> Maybe Nat -> Maybe Nat
addMaybe Nothing Nothing   = Nothing  -- "eval Nothing = -inf"
addMaybe Nothing  (Just m) = Nothing
addMaybe (Just n) Nothing  = Nothing
addMaybe (Just n) (Just m) = Just (n+m)

-- addMaybe = liftMaybe (+)
liftMaybe :: (a->a->a) -> Maybe a -> Maybe a -> Maybe a
liftMaybe op = addMaybe
  where
    addMaybe Nothing Nothing   = Nothing  -- "eval Nothing = -inf"
    addMaybe Nothing  (Just m) = Nothing
    addMaybe (Just n) Nothing  = Nothing
    addMaybe (Just n) (Just m) = Just (op n m)

--    Either Nat Nat

{-
liftMaybe (+) = addMaybe
  where
    addMaybe Nothing Nothing   = Nothing  -- "eval Nothing = -inf"
    addMaybe Nothing  (Just m) = Nothing
    addMaybe (Just n) Nothing  = Nothing
    addMaybe (Just n) (Just m) = Just (n+m)
-}
  
\end{code}
  H2(degree, mulP, (+))  -- false due just to the zero polynomial
  Exists op.  H2(degr, mulP, op)
=
  Exists op.  Forall a, b. degr (mulP a b) = op (degr a) (degr b)

 

























A. Appendix: From Ring to Field.

\begin{spec}
instance Field a => MulGroup (Poly a) where recip = recipP
recipP :: Field a => Poly a -> Poly a
recipP (Poly as) = Poly (recipL as)

recipL :: Field a => [a] -> [a]
recipL [] = error "recipL: division by zero"
recipL (a:as) = r
  where  r  = b:bs
         b  = recip a
         bs = scaleL (negate b) (mulL as r)

test1 :: Field a => Poly a
test1 = takeP 5 (recip (one-xP))
\end{spec}
