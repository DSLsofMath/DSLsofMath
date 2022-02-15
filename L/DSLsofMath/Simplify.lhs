\begin{code}
module DSLsofMath.Simplify where
import qualified Data.Map as Map
import Data.List (sort)
import DSLsofMath.FunExp
import Prelude hiding (Num(..), Fractional(..), Floating(..), (^))
import DSLsofMath.Algebra
import DSLsofMath.FunExpInst ()
\end{code}

Some test cases:

\begin{code}
x3 = (X:*:X):*:X
x3' = derive x3
x3'' = derive x3'
x3''' = derive x3''

t1 = simplify x3'
t2 = simplify x3''
t3 = simplify x3'''

xm12 = (X-one)^2
xm12' = derive xm12
t4 = simplify xm12'
\end{code}

\begin{code}
expr1 :: FunExp
expr1 = (Const 2) :*: (Exp (Exp X))
expr1' :: FunExp
expr1' = derive expr1
\end{code}

Test:

\begin{code}
test1' = simplify expr1'
\end{code}

There are many possible implementations of |simplify| and it is a good
exercise to try to implement your own version. Spoiler below.



































This is one possibility.

SoP = Sum of Products

\begin{code}
newtype SoP = SoP [(REAL,[FunExp])] deriving (Eq, Show)
  -- sorted on the second component of the pairs, no duplicates

simplify :: FunExp -> FunExp
simplify = sop2FunExp . sop

sop :: FunExp -> SoP
sop X           = SoP [(1,[X])]
sop (Const c)   = SoP (mayPrefixTerm c [] [])
sop (x:+:y)     = sopAdd (sop x) (sop y)
sop (x:*:y)     = sopMul (sop x) (sop y)
sop (Negate x)  = sopNeg (sop x)
sop (Recip x)   = sopRecip (sop x)
sop (Sin x)     = sopSin (sop x)
sop (Cos x)     = sopCos (sop x)
sop (Exp x)     = sopExp (sop x)

-- Use only as a last resort
embedSoP :: FunExp -> SoP
embedSoP e = SoP [(1,[e])]

sopAdd :: SoP -> SoP -> SoP
sopAdd (SoP xss) (SoP yss) = SoP (addMerge xss yss)

addMerge :: Ord a => [(REAL, a)] -> [(REAL, a)] -> [(REAL, a)]
addMerge [] yss = yss
addMerge xss [] = xss
addMerge ((cx,xs):xss) ((cy,ys):yss) = case compare xs ys of
  LT -> (cx,xs)      : addMerge xss ((cy,ys):yss)
  EQ -> mayPrefixTerm (cx+cy) xs (addMerge xss yss)
  GT -> (cy,ys)      : addMerge ((cx,xs):xss) yss

sopNeg :: SoP -> SoP
sopNeg (SoP xss) = SoP (map (mapFst negate) xss)
  -- sorting not affected, only the coefficients

mapFst :: (c1->c2) -> (c1, a) -> (c2, a)
mapFst f (c,x) = (f c, x)

mayPrefixTerm :: REAL -> a -> [(REAL,a)] -> [(REAL,a)]
mayPrefixTerm 0 _ = id
mayPrefixTerm c t = ((c, t):)

sopMul :: SoP -> SoP -> SoP
sopMul (SoP xss) (SoP yss) = SoP (distr xss yss)

distr :: Ord a => [(REAL,[a])] -> [(REAL,[a])] -> [(REAL,[a])]
distr [] _yss = []  -- 0*e=0
distr _xss [] = []  -- e*0=0
distr xss yss = combineCoeffs [mulPair cxs cys | cxs <- xss, cys <- yss]

mulPair :: Ord a => (REAL,[a]) -> (REAL,[a]) -> (REAL,[a])
mulPair (cx,xs) (cy,ys) = (cx*cy, mulMerge xs ys)

mulMerge :: Ord a => [a] -> [a] -> [a]
mulMerge xs ys = sort (xs++ys)

combineCoeffs :: Ord a => [(REAL,a)] -> [(REAL,a)]
combineCoeffs =  filter ((0/=).fst) .
                 map swap . Map.assocs .
                 Map.fromListWith (+) . map swap

swap :: (a,b) -> (b,a)
swap (c,t) = (t, c)

uniqOn :: Ord b => (a -> b) -> [a] -> [a]
uniqOn key = Map.elems . Map.fromList . map (\ a -> (key a, a))

sopRecip :: SoP -> SoP
sopRecip (SoP [])        = SoP [(recip 0, [])]
sopRecip (SoP [(c,xs)])  = SoP (mayPrefixTerm (recip c) (map simpRec xs) [])
sopRecip e               = embedSoP (Recip (sop2FunExp e))

getConst :: SoP -> Maybe REAL
getConst (SoP [])        = Just 0
getConst (SoP [(c,[])])  = Just c
getConst _               = Nothing

sopSin, sopCos, sopExp :: SoP -> SoP
sopSin = sopTransOp sin Sin
sopCos = sopTransOp cos Cos
sopExp = sopTransOp exp Exp

sopTransOp :: (REAL->REAL) -> (FunExp -> FunExp) -> SoP -> SoP
sopTransOp sem syn xss = case getConst xss of
  Just c   -> SoP (mayPrefixTerm (sem c) [] [])
  Nothing  -> embedSoP (syn (sop2FunExp xss))

-- Convert back to a normal FunExp

sop2FunExp :: SoP -> FunExp
sop2FunExp (SoP [])   = Const 0
sop2FunExp (SoP xss)  = foldr1 (:+:) (map term2FunExp xss)

term2FunExp :: (REAL, [FunExp]) -> FunExp
term2FunExp (0,_)   = Const 0  -- should not occur
term2FunExp (1,[])  = Const 1
term2FunExp (1,xs)  = foldr1 (:*:) xs
term2FunExp (c,xs)  = foldr1 (:*:) (Const c : xs)
\end{code}

Possible extensions:

1) Make the product part of the "sum of products" use the same
   represenation as the sum part: a list of pairs of factors and
   their exponents. Then multiplication and recip could be handled in
   much the same way as addition and negation.
   (Then X*X := X^2, X*Recip X := X^0 := 1, ...)

2) Extend the representation to rational expressions (a ratio of two
   of the current SoP).

\begin{code}
size :: FunExp -> Int
size (f:+:g) = 1 + size f + size g
size (f:*:g) = 1 + size f + size g
size (Const _) = 1 -- or 2
size X         = 1
size (Negate f)= 1 + size f
size (Recip f) = 1 + size f
size (Sin f)   = 1 + size f
size (Cos f)   = 1 + size f
size (Exp f)   = 1 + size f
\end{code}



An older version below (does not simplify X+one+X+one).

Note the structure with "semantic functions" to help keeping track of
what is "already simplified". (Helps to avoid infinite recursion.)

\begin{code}
type SimpleExp = FunExp

simplify'  ::  FunExp -> SimpleExp
simplify' (x :+: y)   = simpAdd (simplify' x) (simplify' y)
simplify' (x :*: y)   = simpMul (simplify' x) (simplify' y)
simplify' (Exp x)     = simpExp (simplify' x)
simplify' (Sin x)     = simpSin (simplify' x)
simplify' (Cos x)     = simpCos (simplify' x)
simplify' (Negate x)  = simpNeg (simplify' x)
simplify' (Recip x)   = simpRec (simplify' x)
simplify' X           = X
simplify' (Const c)   = Const c

simpAdd :: SimpleExp -> SimpleExp -> SimpleExp
simpAdd (Const a)  (Const b)  = Const (a+b)
simpAdd (Const 0)  x          = x
simpAdd x          (Const 0)  = x
simpAdd (x:+:y)    z          = simpAdd x (y:+:z)
simpAdd (Const a)  (Const b:+:x) = simpAdd (Const (a+b)) x
simpAdd x          y  = case scaledEq x y of
  Left (a,b,x) -> simpMul (Const (a+b)) x
  Right (x,y)  -> x :+: y
\end{code}

More work is needed to handle constants well.

\begin{code}
simpMul :: SimpleExp -> SimpleExp -> SimpleExp
simpMul (Const a)  (Const b)  = Const (a*b)
simpMul (Const 0)  _x         = Const 0
simpMul _x         (Const 0)  = Const 0
simpMul (Const 1)  x          = x
simpMul x          (Const 1)  = x
simpMul (x:*:y)    z          = simpMul x (y:*:z)
simpMul x          (Const c)  = simpMul (Const c) x
simpMul (Const a)  (Const b :*: x) = simpMul (Const (a*b)) x
simpMul x          (Const c :*: y) = simpMul (Const c) (x :*: y)
simpMul x          y          = x :*: y

simpNeg :: SimpleExp -> SimpleExp
simpNeg (Const c)   = Const (negate c)
simpNeg (Negate x)  = x
simpNeg x           = Negate x

simpRec :: SimpleExp -> SimpleExp
simpRec (Const c)   = Const (recip c)
simpRec (Recip x)   = x
simpRec x           = Recip x

simpExp :: SimpleExp -> SimpleExp
simpExp (Const c)  = Const (exp c)
simpExp x          = Exp x

simpSin :: SimpleExp -> SimpleExp
simpSin (Const c)  = Const (sin c)
simpSin x          = Sin x

simpCos :: SimpleExp -> SimpleExp
simpCos (Const c)  = Const (cos c)
simpCos x          = Cos x

scaledEq x y                         | x==y = Left (1,1,x)
scaledEq            x  (Const b:*:y) | x==y = Left (1,b,x)
scaledEq (Const a:*:x)            y  | x==y = Left (a,1,x)
scaledEq (Const a:*:x) (Const b:*:y) | x==y = Left (a,b,x)
scaledEq x y = Right (x,y)
\end{code}
