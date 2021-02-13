Homomorphism2(h,op1,op2) = forall x, y. h (op1 x y) == op2 (h x) (h y)
  where  h   :: A1 -> A2
         op1 :: A1 -> A1 -> A1
         op2 :: A2 -> A2 -> A2

Homomorphism1(h,op1,op2) = forall x. h (op1 x) == op2 (h x)
  where  h   :: A1 -> A2
         op1 :: A1 -> A1
         op2 :: A2 -> A2

Homomorphism0(h,op1,op2) = h op1 == op2
  where  h   :: A1 -> A2
         op1 :: A1
         op2 :: A2

Example:

Homomorphism2(eval,(:+:),(+)) = forall x, y. eval(x:+:y) == (eval x)+(eval y)
  where  eval  :: FunExp -> FunSem
         (:+:) :: FunExp -> FunExp -> FunExp
         (+)   :: FunSem -> FunSem -> FunSem

Homomorphism1(eval,Exp,exp) = forall e. eval(Exp e) == exp (eval e)
  where  eval  :: FunExp -> FunSem
         Exp   :: FunExp -> FunExp
         exp   :: FunSem -> FunSem


P is a polynomial function if

  P(x) = a_n x^n + a_{n-1} x^{n - 1} + \cdots + a_1 x + a_0

where $a_n$, $a_{n-1}$, \ldots, $a_1$, and $a_0$, called the
\textbf{coefficients} of the polymonial [misspelled in the book], are
constants and, if $n > 0$, then $a_n ≠ 0$.
%
The number $n$, the degree of the highest power of $x$ in the
polynomial, is called the \textbf{degree} of the polynomial.
%
(The degree of the zero polynomial is not defined.)
\end{quote}


\begin{code}
module Live_5_1_2019 where
import Data.Complex
import Data.Ratio
import Text.Printf
import DSLsofMath.FunNumInst


-- a polynomial = abstract syntax
-- a polynomial function = semantics :: REAL -> REAL

type Poly' a = [(a,Int)] -- pair the coefficient with its exponent
type Poly a = [a]

p1 :: Num a => Poly a
p1 = [2,0,1] --  2 + x^2 = 2*x^0 + 0*x^1 + 1*x^2

eval :: Num a => Poly a -> (a->a)
eval []     = const 0
eval (c:cs) = const c  +   id * eval cs

-- A polynomial function(?) Chebyshev
ch n x = cos (fromInteger n*acos x)

addP :: Num a => Poly a -> Poly a -> Poly a
addP (c:cs) (d:ds) = (c+d) : addP cs ds
addP cs     []     = cs        -- eval [] = const 0
addP []     ds     = ds
-- addP is almost, but not quite, zipWith (+)

derP :: (Enum a, Num a) => Poly a -> Poly a
derP [] = []
derP (_:cs) = zipWith (*) cs [1..]

intP :: (Enum a, Fractional a) => a -> Poly a -> Poly a
intP c cs = c : zipWith (/) cs [1..]
\end{code}

intP c [3,2,1] --  I (\x->3+2*x+x^2) = \x->3*x+x^2+(1/3)*x^3+c
 == [c,3,1,1/3]


Specification of intP:
  forall c, cs. derP (intP c cs) = cs



mulP :: Num a => Poly a -> Poly a -> Poly a

  addP x y :: Poly a          -- syntax
  eval (addP x y) :: (a->a)   -- semantic domain
  (eval x) :: (a->a)

  forall x, y. eval (addP x y)  =?= (eval x) + (eval y)
  ? exists addP. H2(eval,addP,(+))
  ? exists mulP. H2(eval,mulP,(*))
   -- do case analysis on the syntax


ch0 x = ch 0 x = cos (0*acos x) = cos 0 = 1
ch0 = const 1

ch1 x = ch 1 x = cos (1*acos x) = cos (acos x) = x
ch1 = id

ch2 x = ch 2 x = cos (2*acos x) =? eval [c0,c1,c2] x
-- Find a few values:
  ch2 0    = cos (2*acos 0)    = cos (2*(pi/2)) = cos pi = -1 = c0
  ch2 1    = cos (2*acos 1)    = cos (2*0) = cos 0 = 1
  ch2 (-1) = cos (2*acos (-1)) = cos (2*pi) = 1

  ch2 x =? c0 + c1*x + c2*x^2

  1 = ch2 1 =? -1 + c1*1 + c2*1   simplifies to   1 = -1 + c1 + c2

-- c0 = -1

  eval [c0,c1,c2] x
=
  c0 + x*eval [c1,c2] x

=> eval [c0,c1,c2] 0 =   c0 + 0*eval [c1,c2] 0 = c0


Index in the list is the exponent.

Poly' allows a sparse representation
p1' = [(1,0),(1,1738)] ~= 1 + x^1738  [may need an invariant]

p2 = --
p3 = --












type Poly = []

evalP :: Num a => Poly a -> (a -> a)
evalP []      = const 0
evalP (c:cs)  = const c + id * evalP cs


From polynomial function to a polynomial (parsing?):

Specification: (naming: "lave" = reverse "eval")
  lave :: Fractional a => Int -> (a->a) -> Poly a
  forall x. eval (lave n f) x = f x

(Should work if f is actually a polynomial function of degree n.)

General idea: evaluate the polynomial at (n+1) points and solve for the coefficients.
Best choice of points are (n+1)-roots of unity.

\begin{code}

i = 0:+1
coeff :: Integer -> Integer -> Integer -> Complex Double
coeff n j k = exp(2*pi*i*fromRational ((j*k) % (n+1)))
  where
lave n f = as
  where (_xs, _ys, as) = lave' n f

lave' n f = (xs, ys, map a [0..n])
  where
    xs = map (coeff n (-1)) [0..n]
    ys = map f xs
    a j = sum (zipWith (*) ys (map (coeff n j) [0..n])) / fromInteger (n+1)

test :: Integer -> [String]
test n = map ((printf "%5.1f").realPart) (lave n (ch n))

\end{code}

Example:

[-1.0000000000016485,-6.769534427968954e-13,50.00000000000137,-6.201100239360874e-13,-400.0000000000004,2.191055417907509e-12,1119.9999999999998,-1.5141019751106135e-12,-1279.9999999999993,6.614506921984933e-13,511.9999999999998]
