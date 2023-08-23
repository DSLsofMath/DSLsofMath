\begin{code}
module Typing_Euler where
import Prelude (Bool, (&&), Eq((==)), Double, undefined, Show)
import DSLsofMath.Algebra
type REAL = Double
\end{code}
Euler and the unit circle

The complex plane is the plane formed by the complex numbers, with a
Cartesian coordinate system the real part along the x-axis, and the
imaginary part along the y-axis.

Euler's Formula, 

  \(|euler| \, \varphi = e^{i\varphi} = \cos \varphi + i * \sin \varphi\),

establishes the fundamental relationship between the
trigonometric functions and the complex exponential function.

The unit circle

  \[S^1 = \{ a + i b \mid a^2 + b ^ 2 = 1, a \in |REAL|, b \in |REAL|  \}\]

is the range of the function |euler|.

The elements of type |S1| form a |Group|:
\begin{code}
instance Multiplicative S1 where one = oneS1; (*) = mulS1
instance MulGroup S1 where recip = recipS1
\end{code}

a) Give, and explain, the types of \(\varphi\), |euler| and |i|.

\begin{code}
phi    ::  REAL
euler  ::  REAL -> S1
i      ::  S1  -- Complex

phi = undefined
euler = eulera
eulera v = S1a (cos v) (sin v)
eulerb = S1b 
i = ia
ia = S1a 0 1
ib = S1b (pi/2)
-- Alt. A:
data S1a = S1a REAL REAL deriving (Eq, Show)
-- Alt. B:
data S1b = S1b REAL -- just the angle in radians
instance Eq S1b where (==) = eqS1b
eqS1b :: S1b -> S1b -> Bool
eqS1b (S1b v) (S1b w) = sin v == sin w && cos v == cos w
\end{code}

b) Give the types of |oneS1|, |mulS1|, |recipS1|.

\begin{code}
type S1 = S1a
oneS1    ::  S1
mulS1    ::  S1 -> S1 -> S1
recipS1  ::  S1 -> S1
\end{code}
  
c) Implement |oneS1|, |mulS1|, |recipS1| in Haskell.

\begin{code}
oneS1 = S1a 1 0
mulS1 (S1a re1 im1) (S1a re2 im2) = S1a re3 im3
  where re3 = re1*re2-im1*im2; im3 = re1*im2+im1*re2
  -- (re1+im1*i)*(re2+im2*i) = (re1*re2-im1*im2) + (re1*im2+im1*re2)*i
recipS1 (S1a re1 im1) = S1a re1 (negate im1)
  -- (re1+im1*i)*(re1-im1*i) = re1^2+im1^2 = 1 
\end{code}

In recip for a general complex number we would also need to divide by
n2 = re1^2+im1^2 but that (should be) 1 on the unit circle so we can
skip the division.

d) State the property that a function |h : A -> B| is a group homomorphism in first order logic.

GroupHom(h,(A,mulA,recipA, unitA),(B,mulB,recipB,unitB)) =
  H2(h,mulA,mulB) && H0(h,unitA,unitB) &&
  H1(h,recipA,recipB)

H2(h,opA,opB)  =  Forall x, y : A.   h (opA x y) == opB (f x) (f y)
H1(h,opA,opB)  =  Forall x : A.      h (opA x)   == opB (f x)
H0(h,opA,opB)  =                     h  opA      == opB

e) Prove that |euler| is a group homomorphism.

Step 1: Check types:
|euler| takes REAL with addition to S1 with multiplication.

Step 2: 
GH = GroupHom(euler,(REAL,(+),negate,0),(S1,mulS1,recipS1,oneS1))
   = H2(euler,(+),mulS1) && H0(euler,0,oneS1) && H1(euler,negate,recipS1)
   = GH2 && GH0 && GH1 -- name the parts of the theorem

Step 3: Prove the three parts

Step 3.0:
  GH0 =   euler 0 == oneS1
Equational reasoning:
  euler 0
== -- def.
  S1a (cos 0) (sin 0)
== -- trig.
  S1a 1 0
== -- def. of oneS1
  oneS1

Step 3.1:
  GH1 =   Forall v : REAL. euler (negate v) == recipS1 (euler v)
Equational reasoning:
  euler (negate v)
= -- def. euler & negate
  S1a (cos (-v)) (sin (-v))
= -- trig.
  S1a (cos v) (-sin v)
= -- def. recipS1
  recipS1 (S1a (cos v) (sin v))
= -- def. euler
  recipS1 (euler v)

Step 3.2:
  GH2 = Forall v, w : REAL.  euler (v+w) == mulS1 (euler v) (euler w)
Equational reasoning:
  euler (v+w)
= -- def. euler
  S1a (cos (v+w)) (sin (v+w))
= -- trig. of sum
  S1a (cos v * cos w - sin v * sin w) (sin v * cos w + cos v * sin w)
= -- def. mulS1
  mulS1 (S1a (cos v) (sin v)) (S1a (cos w) (sin w))
= -- def. euler (twice)
  mulS1 (euler v) (euler w)




  
  
