\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
module Live_6_2 where
import DSLsofMath.Algebra as Algebra
import qualified Prelude
import Prelude (Eq, Ord, Show, Int, Double, Rational, id, const, (.), error)
import DSLsofMath.Simplify
import DSLsofMath.PSDS -- Polynomials and Power Series
import DSLsofMath.FunExp
\end{code}

----------------
L6.2 live coding on solving ODEs

Solve   f'' + 2*f' + f = sin,  f 0 = 2, f' 0 = 1

Step 0: transform to power series with this specification:
     f = eval as; f' = eval as'; f'' = eval as''
-- introduce instead of f, f', f'' the series as, as', as''
     sin = eval sinP
Step 1: compute terms for the rhs
Step 2: solve for the highest derivative:
  f''  = sin  - f  -        2*f'
  as'' = sinP - as - scaleP 2 as'
Step 3: fill in "integ-equations" for as' and as
Step 4: If you do this by hand: fill in the coefficient lists step by step.
\begin{code}
sinP, cosP :: Field a => PS a
sinP = error "TODO"
cosP = error "TODO"

as, as', as'' :: Field a => PS a
as'' = error "TODO"
as'  = error "TODO"
as   = error "TODO"
\end{code}

sinP =      :      :
cosP =      :      :
as'' =      :      :
as'  =      :      :
as   =      :      :

----------------
Step 5: Checking

\begin{code}
lhs :: Field a => Int -> (a -> a)

lhs n =  f'' + 2*f' + f
  where  f   = evalPS n as
         f'  = evalPS n as'
         f'' = evalPS n as''

rhs :: Transcendental a => Int -> a -> a
rhs n =  evalPS n sinP

testEq n x = lhs n x - rhs n x -- should be close to zero
\end{code}

Don't forget to check the original equation (often catches simple
 mistakes).

f' + f = 1

notRhs = 1:1:1:1.... -- not correct!
  eval notRhs x = 1+x+x^2+x^3...
rhs = 1:0:0:0:0: ...





























-------------- Later -------------------




\begin{code}
{-
type REAL = Double
data FunExp  =  Const REAL
             |  X
             -- Additive, AddGroup
             |  FunExp :+: FunExp
             |  Negate FunExp
             -- Multiplicative, MulGroup
             |  FunExp :*: FunExp
             |  Recip FunExp
             -- Transcendental
             |  Exp FunExp
             |  Sin FunExp
             |  Cos FunExp
                -- and so on
  deriving (Eq, Ord, Show)
-}
\end{code}

\begin{code}
{-
eval :: Transcendental a => FunExp -> a -> a

-- Base cases
eval (Const c)      =  evalConst c
eval X              =  id

-- Additive, AddGroup
eval (e1 :+: e2)    =  eval e1 + eval e2
eval (Negate e)     =  negate (eval e)

-- Multiplicative, MulGroup
eval (e1 :*: e2)    =  eval e1 * eval e2
eval (Recip e)      =  recip (eval e)

-- Transcendental
eval (Exp e)        =  exp (eval e)
eval (Sin e)        =  sin (eval e)
eval (Cos e)        =  cos (eval e)
-}

evalConst :: Field a => REAL -> a -> a
evalConst c = const (real2Field c)

real2Field :: Field a => REAL -> a
real2Field real = val
  where  val  = Algebra.fromRational rat
         rat  = Prelude.toRational real
\end{code}

Syntactic instances
\begin{code}
{-
instance Additive       FunExp where (+) = (:+:); zero = Const 0
instance Multiplicative FunExp where (*) = (:*:); one  = Const 1
instance AddGroup       FunExp where negate = Negate
instance MulGroup       FunExp where recip  = Recip
instance Transcendental FunExp where pi=Const pi; exp=Exp; sin=Sin; cos=Cos
-}
\end{code}

More DS instances:

\begin{code}
test1 :: Transcendental a => a -> a
test1 = eval (Exp (Negate (X:*:X)))

test1REAL :: REAL
test1REAL = test1 1

test1PS :: PS REAL
test1PS = test1 xP
\end{code}
Check with argument types REAL, FunExp, PS REAL, DS FunExp

-- TODO needs instance Transcendental for DS
test1DS :: DS REAL
test1DS = test1 xDS

\begin{code}
test2 :: Transcendental a => a -> a
test2 x = exp (negate (x*x))

instance Field a => MulGroup (DS a)             where recip = recipDS
instance Transcendental a => Transcendental (DS a) where
  pi=piDS;exp=expDS;sin=sinDS;cos=cosDS

recipDS :: Field a => DS a -> DS a
recipDS = error "recipDS: TODO implement!"

piDS :: Transcendental a => DS a
piDS = DS [pi]
expDS :: Transcendental a => DS a -> DS a
sinDS :: Transcendental a => DS a -> DS a
cosDS :: Transcendental a => DS a -> DS a
expDS = error "expDS: TODO implement!"
sinDS = error "sinDS: TODO implement!"
cosDS = error "cosDS: TODO implement!"
\end{code}


----------------------------------------------------------------

\begin{code}
type PowerSeries = PS

instance (Eq a, Transcendental a) => Transcendental (PowerSeries a) where
   pi   =  P [pi]
   exp  =  expPS
   sin  =  sinPS
   cos  =  cosPS

expPS, sinPS, cosPS :: (Eq a, Transcendental a) => PS a -> PS a
expPS  as  = integP  (exp  (val as))  (expPS as   * derP as)
sinPS  as  = integP  (sin  (val as))  (cosPS as   * derP as)
cosPS  as  = integP  (cos  (val as))  (-sinPS as  * derP as)

val ::  Additive a => PS a  ->  a
val (P (a:_))   =   a
val _           =   zero


fe2ps :: (Eq r, Transcendental r) => FunExp -> PS r
fe2ps (Const c)    =  P [real2Field c]
fe2ps (e1 :+: e2)  =  fe2ps e1 + fe2ps e2
fe2ps (e1 :*: e2)  =  fe2ps e1 * fe2ps e2
fe2ps X            =  xP
fe2ps (Negate e)   =  negate  (fe2ps e)
fe2ps (Recip e)    =  recip   (fe2ps e)
fe2ps (Exp e)      =  exp     (fe2ps e)
fe2ps (Sin e)      =  sin     (fe2ps e)
fe2ps (Cos e)      =  cos     (fe2ps e)
\end{code}
