{-# LANGUAGE FlexibleContexts #-}
\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RebindableSyntax #-}
module Live_6_3_FunExp where
import DSLsofMath.Algebra as Algebra
import qualified Prelude
import Prelude (Eq, Ord, Show, Int, Double, Rational, id, const, (.), error)
import DSLsofMath.Simplify
import DSLsofMath.PSDS -- Polynomials and Power Series
-- import DSLsofMath.FunExp   -- Partially implemented below
type REAL = Double
\end{code}

\begin{code}
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
\end{code}
Additive ((+), zero)
AddGroup(negate, (-))
Multiplicative((*), one)
MulGroup(recip, (/))
\begin{code}
eval :: Transcendental a => FunExp -> a -> a
eval X              =  id
eval (Const c)      =  evalConst c
eval (e1 :+: e2)    =  eval e1 + eval e2
eval (Negate e)     =  negate (eval e)
eval (e1 :*: e2)    =  eval e1 * eval e2
eval (Recip e)      =  recip (eval e)
eval (Exp e)        =  exp (eval e)
eval (Sin e)        =  sin (eval e)
eval (Cos e)        =  cos (eval e)

evalConst :: Field a => REAL -> a -> a
evalConst c = const (real2Field c)

real2Field :: Field a => REAL -> a
real2Field real = val
  where  val  = Algebra.fromRational rat
         rat  = Prelude.toRational real
\end{code}

Syntactic instances
\begin{code}
instance Additive       FunExp where (+) = (:+:); zero = Const 0
instance Multiplicative FunExp where (*) = (:*:); one  = Const 1
instance AddGroup       FunExp where negate = Negate
instance MulGroup       FunExp where recip  = Recip
instance Transcendental FunExp where pi=Const pi; exp=Exp; sin=Sin; cos=Cos
\end{code}

More instances:

\begin{code}
test1 :: Transcendental a => a -> a
test1 = eval (Exp (Negate (X:*:X)))

test1REAL :: REAL
test1REAL = test1 1

test1PS :: PS REAL
test1PS = test1 xP

test1DS :: DS REAL
test1DS = test1 xDS

\end{code}
Check with argument types REAL, FunExp, PS REAL, DS FunExp

-- TODO needs instance Transcendental for DS

\begin{code}
test2 :: Transcendental a => a -> a
test2 x = exp (negate (x*x))

recipDS :: Field a => DS a -> DS a
recipDS = error "recipDS: TODO implement!"

instance Field a => MulGroup (DS a)             where recip = recipDS
instance Transcendental a => Transcendental (DS a) where
  pi=piDS;exp=expDS;sin=sinDS;cos=cosDS

piDS :: Transcendental a => DS a
piDS = DS [pi]

expDS :: Transcendental a => DS a -> DS a
expDS fs = integDS (exp (val0 fs)) (expDS fs * derDS fs)

-- λ> takeDS 10 (test1 xDS)
-- DS [1.0,-0.0, -2.0,0.0, 12.0, -0.0, -120.0,0.0,1680.0,-0.0]
-- λ> takeP 10 (test1 xP)
-- P  [1.0, 0.0, -1.0,0.0, 0.5,  0.0,  -0.16666666666666666,0.0,4.1666666666666664e-2,0.0]

val0 :: Additive a => DS a -> a
val0 (DS []) = zero
val0 (DS (f0:_)) = f0

sinDS :: Transcendental a => DS a -> DS a
cosDS :: Transcendental a => DS a -> DS a
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


\end{code}
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
