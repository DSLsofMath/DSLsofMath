
1. [25p] Algebraic structure: ring

\begin{code}
{-# LANGUAGE GADTs #-}
import qualified Prelude
import Prelude hiding (Num(..))

law1 a b c =     (a + b) + c == a + (b + c)
law2 a     =     a + zero == a
law3 a     =     a + (neg a) == zero
law4 a b c =     a ⋅ (b + c) == (a ⋅ b) + (a ⋅ c)
\end{code}

\item Define a type class |Ring| that corresponds to the structure
  |ring|.

\begin{code}
class Ring a where
  (+)   :: a -> a -> a
  zero  :: a
  neg   :: a -> a

  (⋅)   :: a -> a -> a
  one   :: a
\end{code}
\item Define a datatype |R v| for the language of ring expressions
  (with variables of type |v|) and define a |Ring| instance for it.
  (These are expressions formed from applying the ring operations to
  the appropriate number of arguments, e.g., all the left hand sides
  and right hand sides of the above equations.)

\begin{code}
data R v where
  Add   :: R v -> R v -> R v
  Zero  :: R v
  Neg   :: R v -> R v
  Mul   :: R v -> R v -> R v
  One   :: R v
  V     :: v -> R v
 deriving Show
instance Ring (R v) where
  (+) = Add; zero = Zero; neg = Neg; (⋅) = Mul; one = One
\end{code}

\item Find and implement two other instances of the |Ring| class. Make
  sure the laws are satisfied.  \label{item:inst}

Integers is the classical ring, and the function instance just lifts
 all operations pointwise (and thus preservees all the laws).

\begin{code}
instance Ring Integer where
  (+) = (Prelude.+); zero = 0; neg = Prelude.negate; (⋅) = (Prelude.*); one = 1
instance Ring a => Ring (k->a) where
  (+) = addf; zero = zerof; neg = negf; (⋅) = mulf; one = onef

addf  :: Ring a => (k->a) -> (k->a) -> (k->a)
addf f g = \x -> f x + g x
negf  :: Ring a => (k->a) -> (k->a)
negf f = \x -> neg (f x)
zerof :: Ring a => (k->a)
zerof = \_ -> zero
onef  :: Ring a => (k->a)
onef = \_ -> one
mulf  :: Ring a => (k->a) -> (k->a) -> (k->a)
mulf f g = \x -> f x ⋅ g x
\end{code}
\item Give a type signature for, and define, a general evaluator for
  |R v| expressions on the basis of an assignment function.

\begin{code}
eval :: Ring a => (v->a) -> R v -> a
eval var = e where
  e (Add   x  y) = e x + e y
  e (Zero)       = zero
  e (Neg x)      = neg (e x)
  e (Mul   x  y) = e x ⋅ e y
  e (One)        = one
  e (V v)        = var v
\end{code}
\item Specialise the evaluator to the two |Ring| instances defined in
  (\ref{item:inst}).  Take three ring expressions (of type |Ring
  String|), give the appropriate assignments and compute the results
  of evaluating, in each case, the three expressions.

\begin{code}
evalI :: (v->Integer) -> R v -> Integer
evalI = eval
evalF :: (v->k->Integer) -> R v -> k -> Integer
evalF = eval

e1, e2, e3 :: R String
e1 = V "x"
e2 = e1 + e1
e3 = neg e2 ⋅ e2

funI :: String -> Integer
funI "x" = 2
funI _   = 3

funF :: String -> (Integer -> Integer)
funF "x" = id
funF _   = const 0

testIs =             map (evalI funI) [e1, e2, e3]  == [2, 4,-16]
testFs = map apply1 (map (evalF funF) [e1, e2, e3]) == [1, 2, -4]

apply1 :: (Integer -> a) -> a
apply1 f = f 1
\end{code}
