\section{A parameterised type and some complex number operations on it}
\label{app:CSem}

\begin{code}
module DSLsofMath.CSem where

newtype ComplexSem r = CS (r , r)    deriving Eq
\end{code}

\paragraph{Lifting operations to a parameterised type}

When we define addition on complex numbers (represented as pairs of
real and imaginary components) we can do that for any underlying type
|r| which supports addition.

\begin{code}
type CS = ComplexSem -- for shorter type expressions below
liftCS ::  (    r  ->     r  ->     r  ) ->
           (CS  r  -> CS  r  -> CS  r  )
liftCS (+) (CS (x, y)) (CS (x', y')) = CS (x+x', y+y')
\end{code}
Note that |liftCS| takes |(+)| as its first parameter and uses it
twice on the RHS.

%TODO: Perhaps also add as an exercise to use Num to make the parameter implicit. But this should perhaps be placed in a later chapter after Num has been properly introduced.

\begin{code}
re :: ComplexSem r      ->  r
re z @ (CS (x , y))   =   x

im :: ComplexSem r      ->  r
im z @ (CS (x , y))   =   y

(+.) :: Num r =>  ComplexSem r -> ComplexSem r -> ComplexSem r
(CS (a , b)) +. (CS (x , y))  =  CS ((a + x) , (b + y))

(*.) :: Num r =>  ComplexSem r -> ComplexSem r -> ComplexSem r
CS (ar, ai) *. CS (br, bi) = CS (ar*br - ai*bi, ar*bi + ai*br)

instance Show r => Show (ComplexSem r) where
  show = showCS

showCS :: Show r => ComplexSem r -> String
showCS (CS (x, y)) = show x ++ " + " ++ show y ++ "i"
\end{code}

A corresponding syntax type: the second parameter |r| makes is
possible to express ``complex numbers over'' different base types
(like |Double|, |Float|, |Integer|, etc.).

\begin{code}
data ComplexSy v r  =  Var v
                    |  FromCart r r
                    |  ComplexSy v r  :++  ComplexSy v r
                    |  ComplexSy v r  :**  ComplexSy v r
\end{code}
