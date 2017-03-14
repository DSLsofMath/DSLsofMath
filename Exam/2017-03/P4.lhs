> module P4 where

[15pts] Recall the type of expressions

\begin{code}
data FunExp  =  Const Rational
             |  Id
             |  FunExp  :+:  FunExp
             |  FunExp  :*:  FunExp
             |  FunExp  :/:  FunExp
             |  Exp  FunExp
             |  Sin  FunExp
             |  Cos  FunExp
             -- and so on
             deriving Show
\end{code}

and consider the function

\begin{code}
f :: Double -> Double
f x = exp (sin x) + x
\end{code}


* Find an expression |e| such that |eval e == f| and show this
  using equational reasoning.

Here is a calculational version of the solution:

<   f
< == (+) instance for functions
<   (exp . sin . id) + id
< == exp instance for functions
<   exp (sin . id) + id
< == sin instance for functions
<   exp (sin id) + id
< == eval for Id
<   exp (sin (eval Id)) + (eval Id)
< == eval for Sin
<   exp (eval (Sin Id)) + (eval Id)
< == eval for Exp
<   eval (Exp (Sin Id)) + (eval Id)
< == eval for (:+:)
<   eval (Exp (Sin Id) :+: Id)

Thus |e = Exp (Sin Id) :+: Id| gives |eval e == f|


* Implement a function |deriv2| such that, for any |f : Fractional
  a => a -> a| constructed with the grammar of |FunExp| and any |x| in
  the domain of |f|, we have that |deriv2 f x| computes the second
  derivative of |f| at |x|.
  Use the function |derive :: FunExp -> FunExp| from the lectures
  (|eval (derive e)| is the derivative of |eval e|).
  What instance declarations do you need?

  The type of |deriv2 f| should be |Fractional a => a -> a|.

\begin{code}
deriv2 f x = eval (derive (derive (f Id))) x
\end{code}

The instance declarations needed are:

\begin{code}
instance Num FunExp            -- ...
instance Fractional FunExp     -- ...
instance Floating FunExp       -- ...
instance Num a => Num (x -> a) -- ...
-- etc.
\end{code}

Some type signatures of functions used above:

\begin{code}
eval :: Fractional a => FunExp -> (a -> a)
eval = undefined

derive :: FunExp -> FunExp
derive = undefined
\end{code}

TODO: add proper instances and definitions in a separate module and
import it.
