\section{Week 8: Laplace}

\subsection{The Exponential Function}

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DSLsofMath.W08 where
import DSLsofMath.W05
import DSLsofMath.W06
\end{code}

One of the classical analysis textbooks, Rudin's \cite{rudin1987real}
starts with a prologue on the exponential function.
%
The first sentence is

\begin{quote}
This is undoubtedly the most important function in mathematics.
\end{quote}

Rudin goes on

\begin{quote}
It is defined, for every complex number |z|, by the formula
\begin{spec}
         exp z  =  Sigma (z^n) / n!
\end{spec}
\end{quote}


We have defined the exponential function as the function represented
by the power series

\begin{spec}
expx :: Fractional a => PowerSeries a
expx = integ expx 1
\end{spec}

and approximated by

\begin{spec}
expf :: Fractional a => a -> a
expf = eval 100 expx
\end{spec}

It is easy to see, using the definition of |integ| that the power
series |expx| is, indeed

\begin{spec}
expx = [1, 1/2, 1/(2 * 3), ..., 1 / (2 * 3 * ... * n), ..]
\end{spec}

We can compute the exponential for complex values if we can give an
instance of |Fractional| for complex numbers.
%
We could use the datatype |Data.Complex| from the Haskell standard
library, but we prefer to roll our own in order to remind the basic
operations on complex numbers.

As we saw in week 1, complex values can be represented as pairs of
real values.

\begin{code}
newtype Complex r = C (r , r)    deriving (Eq, Show)

i :: Num a => Complex a
i = C (0, 1)
\end{code}

Now, we have, for example

\begin{code}
ex1 :: Fractional a => Complex a
ex1 = expf i
\end{code}

We have |ex1 = C (0.5403023058681398,0.8414709848078965)|.
%
Note that

\begin{spec}
cosf 1  =  0.5403023058681398
sinf 1  =  0.8414709848078965
\end{spec}

and therefore |expf i == C (cosf 1, sinf 1)|.
%
Coincidence?

Instead of evaluating the sum of the terms |an * z^n|, let us instead
collect the terms in a series:

\begin{code}
terms as z = terms1 as z 0  where
  terms1 (Cons a as) z n  =  Cons (a * z^n) (terms1 as z (n+1))
\end{code}

We obtain

\begin{code}
ex2   ::  Fractional a => PowerSeries (Complex a)
ex2    =  takePoly 10 (terms expx i)
\end{code}

\begin{spec}
ex2 =  [  C (1.0,                     0.0), {-"\qquad"-} C (0.0,  1.0                     )
       ,  C (-0.5,                    0.0), {-"\qquad"-} C (0.0,  -0.16666666666666666    )
       ,  C (4.1666666666666664e-2,   0.0), {-"\qquad"-} C (0.0,  8.333333333333333e-3    )
       ,  C (-1.3888888888888887e-3,  0.0), {-"\qquad"-} C (0.0,  -1.9841269841269839e-4  )
       ,  C (2.4801587301587298e-5,   0.0), {-"\qquad"-} C (0.0,  2.7557319223985884e-6   )
       ]
\end{spec}

We can see that the real part of this series is the same as

\begin{code}
ex2R = takePoly 10 (terms cosx 1)
\end{code}

and the imaginary part is the same as

\begin{code}
ex2I = takePoly 10 (terms sinx 1)
\end{code}

(within approx 20 decimals).
%
But the terms of a series evaluated at |1| are the coefficients of the
series.
%
Therefore, the coefficients of |cosx| are

\begin{spec}
[1, 0, -1/2!, 0, 1/4!, 0, -1/6!, ...]
\end{spec}

i.e.

\begin{spec}
a (2 * n)  = (-1)^n / (2 * n)!
a (2*n+1)  = 0
\end{spec}

and the terms of |sinx| are

\begin{spec}
[0, 1, 0, -1/3!, 0, 1/5!, 0, -1/7!, ...]
\end{spec}

i.e.

\begin{spec}
a (2 * n)  = 0
a (2*n+1)  = (-1)^n / (2*n+1)!
\end{spec}

This can be proven from the definitions of |cosx| and |sinx|.
%
From this we obtain \emph{Euler's formula}:

\begin{spec}
exp (i*x) = cos x + i*sin x
\end{spec}

One thing which comes out of Euler's formula is the fact that the
exponential is a \emph{periodic function}.
%
A function |f : A -> B| is said to be periodic if there exists |T ∈ A|
such that

\begin{spec}
f a = f (a + T)  --  |∀ a ∈ A|
\end{spec}

(therefore, for this definition to make sense, we need addition on
|A|; in fact we normally assume at least group structure, i.e.,
addition and subtraction).

Since |sin| and |cos| are periodic, with period |2 * pi|, we have,
using the standard notation |a+i*b| for |C (a, b)|

\begin{spec}
  e^(a + i * b + i * 2*pi)
=
  e^(a + i * (b + 2*pi))
=
  e^a * e^(i * (b + 2 * pi))
=
  e^a * (cos (b + 2 * pi) + i * sin (b + 2 * pi))
=
  e^a * (cos b + i * sin b)
=
  e^a * e^(i*b)
=
  e^(a + i * b)
\end{spec}

\subsection{Associated code}

TODO: Perhaps import from W01

\begin{code}
instance Num r => Num (Complex r) where
  (+) = addC
  (*) = mulC
  fromInteger n = C (fromInteger n, 0)
--  abs = absC  -- requires Floating r as context

addC :: Num r =>  Complex r -> Complex r -> Complex r
addC (C (a , b)) (C (x , y))  =  C ((a + x) , (b + y))

mulC :: Num r =>  Complex r -> Complex r -> Complex r
mulC (C (ar, ai)) (C (br, bi))  =  C (ar*br - ai*bi, ar*bi + ai*br)

modulusSquaredC :: Num r => Complex r -> r
modulusSquaredC (C (x, y)) = x^2 + y^2

absC :: Floating r => Complex r -> Complex r
absC c = C (sqrt (modulusSquaredC c), 0)

scale :: Num r => r -> Complex r -> Complex r
scale a (C (x, y)) = C (a * x, a * y)

conj :: Num r => Complex r -> Complex r
conj (C (x, x'))   = C (x, -x')

instance Fractional r => Fractional (Complex r) where
  (/) = divC
  fromRational r = C (fromRational r, 0)

divC :: Fractional a => Complex a -> Complex a -> Complex a
divC x y = scale (1/modSq) (x * conj y)
  where  modSq  =  modulusSquaredC y
\end{code}
