On the blackboard:
* types for some examples, most importantly D
* derivative of a function f : Func = REAL -> REAL
* reminder of lim example from Thu 2018-01-25
* using lim to define |D|

Lecture 3.1 Live "coding" (typing)

Exam question 2 from 2016-03:

From exam 2016-03-16:

    Consider the following text from Mac Lane's "Mathematics, Form and
    Function" (page 182):

   \begin{quote}
     In these cases one tries to find not the values of |x| which
     make a given function |y = f(x)| a minimum, but the values of a given
     function |f(x)| which make a given quantity a minimum.  Typically,
     that quantity is usually measured by an integral whose integrand is
     some expression |F| involving both |x|, values of the function |y =
     f(x)| at interest and the values of its derivatives - say an
     integral
   \end{quote}

    $$∫_a^b F(y, y', x)dx,      where y = f(x).$$

    Give the types of the variables involved (|x|, |y|, |y'|, |f|, |F|,
    |a|, |b|) and the type of the four-argument integration operator:

    $$∫_.^. \cdot d\cdot$$

\begin{code}
type REAL = Double
newtype X = MyX REAL  -- some subset or interval of REAL, should include the interval [a,b]
newtype Y = MyY REAL
data Z

f :: X -> Y
y  :: X -> Y   -- but there are other choices, like |Y|
y' :: X -> Y
y' = deriv y
deriv :: (X -> Y) -> (X -> Y)

-- ∫
a, b :: X
integ :: X -> X -> (X -> Z) -> Z
ff :: (X -> Y, X -> Y, X) -> Z
test = integ a b (\x-> ff(f, deriv f, x))

-- ff is Haskell for F
(x, y, deriv, integ, f, ff, a, b) = undefined

\end{code}

Here is another variant:
\begin{code}
ff2 :: Y -> Y -> X -> Z
ff2 = undefined
test2 :: Z
test2 = integ a b expr
  where  expr :: X -> Z
         expr x = ff2 y y' x
           where  y   = f x        :: Y
                  y'  = deriv f x  :: Y
\end{code}
