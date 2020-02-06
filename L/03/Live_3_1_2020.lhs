Week & chapter 3: Types in mathematics

Learning outcomes

* Knowledge and understanding
** L3.1: organize areas of mathematics in DSL terms
** L3.1: explain main concepts of elementary real analysis
** L3: design and implement a DSL for derivatives

* Skills and abilities
** L3.1: develop adequate notation for mathematical concepts
** L3.2: perform calculational proofs

* Judgement and approach
** A1: discuss and compare different software implementations of mathematical concepts


On the blackboard:
* types for some examples, most importantly D
* derivative of a function f : Func = REAL -> REAL
* reminder of lim example from last week
* using lim to define |D|


\begin{code}
data SynF = Kvadrera | Id | Add SynF SynF | Mul SynF SynF | Con REAL
type SemF = REAL -> REAL
deriv :: SynF -> SynF
deriv (Con c)   = Con 0
deriv Kvadrera  = tvåGånger

tvåGånger :: SynF
tvåGånger = Mul (Con 2) Id

eval :: SynF -> SemF
eval (Con c)    = \x -> c
eval Id         = id      -- \x -> x
eval Kvadrera   = (^2)
eval (Add f g)  = oadd (eval f) (eval g)
eval (Mul f g)  = omul (eval f) (eval g)

oadd f g = \x -> f x + g x
omul f g = \x -> f x * g x

f1, f2, f3 :: SynF
f1 = Kvadrera          -- x²
f2 = Add (Con 2) f1    -- 2 + x²
f3 = Mul Id Id         -- x*x
\end{code}



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
type X = REAL
type Y = REAL
type Z = REAL
f :: X -> Y
a, b, x :: X
ff    :: (Y, Y, X) -> Z
integ :: a -> a -> (a->b) -> b
der   :: (a->b) -> (a->b)

toBeMinimized = integ a b (\x -> let y = f x
                                     y' = der f x
                                 in ff(y, y', x))

(f, x, y, y', ff, a, b, der, integ) = undefined

\end{code}
