This file is from the "live coding" session at the end of L1.2
(study week 1, lecture 2, of the DSLsofMath course, 2019).

\begin{code}
{-# LANGUAGE GADTs #-}
module Live2 where
type REAL = Double
\end{code}

A datatype for "semantic complex numbers" as defined on page A.2 in
the book [Adams and Essex, Calculus, 2010].

\begin{code}
data CB where -- semantics
  C :: REAL -> REAL -> CB
 deriving Show
\end{code}

\begin{code}
data CC where  -- syntax
  CPlus  :: CC -> CC -> CC
  CTimes :: CC -> CC -> CC
  I      :: CC
  ToCC   :: REAL -> CC

ex1, ex2, ex3, ex4 :: CC
ex1 = I
ex2 = ToCC 3
ex3 = CPlus ex1 ex2
ex4 = CTimes ex3 ex3
\end{code}

Translation from syntax to semantics:

\begin{code}
eval :: CC -> CB
eval I            = C 0 1
eval (ToCC r)     = C r 0
eval (CPlus x y)  = cplus  (eval x) (eval y)
eval (CTimes x y) = ctimes (eval x) (eval y)
\end{code}

Semantic version of the constructors:
\begin{code}
cplus :: CB -> CB -> CB
cplus (C re1 im1) (C re2 im2) = C (re1+re2) (im1+im2)

ctimes :: CB -> CB -> CB
ctimes (C re1 im1) (C re2 im2) = C  (re1*re2 - im1*im2)
                                    (re1*im2 + re2*im1)
\end{code}

Showing values: another semantics of complex number expressions (as
Strings).

\begin{code}
instance Show CC where
  show = showCC

showCC :: CC -> String
showCC I = "i"
showCC (ToCC r) = show r
showCC (CPlus x y) = "(" ++ showCC x ++ "+" ++ showCC y ++ ")"
showCC (CTimes x y) = "(" ++ showCC x ++ "*" ++ showCC y ++ ")"
\end{code}

----------------

Examples of simpler datatypes:

\begin{code}
data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat
\end{code}
This faithfully encodes all natural numbers, but for big naturals is really Succs(;-).

\begin{code}
data ZZ where
  Pos :: Nat -> ZZ
  Neg :: Nat -> ZZ   -- adding the minus sign in front
\end{code}

What about Pos Zero and Neg Zero? Two representation for the "same" value.

Possible alternative to avoid that:
\begin{code}
data ZZ' where
  Pos' :: Nat -> ZZ'
  Neg' :: Nat -> ZZ'   -- Neg n means (-(n+1))  -- Neg Zero means -1
\end{code}

\begin{code}
data Rat where
  Q :: ZZ -> ZZ -> Rat
\end{code}

Note that Q 6 3 == Q 4 2 == Q 2 1 ... -- many representations for each value

Also: Q 1 0 is a Rat but should not be a rational!

Suggestion: add a type PosNat of just _positive_ naturals and use that as the second argument to Q.
