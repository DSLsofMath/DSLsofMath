\subsection{A case study: complex numbers}

We will start by an analytic reading of the introduction of complex
numbers in \cite{adams2010calculus}.
%
We choose a simple domain to allow the reader to concentrate on the
essential elements of our approach without the distraction of
potentially unfamiliar mathematical concepts.
%
For this section, we bracket our previous knowledge and approach the
text as we would a completely new domain, even if that leads to a
somewhat exaggerated attention to detail.

Adams and Essex introduce complex numbers in Appendix 1.
%
The section \emph{Definition of Complex Numbers} begins with:

\begin{quote}
  We begin by defining the symbol |i|, called \textbf{the imaginary
    unit}, to have the property

<      square i = -1

  Thus, we could also call |i| the square root of |-1| and denote it
  |sqrt (-1)|.
%
  Of course, |i| is not a real number; no real number has a negative
  square.
\end{quote}


----------------------------------------------------------------
% TODO: texify the below

-- Start with small language for arithmetic expressions (known to
-- local students from the Intro FP course) + evaluator. Still with the
-- aim to explain syntax, semantics and Haskell.

Arithmetical expressions

> module L01 where

> data Expr
>   =  Num Int
>   |  Add Expr Expr
>   |  Mul Expr Expr
>   deriving (Show, Eq)

> evExpr  (Num  n)       =  n
> evExpr  (Add  e1  e2)  =  evExpr e1  +  evExpr e2
> evExpr  (Mul  e1  e2)  =  evExpr e1  *  evExpr e2

Other evaluators are possible (to Bool, to String, etc.).

Arithmetical atomic propositions

> data ArAt
>   =  ArEq  Expr Expr
>   |  ArLeq Expr Expr
>               deriving (Show, Eq)

> evArAt (ArEq   e1  e2)  =  evExpr e1  ==  evExpr e2
> evArAt (ArLeq  e1  e2)  =  evExpr e1  <=  evExpr e2

Other semantics give different results.

Propositional logic

> data Prop atom  =  Atom  atom
>                 |  Not   (Prop atom)
>                 |  And   (Prop atom) (Prop atom)
>                 |  Or    (Prop atom) (Prop atom)
>                 |  Impl  (Prop atom) (Prop atom)
>                 deriving Show

> evalP evAt   (Atom a)      =  evAt a
> evalP evAt   (Not p)       =  not (evalP evAt p)
> evalP evAt   (And  p1 p2)  =  evalP evAt p1  &&  evalP evAt p2
> evalP evAt   (Or   p1 p2)  =  evalP evAt p1  ||  evalP evAt p2
> evalP evAt   (Impl p1 p2)  =  not (evalP evAt p1) || evalP evAt p2

Tautology:  propositions that evaluate to true for any evaluation of the atoms.

Example:

    evalP evAt (Or p (Not p))
=
    evalP evAt p || evalP evAt (Not p)
=
    evalP evAt p || not (evalP evAt p)
=
    True

Arithmetical propositional logic

> type ArProp  =  Prop ArAt
> evalArP      =  evalP evArAt

Example:

> test0      =  evalArP (Impl (Atom (ArEq (Num 3) (Num 4))) (Atom (ArLeq (Num 5) (Num 1))))
> test1      =  evalArP (Impl (Atom (ArEq (Num 3) (Num 3))) (Atom (ArLeq (Num 6) (Num 5))))
