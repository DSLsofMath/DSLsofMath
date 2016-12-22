> module L01 where

<!--
Start with small language for arithmetic expressions (known to
local students from the Intro FP course) + evaluator. Still with the
aim to explain syntax, semantics and Haskell.
-->

Arithmetical expressions

> data Expr
>   =  Num Int
>   |  Add Expr Expr
>   |  Mul Expr Expr
>   deriving (Show, Eq)

> evExpr  (Num  n)       =  n
> evExpr  (Add  e1  e2)  =  evExpr e1  +  evExpr e2
> evExpr  (Mul  e1  e2)  =  evExpr e1  *  evExpr e2

Other evaluators are possible (to `Bool`, to `String`, etc.).

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

<     evalP evAt (Or p (Not p))
< =
<     evalP evAt p || evalP evAt (Not p)
< =
<     evalP evAt p || not (evalP evAt p)
< =
<     True

Arithmetical propositional logic

> type ArProp  =  Prop ArAt
> evalArP      =  evalP evArAt

Example:

> test0      =  evalArP (Impl (Atom (ArEq (Num 3) (Num 4))) (Atom (ArLeq (Num 5) (Num 1))))
> test1      =  evalArP (Impl (Atom (ArEq (Num 3) (Num 3))) (Atom (ArLeq (Num 6) (Num 5))))
