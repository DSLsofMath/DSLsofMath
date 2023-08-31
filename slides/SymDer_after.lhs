Domain-Specific Languages of Mathematics course, Chalmers and UGOT
(from Week & chapter 3: Types in mathematics)

0. Motivating the need for working with syntax
1. Reminder about the DSL definition.
2. Define a DSL for functions
3. Define a "syntactic derivative" function
    deriv :: SymF -> SymF  -- can (and will) be implemented
    D     :: SemF -> SemF  -- cannot be implemented
    type SemF = REAL -> REAL

Towards a DSL for derivatives ={here} a DSL for 1-argument functions
(or one-variable epressions)

Motivation:

D :: (X->Y) -> (X->Y)
D f = ???

+ Attempt 1: pattern matching on "semantic functions"? Does not work
+ Attempt 2: using lim and psi? (from the LimFun slides)
+ Attempt 3: build a DSL (called SymF)

Example functions / expressions:

\begin{code}
type REAL = Double
sq x = x^2
tw x = 2*x
c2 x = 2
type X = REAL
type Y = REAL
type H = REAL -- except for zero

psi :: (X->Y) -> X -> H -> Y
psi f x h = (f (x+h) - f x)/h

t1 :: X -> H -> Y
t1 = psi sq -- leads us astray ...

example = (  mapM_ print
          .  take 20
          .  map (t1 3)
          .  iterate (/10)
          ) 1

derivSem :: (X->Y) -> (X->Y)
derivSem f x = lim 0 (psi f x)

lim :: X -> (X->Y) -> Y
lim a g = error "TODO" -- cannot be implemented (due to rounding error)
\end{code}


Each DSL needs
-- a type of syntax trees:     Syn
-- a type of semantic values:         Sem
-- a function          eval :: Syn -> Sem

Syntax datatype for 1-argument function expressions:
\begin{code}
data SynF = X
          | C REAL
          | Add SynF SynF
          | Mul SynF SynF
  deriving Show

-- Semantic type   for 1-argument function expressions
type SemF = REAL -> REAL
eval :: SynF -> SemF
eval X  = \x -> x   -- id
eval (C r) = \x -> r
eval (Add fe ge) = eval fe  .+   eval ge
eval (Mul fe ge) = eval fe  .*   eval ge

-- Add :: SynF -> SynF -> SynF
(.+)   :: SemF -> SemF -> SemF
(.*)   :: SemF -> SemF -> SemF
f .+ g  =  \x -> f x  +  g x -- called "oplus" in the slides
f .* g  =  \x -> f x  *  g x

e1, e2, e3, e4 :: SynF
e1 = Add X X   -- the same as Tw
e2 = Mul X X   -- the same as Sq
e3 = Add (Mul e1 e2) X
e4 = Mul e3 e3
\end{code}

----------------
  3. Define a "syntactic derivative" function
    deriv :: FunExp -> FunExp

Specification:   D (eval e) = eval (deriv e)
   (where D is not implementable in Haskell)

\begin{code}
deriv :: SynF -> SynF
deriv X     = C 1
deriv (C r) = C 0
deriv (Add fe ge) = Add (deriv fe) (deriv ge)
deriv (Mul fe ge) = Add (Mul (deriv fe) ge) (Mul fe (deriv ge))

simplify :: SynF -> SynF
simplify = error "homework" -- difficult!
\end{code}

      D (f .* g) =        (D f .* g)   .+    (f .* D g)
 deriv (Mul f g) = Add (Mul (deriv f) g) (Mul f (deriv g))


  Add (Mul (C 1.0) X) (Mul X (C 1.0))
= Add (Mul (C 1) X) (Mul (C 1) X)
= Mul (C 2) X
















-------------
Mini-course conclusions:
We have talked about 
+ Domain-Specific Languages of Mathematics (DSLs of Math)
+ a short introduction to Functional Programming in Haskell
+ formalising / implementing "text book domains"
  + Complex numbers  (syntax, semantics, eval)
  + Simple functions (syntax, semantics, eval, symbolic derivative (deriv))

(much) more material is available online:
+ [1] A playlist with lectures from the 2022 full course instance
+ [2] A source-code repository with all the examples, exams, etc.
+ [3] The PDF of the course book (can also be bought on paper)

[1] https://www.youtube.com/playlist?list=PLf5C73P7ab-5sdvsqCjnF8iaYOtXMRNaZ
[2] https://github.com/DSLsofMath/DSLsofMath
[3] https://github.com/DSLsofMath/DSLsofMath/blob/master/L/snapshots/DSLsofMathBook_snapshot_2022-12-26.pdf
-------------




























Just the start of a symbolic "simplifier" for expressions.
\begin{spec}
simplify :: SynF -> SynF
simplify (Add e (C 0)) = simplify e
simplify (Add (C 0) e) = simplify e
simplify (Mul (C 0) e) = C 0
simplify (Mul (C 1) e) = simplify e
simplify (Mul e (C 0)) = C 0
simplify (Mul e (C 1)) = simplify e
simplify (Add e1 e2) = Add (simplify e1) (simplify e2)
simplify e = e
  -- 1*e -> e
  -- 0*e -> 0
  -- 0+e -> e
  -- etc.
\end{spec}
          1         2         3         4         5         6         7         8
012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
(The column numbers are just here to help choose the font size when presenting.)

