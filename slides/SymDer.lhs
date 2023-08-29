Domain-Specific Languages of Mathematics course, Chalmers and UGOT
(from Week & chapter 3: Types in mathematics)

1. Reminder about the DSL definition.
2. Define a DSL for functions
3. Define a "syntactic derivative" function
    deriv :: SymF -> SymF

Towards a DSL for derivatives ={here} a DSL for 1-argument functions (or 1-var. epressions)

Motivation:

deriv :: (X->Y) -> (X->Y)
deriv f = ???

+ Attempt 1: pattern matching on "semantic functions"?
+ Attempt 2: using lim and psi? (from the LimFun slides)
+ Attempt 3: build a DSL (called SymF)

Example functions / expressions:

We cannot "tease apart" general function (in Haskell).

  derivSem (f .+ g) = derivSem f .+ derivSem g
  derivSem (c .* f) = c .* derivSem f

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
t1 = psi sq -- leder inte till rätt väg

exempel = map (t1 1) (iterate (/10) 1)

derivSem :: (X->Y) -> (X->Y)
derivSem f x = lim 0 (psi f x)

lim :: X -> (X->Y) -> Y
lim a g = error "TODO" -- cannot be implemented 
\end{code}


Each DSL needs
-- a type of syntax trees:     Syn
-- a type of semantic values:         Sem
-- a function          eval :: Syn -> Sem

Syntax datatype for 1-argument function expressions:

\begin{code}
type Nat = Int

data SynF 
  deriving Show

-- Semantic type   for 1-argument function expressions
type SemF = REAL -> REAL
eval :: SynF -> SemF
eval = error "TODO"

(.+) :: SemF -> SemF -> SemF
(.+) = error "TODO"

(.*) :: SemF -> SemF -> SemF
(.*) = error "TODO"

e1, e2, e3, e4 :: SynF
(e1, e2, e3, e4) = error "TODO: examples"
\end{code}

----------------
  3. Define a "syntactic derivative" function
    deriv :: FunExp -> FunExp

Specification:   D (eval e) = eval (deriv e)
   (where D is not implementable in Haskell)

\begin{code}
deriv :: SynF -> SynF
deriv = error "TODO"
\end{code}




Just the start of a symbolic "simplifier" for expressions.
\begin{spec}
simplify :: SynF -> SynF
simplify (Add e (Con 0)) = e
simplify (Add (Con 0) e) = e
simplify (Mul (Con 0) e) = Con 0
simplify (Mul (Con 1) e) = e
simplify (Mul e (Con 0)) = Con 0
simplify (Mul e (Con 1)) = e
simplify (Add e1 e2) = Add (simplify e1) (simplify e2)
simplify e = e
  -- 1*e -> e
  -- 0*e -> 0
  -- 0+e -> e
  -- etc.
\end{spec}

