
deriv :: (X->Y) -> (X->Y)
deriv f = ???

+ Attempt 1: pattern matching on "semantic functions"?
+ Attempt 2: using lim and psi?
+ Attempt 3: build a DSL (called FunExp)

* Live coding
DSL for derivatives ={here} a DSL for 1-argument functions (or 1-var. epressions)

Example functions / expressions:

\begin{code}
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

derivSem :: (X->Y) -> (X->Y)
derivSem f x = lim 0 (psi f x)

lim :: H -> (H->Y) -> Y
lim h g = error "TODO"

type REAL = Double

-- Syntax datatype for 1-argument function expressions
data SynF -- TODO

-- Semantic type   for 1-argument function expressions
type SemF = REAL -> REAL
eval :: SynF -> SemF
eval = error "TODO: eval"

deriv :: SynF -> SynF
deriv = error "TODO: deriv"

e1, e2, e3, e4 :: SynF
(e1, e2, e3, e4) = error "TODO: examples"
\end{code}

Each DSL needs
-- type of syntax trees:     Syn
-- type of semantic values:         Sem
-- a function        eval :: Syn -> Sem


\begin{code}
simplify :: SynF -> SynF
simplify = error "Exercise!"
  -- 1*e -> e
  -- 0*e -> 0
  -- 0+e -> e
  -- etc.
\end{code}

