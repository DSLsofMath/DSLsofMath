deriv :: (X->Y) -> (X->Y)
deriv f = ???

+ Attempt 1: pattern matching on "semantic functions"?
+ Attempt 2: using lim and psi?
+ Attempt 3: build a DSL (called FunExp)

* Live coding
DSL for derivatives ={here} a DSL for 1-argument functions (or 1-var. epressions)

Example functions / expressions:

Vi kan inte "plocka isär" en allmän funktion (i Haskell).

  derivSem (f .+ g) = derivSem f .+ derivSem g
  derivSem (c .* f) = c .* derivSem f

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

exempel = map (t1 1) (iterate (/10) 1)

derivSem :: (X->Y) -> (X->Y)
derivSem f x = lim 0 (psi f x)

lim :: X -> (X->Y) -> Y
lim a g = error "TODO"

type REAL = Double

-- Syntax datatype for 1-argument function expressions 
type Nat = Int

data SynF = Sq | Tw | Con REAL | X | Pow SynF REAL
          | Add SynF SynF
          | Mul SynF SynF
  deriving Show

s1 = X
s2 = Pow X 3
s3 = Pow Sq 3
s4 = Add s1 s2
s5 = Mul X X

-- λ> deriv s4
-- ds4 = Add (Con 1.0) (Mul (Mul (Con 3.0) (Pow X 2.0)) (Con 1.0))
ds4 = Add (Con 1.0) (Mul (Con 3.0) (Pow X 2.0))
ds5 = Add X X
-- dds5 = Add (Add (Mul (Con 0.0) X) (Mul (Con 1.0) (Con 1.0))) (Add (Mul (Con 1.0) (Con 1.0)) (Mul X (Con 0.0)))
dds5 = Add (Add (Mul (Con 0.0) X)
                (Mul (Con 1.0) (Con 1.0)))

           (Add (Mul (Con 1.0) (Con 1.0))
                (Mul X (Con 0.0)))




-- Semantic type   for 1-argument function expressions
type SemF = REAL -> REAL
eval :: SynF -> SemF
eval Sq          = (^2)
eval Tw          = (2*)
eval (Con c)     = const c
eval X           = \x -> x   -- id
eval (Pow e n)   = \x -> (eval e x)**n
eval (Add e1 e2) = eval e1 .+ eval e2
eval (Mul e1 e2) = eval e1 .* eval e2

(.+) :: SemF -> SemF -> SemF
f .+ g = \x -> f x  +  g x

(.*) :: SemF -> SemF -> SemF
f .* g = \x -> f x  *  g x

deriv :: SynF -> SynF
deriv Sq         = Tw
deriv Tw         = Con 2
deriv (Con c)    = Con 0
deriv X          = Con 1
deriv (Add e1 e2)= Add (deriv e1) (deriv e2)
deriv (Pow e n)  = Mul (Mul (Con n) (Pow e (n-1)))
                       (deriv e)
                 -- n*e^(n-1) * deriv e
deriv (Mul e1 e2)= Add (Mul (deriv e1) e2)
                       (Mul e1 (deriv e2))
  -- 
e1, e2, e3, e4 :: SynF
(e1, e2, e3, e4) = error "TODO: examples"
\end{code}

Each DSL needs
-- type of syntax trees:     Syn
-- type of semantic values:         Sem
-- a function        eval :: Syn -> Sem


\begin{code}
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
\end{code}

