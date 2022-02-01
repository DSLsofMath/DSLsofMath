
deriv :: "some function type" -> "some function type" 
deriv f = ???

+ Attempt 1: pattern matching on "semantic functions"?
+ Attempt 2: using lim and psi?
+ Attempt 3: build a DSL (called FunExp) {- actually works! -}

deriv :: (R->R) -> (R->R)    -- R is real numbers
deriv (f+g) = deriv f + deriv g  -- does not work because functions are "black box"

* Live coding
DSL for derivatives ={here} a DSL for 1-argument functions (or 1-var. epressions)

Example functions / expressions:

\begin{code}
sq x = x^2
tw x = 2*x
c2 x = 2

psi :: Fractional a =>   (a -> a) -> (a -> a -> a)
psi f x h = (f (x+h) - f x)/h

t1 = psi sq -- does not lead down the correct path
-- try with smaller and smaller h
-- λ> psi sq 1 0.1  =
--    2.100000000000002
--    2.0100000000000007
--    will approach 2 for a while, then diverge (to 0 actually)
--  due to limited precision of Double
-- => not where we want to go in this course (but sometimes necessary)
type REAL = Double

-- Syntax datatype for 1-argument function expressions
-- FunExp is a syntax (DSL) for 1-argument function expressions
data FunExp = Sq
            | Tw
            | C REAL
            | Add FunExp FunExp
            | Mul FunExp FunExp
            | X 
--            | Sin FunExp
--            | Cos FunExp
--            | Exp FunExp
  deriving Show

-- we can then compute the "syntactic" derivative
deriv :: FunExp -> FunExp
deriv Sq     = Tw
deriv Tw     = C 2
deriv X      = C 1
deriv (C c)  = C 0
deriv (Add e1 e2) = Add (deriv e1) (deriv e2)
deriv (Mul e1 e2) = Add (Mul (deriv e1) e2)
                        (Mul e1 (deriv e2))
--power :: FunExp -> Nat -> FunExp  -- useful exercise

-- and then at any point translate to the semantics ("real functions")
-- Semantic type   for 1-argument function expressions
type SemF = REAL -> REAL
eval :: FunExp -> SemF
eval X            = id
eval Sq           = (^2)
eval Tw           = (2*)   -- obsolete?
eval (C c)        = const c     -- :: REAL -> REAL
eval (Add e1 e2)  = oplus (eval e1) (eval e2)
eval (Mul e1 e2)  = omul  (eval e1) (eval e2)
  -- eval e1 :: REAL -> REAL
  -- eval e2 :: REAL -> REAL
  -- oplus :: (REAL -> REAL) -> (REAL -> REAL) -> (REAL -> REAL)


oplus :: (REAL -> REAL) -> (REAL -> REAL) -> (REAL -> REAL)
oplus f g = \x -> f x + g x

omul :: (REAL -> REAL) -> (REAL -> REAL) -> (REAL -> REAL)
omul f g = \x -> f x * g x


-- Specification:   D (eval e) = eval (deriv e)


e1, e2, e3, e4 :: FunExp
e1 = Add Sq Tw -- \x ->  x^2 + 2*x
e2 = Add X X
e3 = Mul X X
e4 = Mul e3 e3
\end{code}

Each DSL needs
-- type of syntax trees:     Syn
-- type of semantic values:         Sem
-- a function        eval :: Syn -> Sem


\begin{code}
simplify :: FunExp -> FunExp
simplify = error "Exercise!"
  -- 1*e -> e
  -- 0*e -> 0
  -- 0+e -> e
  -- etc.
\end{code}

