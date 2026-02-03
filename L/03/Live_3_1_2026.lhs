\begin{code}
module Live_3_1_2026 where
type REAL = Double
\end{code}
No pattern-matching available on "abstract functions" in Haskell

deriv :: (X->Y) -> (X->Y)
deriv (f+g) = deriv f + deriv g
deriv (f*g) = f*deriv g   + deriv f * g
...


+ Attempt 1: pattern matching on "semantic functions"?
  derivSem (f .+ g) = derivSem f .+ derivSem g
  derivSem (c <* f) = c <* derivSem f
+ Attempt 2: using lim and psi?
  (how to pick h?)
+ Attempt 3: build a DSL SynF (later called FunExp)

* Live coding
DSL for derivatives ={here} a DSL for 1-argument functions (or 1-var. epressions)


** Attempt 2:

Example functions / expressions:

\begin{code}
sq x = x^2
tw x = 2*x
c2 x = 2
type X = REAL
type Y = REAL
type H = REAL -- except for zero
\end{code}

Definition from the book / slides / blackboard:
\begin{code}
psi :: (X->Y) -> X -> H -> Y
psi f x h = (f (x+h) - f x)/h

derivSem :: (X->Y) -> (X->Y)
derivSem f x = lim 0 (psi f x)

lim :: X -> (X->Y) -> Y
lim a g = error "TODO" -- cannot be implemented in general
\end{code}

What goes wrong with attempt 2?
\begin{code}
t1 :: X -> H -> Y
t1 = psi sq

smallh :: [H]
smallh = iterate (/10) 1
example :: [Y]
example = map (t1 1) smallh
showExample :: IO ()
showExample = mapM_ print (take 20 example)
\end{code}

Attempt 3: A Domain-Specific Language approach

Reminder: each DSL needs
+ a type of syntax trees:     Syn
+ a type of semantic values:         Sem
+ a function          eval :: Syn -> Sem

We can then start crafting the DSL step by step.
Syntax datatype for 1-argument function expressions
\begin{code}
data SynF = X | C REAL | Add SynF SynF | Mul SynF SynF deriving Show

-- Semantic type   for 1-argument function expressions
type SemF = REAL -> REAL
eval :: SynF -> SemF
eval X         = id
eval (C c)     = const c
eval (Add f g) = (eval f) .+ (eval g)
eval (Mul f g) = (eval f) .* (eval g)

deriv :: SynF -> SynF
deriv X          = C 1
deriv (Mul f g)  = add (Mul f (deriv g)) (Mul (deriv f) g)
deriv (Add f g)  = add (deriv f) (deriv g)
deriv (C c)      = C 0

add = Add
-- or add = addSimp
addSimp :: SynF -> SynF -> SynF
addSimp x y = simplify (Add x y)
\end{code}

c2? = Add (Add (Mul X (C 0.0))
               (Mul (C 1.0) (C 1.0)))
          (Add (Mul (C 1.0) (C 1.0))
               (Mul (C 0.0) X))
c2? = C 2













Spec.
  Forall e. eval e == eval (simplify e)

Partial implementation of a simplifier
\begin{code}
simplify :: SynF -> SynF
simplify (Add (C a) (C b)) = C (a + b)
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

-- Helpers

(.+) :: SemF -> SemF -> SemF
f .+ g = \x -> f x  +  g x

(.*) :: SemF -> SemF -> SemF
f .* g = \x -> f x  *  g x
\end{code}
