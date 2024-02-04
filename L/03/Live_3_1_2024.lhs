\begin{code}
module Live_3_1_2024 where
type REAL = Double
\end{code}
deriv :: (X->Y) -> (X->Y)
deriv f = ???

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
\begin{code}
-- Syntax datatype for 1-argument function expressions
data SynF = -- Sq
            --  | Tw
            Const REAL
          | X  
          | Add SynF SynF
          | Mul SynF SynF 
--          | Exp1        -- (\x -> exp x)
--          | Exp2 SynF   -- (\x -> exp (f x))
--          | Power SynF Integer  -- f^n
          -- TODO
          -- Polynom
  deriving Show

e1' = Mul X X
-- test1 = eval e1 === eval e1'  -- ? (===) ?

e1, e2, e3, e4 :: SynF
(e1, e2, e3, e4) = error "TODO: examples"

-- Semantic type   for 1-argument function expressions
type SemF = REAL -> REAL
eval :: SynF -> SemF
eval (Const c) = const c
-- eval Sq          = (^2)
-- eval Tw          = (2*)

(.+) :: SemF -> SemF -> SemF
f .+ g = \x -> f x  +  g x

(.*) :: SemF -> SemF -> SemF
f .* g = \x -> f x  *  g x

deriv :: SynF -> SynF
-- deriv Sq          = Tw
deriv (Const c)   = Const 0
deriv (Add e1 e2) = Add (deriv e1) (deriv e2)

\end{code}
                             D (eval e) = eval (deriv e)

  eval (deriv Sq)
== -- Def. deriv
  eval Tw
== -- Def. eval
  (2*)
== -- Enl. der. regel   ****
  D (^2)
== -- Def. eval
  D (eval Sq)

















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
