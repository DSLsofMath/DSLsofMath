Week & chapter 3: Types in mathematics

Learning outcomes

* Knowledge and understanding
** L3.1: organize areas of mathematics in DSL terms
** L3.1: explain main concepts of elementary real analysis
** L3: design and implement a DSL for derivatives

* Skills and abilities
** L3.1: develop adequate notation for mathematical concepts
** L3.2: perform calculational proofs

* Judgement and approach
** A1: discuss and compare different software implementations of mathematical concepts


* On the blackboard / other lecture parts:
+ limit of a function      [[https://youtu.be/Zky9J949jo4][lecture video part 1]]
+ types for some examples, most importantly D : (X->Y) -> (X->Y)
+ using lim to define |D|  [[https://youtu.be/0Xzvn5nVbh8][lecture video part 2]]
+ derivative of a function f : Func = REAL -> REAL

deriv :: (R->R)->(R->R)
deriv f x = ???
-- Försök 1: fungerar inte:
  -- "om f = sq låt högerledet vara 2*x"
  -- "om f = f1 `oplus` f2 låt högerledet vara deriv f1 x + deriv f2 x"
-- Försök 2: fungerar inte heller!
  -- använd lim och psi  psi f x h = (f (x+h) - f x)/h
-- Försök 3: Skapa ett DSL för funktioner!

* Live coding
DSL for derivatives ={here} a DSL for 1-argument functions (or 1-var. epressions)

Example functions / expressions:

\begin{code}
sq x = x^2
tw x = 2*x
c2 x = 2
psi f x h = (f (x+h) - f x)/h
t1 = psi sq -- leder inte till rätt väg

type REAL = Double

-- Syntax datatype for 1-argument function expressions

data SynF =
     Con REAL        -- konstant funktion
  |  X               -- identitetsfunktionen
  |  Tw
  |  Sq
  |  Pow Int   -- Sq = Pow 2
  |  Add SynF SynF   -- addera funktioner punktvis
  |  Mul SynF SynF   -- mult.  funktioner punktvis
 deriving Show
  -- Saknas: Div SynF SynF
  --    Sub - kan implementeras med Mul, Con, och Add
  --
-- Semantic type   for 1-argument function expressions
type SemF = REAL -> REAL
eval :: SynF -> SemF
eval (Con c)          = \x -> c  -- const c
eval X                = id
eval Tw               = (2*)   -- \x -> 2*x
eval Sq               = (^2)   -- \x -> x^2
eval (Pow n)          = (^n)
eval (Add fSyn gSyn)  = oadd  (eval fSyn) (eval gSyn)
eval (Mul fSyn gSyn)  = omul  (eval fSyn) (eval gSyn)


-- Add :: SynF -> SynF -> SynF
oadd  :: SemF -> SemF -> SemF
oadd f g = \x -> f x + g x
omul  :: SemF -> SemF -> SemF
omul f g = \x -> f x * g x

deriv :: SynF -> SynF
deriv (Add f g)  = Add (deriv f) (deriv g)
deriv Sq         = Tw
deriv Tw         = Con 2
deriv X          = Con 1
deriv (Con c)    = Con 0
deriv (Mul f g)  = Add (Mul (deriv f) g) (Mul f (deriv g))

simplify :: SynF -> SynF
simplify = error "Exercise!"  -- Mul (Con 1) e  =  e

e1, e2, e3, e4 :: SynF
e1 = Sq
e2 = Tw
e3 = Add e1 e2
e4 = Add X (Mul X X)   -- X + X^2


{-
deriv (Mul f g)  = (f'*g) + (f*g')
  where  f' = deriv f
         g' = deriv g
         (+) = Add
         (*) = Mul
-}
\end{code}
q = Add (Mul (Con 1.0) X)
        (Mul X (Con 1.0))
q = Add X X
q = Tw

-- Fel q:
q = Mul  (Add (Mul (Con 1.0) X) X)
         (Con 1.0)
q = Add (Mul (Con 1.0) X)
        X
q = Add X X
q = Tw



DSL behöver
 -- syntaxtyp    Syn
 -- semantiktyp         Sem
 -- eval ::      Syn -> Sem
