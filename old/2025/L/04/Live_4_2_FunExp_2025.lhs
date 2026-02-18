\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Live_4_2 where
type REAL = Double
\end{code}
L4.2.1: Week 4, Lecture 2, Part 2 (if time permits)

----------------
* 1. We reuse the definition of FunExp (from L3.3 and book §1.7.1).

FunExp is a syntax (DSL) for "1-argument function expressions".
\begin{code}
data FunExp = C REAL
            | X
            | Ad FunExp FunExp  -- Ad :: F -> F -> F
            | Mu FunExp FunExp
  deriving Show

e1, e2, e3, e4 :: FunExp
e1 = X
e2 = C 1
e3 = Ad e1 e2
e4 = Mu e3 (Ad X (C (-1)))
-- Mu (Ad X (C 1.0)) (Ad X (C (-1.0)))
-- (X+1)*(X-1)   means the function \X -> (X+1)*(X-1)

-- (the definition of eval is from L3.3)
type FunSem = REAL -> REAL
eval :: FunExp -> FunSem
eval (C c)  = cSem c
eval X      = xSem
eval (Ad fe ge) = addSem (eval fe) (eval ge)   -- H2(eval, Ad, addSem)
eval (Mu fe ge) = mulSem (eval fe) (eval ge)   -- H2(eval, mu, mulSem)

cSem :: REAL -> FunSem
cSem = const
xSem :: FunSem
xSem = id


addSem, mulSem :: FunSem -> FunSem -> FunSem
addSem f g = \x -> f x  +  g x
mulSem f g = \x -> f x  *  g x

derFail :: FunExp -> FunSem
derFail (C c)  = conDer c
derFail X      = xDer
derFail (Ad fe ge) = addDer (derFail fe) (derFail ge)   -- H2(derFail, Ad, addDer)
derFail (Mu fe ge) = mulDer (derFail fe) (derFail ge)   -- H2(derFail, mu, mulDer)

conDer c = const 0
xDer = const 1
addDer = addSem
mulDer = error "hard!"

\end{code}
D (f*g) = D f * g  + f * D g

Then some examples from the blackboard (proof that der is _not_ a
homomorphism).

\begin{code}
type F = FunExp
f, fbar, g :: F
f    = Ad X (C 1)
fbar = Ad X (C 0)
g    = X
h    = Mu f g
hbar = Mu fbar g
\end{code}

----------------
* 2. The tupling transform

Theory (see also exercise 1.6 in the book): a function fg returning a
pair can be seen as two functions f and g:
\begin{code}
f2p :: (a->(b,c)) -> (a->b, a->c)
f2p fg = (fst . fg, snd . fg)

p2f :: (a->b, a->c) -> (a->(b,c))
p2f (f, g) = \x->(f x, g x)
\end{code}

Here we want to use it to help implementing a function that is not by
itself a homomorphism. Example: der :: F -> F
Specification: H1(eval,der,D) or in other words (eval . der = D . eval)
We need to combine der with deepcopy :: F -> F:
\begin{spec}
deepcopy :: F ->  F
der      :: F ->    F
der2     :: F -> (F,F)
der2 = p2f (deepcopy, der)  -- specification alt. 1
(deepcopy, der) = f2p der2  -- specification alt. 2
\end{spec}

----------------
* 3. Implement der2 :: F -> (F,F)

Specification of der:
  ∀ fe. eval (der fe) = D (eval fe)

Specification of der2:
  ∀ fe.  let (f, f') = der2 fe
              in (eval f  ==    eval fe )  &&
                 (eval f' == D (eval fe))

\begin{code}
der :: F -> F
der = getf' . der2
deepcopy :: F -> F
deepcopy = getf . der2

der2    :: F -> Bi F
der2 (Ad fe ge) = addder2 (der2 fe) (der2 ge)
der2 (Mu fe ge) = mulder2 (der2 fe) (der2 ge)
der2 (C c)      = cder2 c
der2 X          = xder2

newtype Bi a = Bi (a, a) deriving (Show)
getf, getf' :: Bi a -> a
getf  (Bi (f, _ )) = f
getf' (Bi (_, f')) = f'

cder2   :: REAL -> Bi F
xder2   :: Bi F
addder2 :: Bi F -> Bi F -> Bi F
mulder2 :: Bi F -> Bi F -> Bi F

cder2 c = Bi (C c, C 0)
xder2   = Bi (X,   C 1)
addder2 (Bi (fe, fe')) (Bi (ge, ge')) = Bi (Ad fe ge, Ad fe' ge')
mulder2 (Bi (fe, fe')) (Bi (ge, ge')) = Bi (Mu fe ge, Ad (Mu fe' ge) (Mu fe ge'))
\end{code}

Bi (X*X - 1, 2*X)

simplify :: FunExp -> FunExp
simplify (Mu (C 0) _) = C 0   -- good exercise

Sum up so far:
  eval      :: F -> S    -- S = REAL -> REAL
  deepcopy  :: F -> F
  der2      :: F -> Bi F

----------------
* 4. Make your own type class (now for F = FunExp)
(If time permits)

Reminder: Earlier lecture had data IE ~= DSL for Integer expressions
  and we made a class IntExp to collect the constructors as methods.

Now we do it for FunExp
  (collect semantic operations corresp. to the constructors)

For reference: the types of our constructors:
  C   :: REAL -> F
  X   :: F
  Ad :: F -> F -> F
  Mu :: F -> F -> F

\begin{code}
class Fun s where
  c   :: REAL -> s
  x   :: s
  add :: s -> s -> s
  mul :: s -> s -> s

instance Fun FunExp       where c = C;      x = X;      add = Ad;       mul = Mu
instance Fun FunSem       where c = cSem;   x = xSem;   add = addSem;   mul = mulSem
instance Fun (Bi FunExp)  where c = cder2;  x = xder2;  add = addder2;  mul = mulder2

sq :: Fun s => s
sq = mul x x

sqS :: REAL -> REAL
sqF :: FunExp
sqB :: Bi FunExp
sqS = sq
sqF = sq
sqB = sq
\end{code}
