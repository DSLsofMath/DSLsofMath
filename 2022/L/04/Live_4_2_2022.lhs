\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Live_4_2 where
type REAL = Double
\end{code}

Domain-Specific Languages of Mathematics
Course Week 4, Lecture 2, Live-coding.
----------------
* 1. We reuse the definition of FunExp (from L3.3 and book §1.7.1).

FunExp is a syntax (DSL) for "1-argument function expressions".
\begin{code}
data FunExp = C REAL
            | X
            | Add FunExp FunExp  -- Add :: F -> F -> F
            | Mul FunExp FunExp
  deriving Show
\end{code}
Then some examples from Jamboard 4.2

\begin{code}
type F = FunExp
f, fbar, g :: F
f    = Add X (C 1)
fbar = Add X (C 0)
g    = X
h    = Mul f g
hbar = Mul fbar g
\end{code}

----
Also the definition of eval is from L3.3:
\begin{code}
type S = REAL -> REAL
eval :: F -> S
eval (C c)        = cS c     -- :: REAL -> REAL
eval X            = xS
eval (Add e1 e2)  = addS (eval e1) (eval e2)
eval (Mul e1 e2)  = mulS (eval e1) (eval e2)

cS   :: REAL -> S
xS   :: S
addS :: S -> S -> S
mulS :: S -> S -> S
cS = const
xS = id
addS f g = \x -> f x  +  g x
mulS f g = \x -> f x  *  g x
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
  forall fe. eval (der fe) = D (eval fe)

Specification of der2:
  forall fe.  let (f, f') = der2 fe
              in (eval f  ==    eval fe )  &&
                 (eval f' == D (eval fe))

\begin{code}
der :: F -> F
der = getf' . der2
deepcopy :: F -> F
deepcopy = getf . der2

der2 :: F -> Bi F
der2 (C c)        = cder2 c     -- :: Bi F
der2 X            = xder2
der2 (Add e1 e2)  = addder2 (der2 e1) (der2 e2) -- H2(der2,Add,addder2)
der2 (Mul e1 e2)  = mulder2 (der2 e1) (der2 e2) -- H2(der2,Mul,mulder2) 

newtype Bi a = Bi (a, a) deriving (Show)
getf, getf' :: Bi a -> a
getf  (Bi (f, _ )) = f
getf' (Bi (_, f')) = f'
cder2   :: REAL -> Bi F
cder2 c = Bi (C c, C 0)
xder2   :: Bi F
xder2 = Bi (X, C 1)
addder2 :: Bi F -> Bi F -> Bi F
addder2 (Bi (f, f')) (Bi (g, g')) = Bi (Add f g, Add f' g')
mulder2 :: Bi F -> Bi F -> Bi F
mulder2 (Bi (f, f')) (Bi (g, g')) = Bi (Mul f g, Add (Mul f' g) (Mul f g'))
\end{code}
der2 (Mul X X) == Bi ( Mul X X, Add (Mul (C 1.0) X)
                                    (Mul X (C 1.0)))

Sum up so far: three homomorphisms
  eval      :: F -> S    -- S = REAL -> REAL
  deepcopy  :: F -> F   
  der2      :: F -> Bi F
  (not der) -- not a homomorphism

----------------
* 4. Make your own type class (now for F = FunExp)
Reminder: Earlier lecture had data IE ~= DSL for Integer Expressions
  and we made a class IntExp to collect the constructors as methods.

Now we do it for FunExp
  (collect semantic operations corresp. to the constructors)

For reference: the types of our constructors (of F = FunExp):
  C   :: REAL -> F
  X   :: F
  Add :: F -> F -> F
  Mul :: F -> F -> F
\begin{code}
class Fun s where
  c   :: REAL -> s
  x   :: s
  add :: s -> s -> s
  mul :: s -> s -> s
instance Fun S       where c=cS;    x=xS;    add=addS;    mul=mulS    -- eval 
instance Fun F       where c=C;     x=X;     add=Add;     mul=Mul     -- deepcopy 
instance Fun (Bi F)  where c=cder2; x=xder2; add=addder2; mul=mulder2 -- derivative

sq :: Fun s => s
sq = mul x x
\end{code}

If you can define a function h as a homomorphism from the syntactic
constructors to corresponding "semantic constructors", then you can
also make an instance declaration of the corresponding type class.


\begin{code}
\end{code}

\begin{code}
\end{code}
