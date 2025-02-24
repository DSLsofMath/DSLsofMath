\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Live_4_2 where
type REAL = Double
\end{code}

Domain-Specific Languages of Mathematics
Course Week 4, Lecture 2, Live-coding.


---------------
L4.2.1: Week 4, Lecture 2, Part 1
  Book §4.3 Compositional semantics
   and §4.4 Folds
\begin{code}
data IE where  -- "IE" stands for "Integer Expression"
  Add :: IE -> IE -> IE
  Mul :: IE -> IE -> IE
  Con :: Integer -> IE
 deriving Show
\end{code}
Spec.:  H2(eva,Add,(+)) = -- TODO
Spec.:  H2(eva,Mul,(*))
\begin{code}
type I = Integer
eva :: IE->I
eva = error "TODO"

e1, e2, e3, e4 :: IE
e1 = Con 1
e2 = Con 2
e3 = Add e1 e2  -- 1+2
e4 = Mul e3 e3  -- (1+2)*(1+2)

ebig 0 = Con 2
ebig n = let big = ebig (n-1) in Mul big big
\end{code}

----------------
Step 2: refactor to eva2 with three parameters (+), (*), id
H2(eva,              Add, (+))
H2(eva2 add mul con, Add, add)

Pattern: use a local helper for the recursion

\begin{code}
--       add         mul           con              e
eva2 :: (b->b->b) -> (b->b->b) -> (Integer -> b) -> IE->b
eva2 = error "TODO"

eva' :: IE -> I
eva' = error "TODO"

evaDeepCopy :: IE -> IE
evaDeepCopy = error "TODO"
\end{code}

Try to use eva2 for checking if an expression denotes an even number.

Specification:
  evenIE == even . eva
  H2(evenIE,Add,evenAdd)
  H2(evenIE,Mul,evenMul)
  H0(evenIE,Con c,evenCon c)

\begin{code}
evenIE :: IE -> Bool
evenIE = eva2 evenAdd evenMul evenCon

evenAdd = error "TODO"
evenMul = error "TODO"
evenCon = error "TODO"
\end{code}

----------------
+ L4.1.5: Make your own type class
  to keep the type and the parameters together

Implement the example folds as instances.
\begin{code}
\end{code}
s1, s2, s3, s4 :: IntExp a => a
s1 = con 1
s2 = con 2
s3 = add s1 s2  -- 1+2
s4 = mul s3 s3  -- (1+2)*(1+2)














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
\end{code}

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

----
Also the definition of eval is from L3.3:
\begin{code}
type S = REAL -> REAL
eval :: F -> S
eval (C c)        = cS c     -- :: REAL -> REAL
eval X            = xS
eval (Ad e1 e2)  = addS (eval e1) (eval e2)
eval (Mu e1 e2)  = mulS (eval e1) (eval e2)

cS :: REAL -> S
xS :: S
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

newtype Bi a = Bi (a, a) deriving (Show)
getf, getf' :: Bi a -> a
cder2   :: REAL -> Bi F
xder2   :: Bi F
addder2 :: Bi F -> Bi F -> Bi F
mulder2 :: Bi F -> Bi F -> Bi F

(der2,getf,getf',cder2, xder2, addder2, mulder2) = error "TODO"
\end{code}

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

\end{code}
sq :: Fun s => s
sq = mul x x
sqS :: REAL -> REAL
sqF :: FunExp
sqB :: Bi FunExp
sqS = sq
sqF = sq
sqB = sq

\begin{code}
\end{code}

\begin{code}
\end{code}
