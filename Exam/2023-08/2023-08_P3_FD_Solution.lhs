Derivatives and |FunExp|

Consider the domain of simple functions of one variable.
The syntax is given by |FunExp|, the semantic type by |S|, and the
semantics by the homomorphism |eval|:

\begin{code}
type REAL = Double
data FunExp = C REAL  | X  | Add FunExp FunExp  | Mul FunExp FunExp
  deriving Show

type F  = FunExp
type S  = REAL -> REAL

eval :: F         ->  S
eval (C c)        =   cS c
eval X            =   xS
eval (Add e1 e2)  =   addS (eval e1) (eval e2)
eval (Mul e1 e2)  =   mulS (eval e1) (eval e2)

cS    :: REAL -> S;       cS = const
xS    :: S;               xS = id
addS  :: S -> S -> S;     addS f g = \x -> f x +  g x
mulS  :: S -> S -> S;     mulS f g = \x -> f x *  g x
\end{code}

Your task is to implement another homomorphism: |der2 : F -> (F, F)|
which (for all |fe : F|) should satisfy |eval (fst (der2 fe)) = eval
fe| and |eval (snd (der2 fe)) = D (eval fe)| where |D : S -> S| is the
derivative operator.

3a [5p] Implement the top level (recursive) structure in the same
  style as |eval| so that it is clear that |der2| is also a
  homomorphism. In this part you can assume that |cder2|, |xder2|,
  |addder2|, and |mulder2| are available (``wishful thinking'').

\begin{code}
type FD = (F, F)
der2 :: F         ->  FD
der2 (C c)        =   cder2 c
der2 X            =   xder2
der2 (Add e1 e2)  =   addder2 (der2 e1) (der2 e2)
der2 (Mul e1 e2)  =   mulder2 (der2 e1) (der2 e2)
\end{code}


3b [5p] Give the types and implementations of |cder2| and |xder2|.

\begin{code}
cder2 :: REAL -> FD;
xder2 :: FD;
cder2 c  = (C c,  C 0)
xder2    = (X,    C 1)
\end{code}

3c [10p] Give the types and implementations of |addder2| and |mulder2|.

\begin{code}
addder2  :: FD -> FD -> FD;
mulder2  :: FD -> FD -> FD;
addder2 (f,f') (g,g') =  (Add f g,  Add f' g')
mulder2 (f,f') (g,g') =  (Mul f g,  Add (Mul f' g) (Mul f g'))
\end{code}


-------------------
Not part of the exam question

Type checking the specification (cannot run law2 because deriv is not
implementable).

\begin{code}
law1 fe = eval (fst (der2 fe)) =.= eval fe
law2 fe = eval (snd (der2 fe)) =.= deriv (eval fe)

deriv :: S -> S
deriv = undefined

-- Just an approximate equality check for functions
f =.= g = all (\x -> f x == g x) [0,1,sqrt 2,pi]
\end{code}
