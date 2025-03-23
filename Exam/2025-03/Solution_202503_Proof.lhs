\item {[25p]} \textbf{Calculational proof:} Syntactic derivatives

Module header (not expected to be written on the exam).

\begin{code}
module Exam_202503_Proof where
import Prelude (error, Eq((==)), Show, Bool, (&&), snd, map)
import DSLsofMath.Algebra(
  Additive(zero,(+)), AddGroup(negate), (-),
  Multiplicative(one,(*)), Ring,
  (/), Field
  )
\end{code}

\begin{enumerate}
\item {[5p]} Give types for, and implement, |der2One|, |der2X|, and |der2Sub|.

Suggested solution:
\begin{code}
der2One  ::  (F, F)
der2One   =  (One,   Zero)

der2X    ::  (F, F)
der2X     =  (X,     One )

der2Sub :: (F, F) -> (F, F) -> (F, F)
der2Sub    (f, f')   (g, g') = (Sub f g, Sub f' g')
\end{code}

\item {[5p]} In an induction proof of correctness of |der2|, one of
  the base cases is |P(X)| (where |X| is a constructor in the
  datatype |F|). Prove this case using equational reasoning, carefully
  motivating each step as in ``by def.\ |eval One|'', ``by def.\ |der2 X|'', etc.

Suggested solution:
\begin{spec}
  P(X)
= -- Def. of P, and let (fe2, fe') = der2 X
  (fe2 == X)  &&  (eval fe' == D (eval X))
= -- Def. of der2 X, der2X, and eval X
  (X == X)  &&  (eval One == D (\x -> x))
= -- Simplify and use Def. of |eval One| and |D id = const 1|
  one == const 1
= -- Def. of one of function type
  const 1 == const 1
= -- Simplify
  True
\end{spec}

\item {[5p]} Give the type for, and implement, |der2Div|.

\begin{code}
der2Div :: (F, F) -> (F, F) -> (F, F)
der2Div (f, f') (g, g') = (Div f g, Div (Sub (Mul f' g) (Mul f g')) (Mul g g))
\end{code}

\item {[10p]} One of the inductive step cases is |Forall ge (Forall he
  (P(ge) && P(he) => P(Div ge he)))|. Prove this step using
  equational reasoning.
\end{enumerate}

Proof:
Start from the specification with |fe = Div ge he| and some convenient names:
\begin{spec}
let  g x  =  eval ge x
     h x  =  eval he x
     f x  =  eval fe x              -- Def. of fe
          =  eval (Div ge he) x     -- Def. of eval Div
          =  eval ge x / eval he f  -- Def. of g, h
          =  g x / h x
\end{spec}

Note that der2 always returns a pair, so that we can use these names below:

\begin{spec}
let  (ge2, ge') = der2 ge
     (he2, he') = der2 he
     (fe2, fe') = der2 (Div ge he)
\end{spec}
Then we start our equational reasoning:
\begin{spec}
  (fe2, fe')
= -- Naming above
  der2 (Div ge he)
= -- Def. of der2
  der2Div (der2 ge) (der2 he)
= -- Use names introduced above
  der2Div (ge2, ge') (he2, he')
= -- Def. of der2Div
  (Div ge2 he2, Div (Sub (Mul ge' he) (Mul ge he')) (Mul he he))
\end{spec}

Or, to sum it up, we know that |fe2 == Div ge2 he2| and |fe' = Div (Sub (Mul ge' he) (Mul ge he')) (Mul he he)|

Now we know from the induction hypothesis that
  |ge2 == ge|,  |eval ge' x == D (eval ge) x|, and
  |he2 == he|,  |eval he' x == D (eval he) x|.

The first component returned from |der2| is thus as expected and we
can make another equality chain for the second component:

\begin{spec}
  eval fe' x
= -- Above calculation
  eval (Div (Sub (Mul ge' he) (Mul ge he')) (Mul he he)) x
= -- Def. of eval
  eval (Sub (Mul ge' he) (Mul ge he')) x / eval (Mul he he) x
= -- Def. of eval
  (eval (Mul ge' he) x - eval (Mul ge he') x) / (eval he x * eval he x)
= -- Def. of eval
  (eval ge' x * eval he x - eval ge x * eval he' x) / (eval he x * eval he x)
= -- Names from above, plus a few new: g' = eval ge'; h' = eval he'
  (g' x * h x - g x * h' x) / (h x * h x)
= -- Derivative rule for division
  D (\x -> g x / h x) x
= -- Calculation above
  D f x
\end{spec}

or to summarise:  |eval fe' = D f|.

-- end of solution

Below is just code from the exam question + some testing code.

\begin{code}
data F = Zero | One | X | Sub F F | Mul F F | Div F F deriving (Eq, Show)

eval :: Field a => F -> (a->a)                      -- Equality labels below (for the proof)
eval Zero  _  = zero                                -- |eval Zero|
eval One   _  = one                                 -- |eval One|
eval X     x  = x                                   -- |eval X|
eval (Sub  fe ge)  x = eval fe x  -  eval ge x      -- |eval Sub|
eval (Mul  fe ge)  x = eval fe x  *  eval ge x      -- |eval Mul|
eval (Div  fe ge)  x = eval fe x  /  eval ge x      -- |eval Div|

der2 :: F -> (F, F)
der2 Zero     = der2Zero                               -- |der2 Zero|
der2 One      = der2One                                -- |der2 One|
der2 X        = der2X                                  -- |der2 X|
der2 (Sub  fe ge)  =  der2Sub  (der2 fe)  (der2 ge)    -- |der2 Sub|
der2 (Mul  fe ge)  =  der2Mul  (der2 fe)  (der2 ge)    -- |der2 Mul|
der2 (Div  fe ge)  =  der2Div  (der2 fe)  (der2 ge)    -- |der2 Div|
\end{code}

Let the property |P(fe)| be ``Let |fe2| be the first, and |fe'| the
second, component of the pair returned by |der2 fe|. Then |fe2 == fe|
and |eval fe' == D (eval fe)|, where |D| is the differentiation
operator.'' The specification of |der2| is then |Forall fe (P(fe))|.

Pseudo-code version (for type checking the exam):
\begin{code}
der2Spec :: (Field a, Eq a) => F -> a -> Bool
der2Spec fe x =
  let  (fe2, fe') = der2 fe
  in (fe2 == fe)  &&
     (eval fe' x == mathDer (eval fe) x)
mathDer :: (a->a) -> (a->a)
mathDer = error "mathDer cannot be implemented"
\end{code}
Helper code for checking the exam (not part of the task for students):
\begin{code}
negF :: F -> F
negF f = Sub Zero f
addF :: F -> F -> F
addF f g = Sub f (negF g)
der2Mul :: (F, F) -> (F, F) -> (F, F)
der2Mul (f,f') (g,g') = (Mul f g, addF (Mul f' g) (Mul f g'))

der2Zero :: (F, F)
der2Zero  = (Zero,  Zero)
\end{code}


\begin{code}
test1, test2 :: Field a => a -> (Bool, a, a)
test1 x = let fe = Div X X; (fe2, fe') = der2 fe in (fe2==fe, eval fe' x, eval Zero x)
test2 x = let fe = Div (Mul X X) X; (fe2, fe') = der2 fe in (fe2==fe, eval fe' x, eval One x)
test3 :: Field a => a -> [a]
test3 x = map (\e -> eval e x) [
    snd (der2 (Div (Mul X X) X))
  , Div (Sub (Mul (Sub (Mul One X) (Sub Zero (Mul X One))) X) (Mul (Mul X X) One)) (Mul X X)
  , Div (Sub (Mul (two*X) X) (Mul X X)) (Mul X X)
  , Div (Sub (two*(X*X)) (Mul X X)) (Mul X X)
  , Div (X*X) (Mul X X)
  , One
  ]

two :: Ring a => a
two = one + one
instance Additive F        where (+) = addF; zero = Zero
instance AddGroup F        where negate = negF
instance Multiplicative F  where (*) = Mul; one = One
\end{code}
