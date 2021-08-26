\begin{code}
{-# LANGUAGE GADTs #-}
\end{code}
P3 [25p] Algebraic structure: abelian monoid

----------------
3a [5p] Define a type class |AbMonoid| that corresponds to the abelian
monoid structure.
\begin{code}
class AbMonoid a where
  zero :: a
  oplus :: a -> a -> a
\end{code}

----------------
3b [5p] Define a datatype |AbMonoidExp v| for the language of abelian monoid
expressions (with variables of type |v|) and define an |AbMonoid|
instance for it.  (These are expressions formed from applying the
monoid operations to the appropriate number of arguments, e.g., all
the left hand sides and right hand sides of the

\begin{code}
data AbMonoidExp v where
  Zero    :: AbMonoidExp v
  Oplus   :: AbMonoidExp v -> AbMonoidExp v -> AbMonoidExp v
  Var     :: v -> AbMonoidExp v

instance AbMonoid (AbMonoidExp v) where zero = Zero; oplus = Oplus
\end{code}

----------------
3c [5p] Find one other instance of the |AbMonoid| class and give an example
which is a monoid but \textbf{not} an |AbMonoid|.

\begin{code}
instance AbMonoid Integer where
  zero = 0
  oplus = (+)
\end{code}

Lists with concatenation is a monoid, but not an |AbMonoid| because
|xs++ys| is usually not the same as |ys++xs| (thus the operation |++|
is not commutative).

\begin{spec}
instance Monoid [a] where
  zero = []
  oplus = (++)
\end{spec}

----------------
3d [5p] Define a general evaluator for |AbMonoidExp v| expressions on the
  basis of an assignment function.

\begin{code}
eval :: AbMonoid a => (v->a) -> AbMonoidExp v -> a
eval f Zero = zero
eval f (Oplus x y) = oplus (eval f x) (eval f y)
eval f (Var v) = f v
\end{code}

----------------
3e [5p]  Specialise the evaluator to the |AbMonoid| instance defined at
  point 3c. Take three |AbMonoidExp String| expressions,
  give the appropriate assignments and compute the results of
  evaluating the three expressions.

\begin{code}
evalI :: (v->Integer) -> AbMonoidExp v -> Integer
evalI = eval

e1, e2, e3 :: AbMonoidExp String
e1 = Oplus (Var "x") (Var "y")
e2 = Oplus (Var "y") (Var "x")
e3 = Oplus e1 Zero

myf :: String -> Integer
myf "x" = 1
myf "y" = 2
myf _   = -1

main :: IO ()
main = print $ map (evalI myf) [e1, e2, e3] == [3, 3, 3]
\end{code}
