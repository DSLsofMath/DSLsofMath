Problem 1 [25pts]: Algebraic structure: a DSL for semirings.

\begin{code}
{-# LANGUAGE GADTs #-}
module P1 where
import qualified Prelude
import Prelude((==),Bool(False,True),(&&),(||),and,Integer,String)
import Test.QuickCheck -- only for testing, not part of exam question
\end{code}

1a: Define a type class |SemiRing| that corresponds to the semiring
  structure.

\begin{code}
class SemiRing r where
  add    ::  r -> r -> r
  mul    ::  r -> r -> r
  zero   ::  r
  one    ::  r
\end{code}

1b: Define a datatype |SR v| for the language of semiring expressions
  (with variables of type |v|) and define a |SemiRing| instance for
  it.

\begin{code}
data SR v where
  Add    ::  SR v -> SR v -> SR v
  Mul    ::  SR v -> SR v -> SR v
  Zero   ::  SR v
  One    ::  SR v
  Var    ::  v -> SR v
\end{code}

\begin{code}
instance SemiRing (SR v) where add=Add; mul=Mul; zero=Zero; one=One
\end{code}

1c: Find two other instances of the |SemiRing| class.

\begin{code}
instance SemiRing Integer where add=(Prelude.+); mul=(Prelude.*); zero=0; one=1
instance SemiRing Bool    where add=(||); mul=(&&); zero=False; one=True
\end{code}

Other possibilities: square matrices (of a fixed size), Maybe Integer
with max and (+), Bool with xor and (&&), modulo arithmetics, ...

1d: Gice a type signature for, and define, a general evaluator for
  |SR v| expressions on the basis of an assignment function.

\begin{code}
eval :: SemiRing r => (v -> r) -> SR v -> r
eval f = e where
  e (Add x y)  =  add (e x) (e y)
  e (Mul x y)  =  mul (e x) (e y)
  e Zero       =  zero
  e One        =  one
  e (Var v)    =  f v
\end{code}

1e: Specialise |eval| to the types in 1c.

\begin{code}
evalI :: (v -> Integer)  -> SR v -> Integer
evalI = eval
evalB :: (v -> Bool)     -> SR v -> Bool
evalB = eval
\end{code}

\begin{code}
e1, e2, e3 :: SR String
e1 = Add (Var "x") One
e2 = Mul e1 e1
e3 = Add e2 e2
\end{code}

\begin{code}
v1I, v2I, v3I :: Integer
v1B, v2B, v3B :: Bool
aI :: String -> Integer
aI _ = 2
aB :: String -> Bool
aB _ = True
v1I = evalI aI e1
v2I = evalI aI e2
v3I = evalI aI e3
v1B = evalB aB e1
v2B = evalB aB e2
v3B = evalB aB e3
\end{code}

\begin{code}
check1e = and [ v1I==3, v2I==9, v3I==18, v1B, v2B, v3B]
\end{code}

----------------

The below is not required in the answer - just included here to check
the types.

Synonyms for more readable laws:

\begin{code}
(+), (⋅) :: SemiRing r => r -> r -> r
(+) = add
(⋅) = mul
\end{code}

\begin{code}
infixl 6 +
infixl 7 ⋅
\end{code}

\begin{code}
law1 a b c =    (a + b) + c == a + (b + c)
law2 a     =     zero + a == a && a + zero == a
law3 a b   =     a + b == b + a
\end{code}

\begin{code}
law4 a b c =     (a⋅b)⋅c == a⋅(b⋅c)
law5 a     =     one⋅a == a &&  a⋅one == a
\end{code}

\begin{code}
law6 a b c =     a⋅(b + c) == (a⋅b) + (a⋅c)
law7 a b c =     (a + b)⋅c == (a⋅c) + (b⋅c)
law8 a     =     a⋅zero == zero && zero⋅a == zero
\end{code}

\begin{code}
law1I i = law1 (i :: Integer); law1B b = law1 (b :: Bool)
law2I i = law2 (i :: Integer); law2B b = law2 (b :: Bool)
law3I i = law3 (i :: Integer); law3B b = law3 (b :: Bool)
law4I i = law4 (i :: Integer); law4B b = law4 (b :: Bool)
law5I i = law5 (i :: Integer); law5B b = law5 (b :: Bool)
law6I i = law6 (i :: Integer); law6B b = law6 (b :: Bool)
law7I i = law7 (i :: Integer); law7B b = law7 (b :: Bool)
law8I i = law8 (i :: Integer); law8B b = law8 (b :: Bool)
\end{code}

\begin{code}
main = do quickCheck law1I; quickCheck law1B
          quickCheck law2I; quickCheck law2B
          quickCheck law3I; quickCheck law3B
          quickCheck law4I; quickCheck law4B
          quickCheck law5I; quickCheck law5B
          quickCheck law6I; quickCheck law6B
          quickCheck law7I; quickCheck law7B
          quickCheck law8I; quickCheck law8B
\end{code}

