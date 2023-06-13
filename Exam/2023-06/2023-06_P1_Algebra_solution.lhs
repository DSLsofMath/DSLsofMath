--------------------------------------------------------------------------------
-- P1 [25p]: Algebraic structure- a DSL for Loop
--------------------------------------------------------------------------------
\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
import Prelude hiding ((**), (//), (\\))

--------------------------------------------------------------------------------
-- (a) Define a type class Loop l that corresponds to the Loop structure.
--------------------------------------------------------------------------------
class Loop l where
  e     ::  l
  mul   ::  l -> l -> l
  rdiv  ::  l -> l -> l
  ldiv  ::  l -> l -> l

--------------------------------------------------------------------------------
-- (b) Define a datatype L v for the language of loop expressions
--  (with variables of type v) and define a Loop instance for
--  it. (These are expressions formed from applying the loop operations
--  to the appropriate number of arguments, e.g., all the left hand
--  sides and right hand sides of the above equations.)
--------------------------------------------------------------------------------
data L v where
  E     :: L v
  Mul   :: L v -> L v -> L v
  RDiv  :: L v -> L v -> L v
  LDiv  :: L v -> L v -> L v
  Var   :: v -> L v

instance Loop (L v) where
  e     = E
  mul   = Mul
  rdiv  = RDiv
  ldiv  = LDiv

--------------------------------------------------------------------------------
-- (c) Integers form an instance of |Loop| with |(**) = (+)| and
-- |e=0|. Calculate the definitions of |(//)| and |(\\)| from the
-- above identities and implement the instance of the Loop
--------------------------------------------------------------------------------
-- Just for readability (not part of the exam question):
(**), (//), (\\) :: Loop a => a -> a -> a
(**) = mul
(//) = rdiv
(\\) = ldiv

-- Just for type-checking (not part of the exam question)
law1, law2, law3, law4 :: (Eq a, Loop a) => a -> a -> Bool
law1 x y =   y == (y ** x) // x
law2 x y =   y == (y // x) ** x
law3 x y =   y == x \\ (x ** y)
law4 x y =   y == x ** (x \\ y)
law5 x   =   x ** e == x
law6 x   =   e ** x == x

{-
Law 2 says that for all integers x and y
  y == (y // x) + x
which simplifies to
  y - x == y // x
Thus we can define 
 y // x = y - x
or simply
 (//) = (-)
We can check that also law 1 is satisfied:
  (y + x) - x = y + (x-x) = y + 0 = y

Similarly, law 4 says that
  y == x + (x \\ y)
which simplifies to
  y - x == x \\ y
or simply
(\\) = flip (-)
We can check that also law 3 is satisfied:
  flip (-) x (x + y) = (x+y) - x = y+(x-x) = y+0 = y
-}


instance Loop Integer where
  e     = 0
  mul   = (+)
  rdiv  = (-)
  ldiv  = flip (-)



-- Not part of the question:
type RPos = Double
instance Loop RPos where
  e = 1
  mul = (*)
  rdiv = (/)
  ldiv = flip (/)

--------------------------------------------------------------------------------
-- (d) Give a type signature for, and define, a general evaluator for
--   L v expressions on the basis of an assignment function.
--------------------------------------------------------------------------------
eval :: Loop q => (v -> q) -> (L v -> q)
eval af = eval'
 where
  eval' E = e
  eval' (Mul a b) = mul (eval' a) (eval' b)
  eval' (RDiv a b) = rdiv (eval' a) (eval' b)
  eval' (LDiv a b) = ldiv (eval' a) (eval' b)
  eval' (Var v) = af v

--------------------------------------------------------------------------------
{- (e) Specialise the evaluator to the |Loop| instance defined in (c).
  Define three loop expressions of type |L String|. Give appropriate
  assignments and compute the results of evaluating the three
  expressions.
-}
  
--------------------------------------------------------------------------------
evalI :: (v -> Integer) -> (L v -> Integer)
evalI = eval
x, y, e1, e2, e3, e4 :: L String
x = Var "x"
y = Var "y"
e1 = Mul x y
e2 = LDiv x e1 -- Should be equivalent to y, according to 3rd law
e3 = RDiv e1 y -- Should be equivalent to x, according to 1st law
e4 = Mul x E

afI "x" = 2
afI "y" = -5

test =
  map (evalI afI) [e1, e2, e3, e4] == [-3, -5, 2, 2]
\end{code}
