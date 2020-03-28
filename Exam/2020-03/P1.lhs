--------------------------------------------------------------------------------
-- P1 [25p]: Algebraic structure- a DSL for Quasigroups
--------------------------------------------------------------------------------
\begin{code}
{-# LANGUAGE GADTs , TypeSynonymInstances #-}
--------------------------------------------------------------------------------
-- (a) Define a type class Quasi that corresponds to the Quasigroup structure.
--------------------------------------------------------------------------------
class Quasi q where
  mul  :: q -> q -> q
  rdiv :: q -> q -> q
  ldiv :: q -> q -> q

--------------------------------------------------------------------------------
-- (b) Define a datatype Q v for the language of quasigroup expressions
--     (with variables of type v) and define a Quasi instance for it.
--------------------------------------------------------------------------------
data Q v where
  Mul  :: Q v -> Q v -> Q v
  RDiv :: Q v -> Q v -> Q v
  LDiv :: Q v -> Q v -> Q v
  Var  :: v -> Q v

instance Quasi (Q v) where
  mul = Mul
  rdiv = RDiv
  ldiv = LDiv

--------------------------------------------------------------------------------
-- (c) Find and implement two other instances of the Quasigroup class.
--     Make sure the laws are satisfied.
--------------------------------------------------------------------------------
instance Quasi Integer where
  mul = (+)
  rdiv = (-)
  ldiv = flip (-)

type RPos = Double
instance Quasi RPos where
  mul = (*)
  rdiv = (/)
  ldiv = flip (/)

--------------------------------------------------------------------------------
--(d) Give a type signature for, and define, a general evaluator for
--    Q v expressions on the basis of an assignment function.
--------------------------------------------------------------------------------
eval :: Quasi q => (v -> q) -> (Q v -> q)
eval af = e where
  e (Mul a b)  = mul  (e a) (e b)
  e (RDiv a b) = rdiv (e a) (e b)
  e (LDiv a b) = ldiv (e a) (e b)
  e (Var v)    = af v

--------------------------------------------------------------------------------
--(e) Specialise the evaluator to the two Quasi instances defined in (1c).
--    Take three quasigroup expressions of type Q String, give the appropriate
--    assignments and compute the results of evaluating, in each case,
--    the three expressions.
--------------------------------------------------------------------------------
evalI :: (v -> Integer) -> (Q v -> Integer)
evalI = eval

evalR :: (v -> RPos) -> (Q v -> RPos)
evalR = eval

x, y, e1, e2, e3 :: Q String

x = Var "x"
y = Var "y"

e1 = Mul x y
e2 = LDiv x e1 -- Should be equivalent to y, according to 3d law
e3 = RDiv e1 y -- Should be equivalent to x, according to 1st law

afI "x" = 2
afI "y" = -5

afR "x" = 0.5
afR "y" = 3.0

tests = ( map (evalI afI) [e1,e2,e3]  ==  [-3, -5, 2]
        , map (evalR afR) [e1,e2,e3]  ==  [1.5, 3.0, 0.5]
        )
\end{code}
