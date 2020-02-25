



----

Below is supporting code, not part of the learning outcomes.

propH2 h op1 op2 x y = h (op1 x y) === op2 (h x) (h y)

\begin{code}
{-# LANGUAGE TypeFamilies #-}
propH1 ::  SemEq b =>
           (a->b) -> (a->a) -> (b->b) ->
           a -> EQ b
propH1 h fa fb = (h . fa) === (fb . h)
-- same as
-- propH1 h fa fb = (\x -> h (fa x)) === (\x -> fb (h x))

propH2 ::  SemEq b =>
           (a->b) -> (a->a->a) -> (b->b->b) ->
           a -> a -> EQ b
propH2 h op1 op2 = (\x y -> h (op1 x y)) === (\x y -> op2 (h x) (h y))


class SemEq s where
  type EQ s
  (===) :: s -> s -> EQ s

instance SemEq b => SemEq (a->b) where
  type EQ (a->b) = a -> EQ b
  (===) = semEqFun

semEqFun :: SemEq b => (a->b) -> (a->b) -> a -> EQ b
semEqFun = error "TODO"

type Nat = Int

instance SemEq a => SemEq [a] where
  type EQ [a] = Nat -> EQ a
  (===) = semEqList

semEqList :: SemEq a => [a] -> [a] -> Nat -> EQ a
semEqList xs ys i = (xs!!i) === (ys!!i)

instance SemEq Double where
  type EQ Double = Bool
  (===) = (==)
\end{code}
