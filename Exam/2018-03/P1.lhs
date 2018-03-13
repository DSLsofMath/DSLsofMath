1. Algebraic structure: a DSL for monoids

* |(R, +, 0)| is a monoid with identity element |0|:
    (a + b) + c = a + (b + c)
    0 + a = a + 0 = a

1a Define a type class |Monoid| that corresponds to the
    monoid structure.
1b Define a datatype |ME v| for the language of monoid
    expressions (with variables of type |v|) and define a |Monoid|
    instance for it. (These are expressions formed from applying the
    monoid operations to the appropriate number of arguments, e.g.,
    all the left hand sides and right hand sides of the above
    equations.)

1c Find two other instances of the |Monoid| class. \label{item:inst}

1d Give a type signature for, and define, a general evaluator for
    |ME v| expressions on the basis of an assignment function.

1e Specialise the evaluator to the two |Monoid| instances
    defined in (\ref{item:inst}).  Take three monoid expressions of
    type |ME String|, give the appropriate assignments and compute the
    results of evaluating, in each case, the three expressions.

\begin{code}
module P1 where
import Prelude hiding (Monoid)
class Monoid m where unit :: m; op :: m->m->m     -- 1a class
data ME v = Unit | Op (ME v) (ME v) | V v         -- 1b data
instance Monoid (ME v)  where unit=Unit; op=Op    -- 1b instance
instance Monoid Bool    where unit=True; op=(&&)  -- 1c Bool
instance Monoid Integer where unit=0;    op=(+)   -- 1c Integer
eval :: Monoid m => (v->m) -> (ME v -> m)         -- 1d eval
eval f = e where e Unit = unit; e (Op x y) = op (e x) (e y); e (V v) = f v
evalB :: (v->Bool)   ->(ME v->Bool);    evalB=eval-- 1e evalB
evalI :: (v->Integer)->(ME v->Integer); evalI=eval-- 1e evalI
e1=Op(V"a")Unit;e2=Op(V"b")e1;e3=V"b"`Op`V"a"     -- 1e three expr
fB=("a"==);testB=map(evalB fB)[e1,e2,e3]==[True,False,False]
fI"a"=1;fI"b"=2;testI=map(evalI fI)[e1,e2,e3]==[1,3,3]
main=print$testB&&testI
\end{code}
