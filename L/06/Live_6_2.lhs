Higher-order Derivatives and their Applications

Yet another type for representing functions

\begin{code}
{-# LANGUAGE TypeFamilies #-}
module DSLsofMath.Live_6_2 where
import DSLsofMath.FunExp
import DSLsofMath.Derive
import DSLsofMath.Simplify

d :: FunExp -> FunExp
d = simplify . derive

-- eval :: FunExp -> Func
type DS = []
evalAll :: FunExp -> DS Func
evalAll e = eval e : evalAll (d e)

es :: DS FunExp
es = (Id:*:Id) : map d es
fs :: DS Func
fs = evalAll (Id:*:Id)   -- x²

testfs :: [Func]
testfs = take 5 fs

testvs1 :: [REAL]
testvs1 = apply 1 testfs
testvs2 = apply 2 testfs

apply :: a -> [a -> b] -> [b]
\end{code}

Questions:

* Is there a function |derDS| which makes |evalAll| a homomorphism from |derive| to |derDS|?
* Is there a definition of |mulDS| which makes |evalAll| a homomorphism from |:*:| to |mulDS|?

Equational reasoning: we want to prove |evalAll (derive e) == derDS (evalAll e)| for all |e|.

Conveniently, by the definition of |evalAll e| the RHS is

  derDS (eval e : evalAll (derive e))

which means we can pick |derDS = tail| to get to the LHS!

So, yes, there is a (very simple) function that implements |derDS|.

----------------

|d :: FunExp -> FunExp|
we want a function |derDS| such that |evalAll :: FunExp -> DS Func| is a homomorphism

We want to show  |derDS (evalAll e) == evalAll (d e)|


  LHS
=
  derDS (evalAll e)
=
  derDS (eval e : evalAll (d e))
= -- let derDS = tail
  tail  (eval e : evalAll (d e))
= -- def. av tail
  evalAll (d e)
=
  RHS

\begin{code}

derDS :: DS Func -> DS Func
derDS = tail

addDS :: DS Func -> DS Func -> DS Func
addDS = zipWith (+)
\end{code}
  propH1 evalAll (:+:) addDS
  forall x y. evalAll (x:+:y) === addDS (evalAll x) (evalAll y)
  show that addDS = zipWith (+)

   LHS
=
  evalAll (x:+:y)
= -- def. evalAll
  eval (x:+:y) : evalAll (d (x:+:y))
= -- def. d   -- H2(d,(:+:),(:+:)) = forall x,y. d(x:+:y) = d x :+: d y
  eval (x:+:y) : evalAll (d x :+: d y)
= -- H2(eval,(:+:),(+))
  ((eval x) + (eval y))  :  zipWith (+) (evalAll (d x)) (evalAll (d y))
= -- zipWith
  zipWith (+) (eval x : evalAll (d x))   (eval y : evalAll (d y))
= -- evalAll
  zipWith (+) (evalAll x)                (evalAll y)
=
  RHS

--

Next: the multiplication case.

We are looking for a function |mulDS| and we would like it to be
"multiplication" for "derivative streams". We want |evalAll| to
be a homomorphism from |(:*:)| to |mulDS|. Thus it should satisfy

forall fe, ge. evalAll (fe :*: ge) == mulDS (evalAll fe) (evalAll ge)

As |mulDS| should return a stream we need to compute its head and its tail.
%
To compute the head we let |fs = evalAll fe| and |gs = evalAll ge|.
%
Then by the definition of |evalAll| we have |head fs == eval fe| and
|head gs == eval ge|.
%
Similarly, we can calculate:
\begin{spec}
  head (evalAll (fe :*: ge))
== {- Def. of |evalAll| -}
  eval (fe :*: ge)
== {- Def. of |eval| -}
  (eval fe) * (eval ge)
== {- See above -}
  (head fs) * (head gs)
\end{spec}
%
Thus the ``|head|-part'' of the homomorphism condition is satisfied if:
%
\begin{spec}
  head (mulDS fs gs) == (head fs) * (head gs)
\end{spec}

To come up with a definition for the tail of |mulDS fs gs| we start by
noting that |tail| for |DS| is actually an implementation of |derDS|.
%
Thinking in terms of the usual law for derivative of a product we
would like:
%
\begin{spec}
  d (fs*gs) == (d fs)*gs + fs*(d gs)  where d = derDS; (*) = mulDS
\end{spec}
%
which is the same as
%
\begin{spec}
  tail (fs*gs) == (tail fs)*gs + fs*(tail gs)  where (*) = mulDS
\end{spec}
%
Both the |head| and |tail| conditions are satisfied by the following definition:
%
\begin{code}
mulDS :: DS Func -> DS Func -> DS Func
mulDS fs gs = h : t
  where  h = (head fs) * (head gs)
         t = ((derDS fs)*gs) + (fs*(derDS gs))
           where  (*) = mulDS
                  (+) = addDS
\end{code}
or equivalently:
\begin{code}
mulDS' :: Num a => DS a -> DS a -> DS a
mulDS' fs@(f:fs') gs@(g:gs') =
  (f*g) : addDS' (mulDS' fs' gs) (mulDS' fs gs')

addDS' :: Num a => DS a -> DS a -> DS a
addDS' = zipWith (+)
\end{code}

Some test code:
\begin{code}
e1s :: DS Func
e1s = evalAll (Exp Id)
e2s :: DS Func
e2s = evalAll (Exp Id :*: Exp Id)
e3s :: DS Func
e3s = mulDS e1s e1s

apply x = map (\f->f x)

t :: DS Func -> [REAL]
t = take 6 . apply 0
\end{code}

Now, we have done all of this with streams of functions, but all the
definitions go through also after picking a particular point using
|apply x|. The classical example is to pick |x=0| to get the
"Maclaurin series".

\begin{code}
newtype DerStream a = DS [a]   -- could be called Maclaurin

instance Num a => Num (DerStream a) where
  (+) = addDerStream
  (*) = mulDerStream
  negate = negateDerStream
  fromInteger = fromIntegerDerStream

addDerStream :: Num a => DerStream a -> DerStream a -> DerStream a
addDerStream (DS xs) (DS ys) = DS (addDS' xs ys)

mulDerStream :: Num a => DerStream a -> DerStream a -> DerStream a
mulDerStream (DS xs) (DS ys) = DS (mulDS' xs ys)

fromIntegerDerStream :: Num a => Integer -> DerStream a
fromIntegerDerStream i = DS (fromInteger i : repeat 0)

negateDerStream :: Num a => DerStream a -> DerStream a
negateDerStream (DS xs) = DS (map negate xs)
\end{code}




----

Below is supporting code, not part of the learning outcomes.

propH2 h op1 op2 x y = h (op1 x y) === op2 (h x) (h y)

\begin{code}
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
