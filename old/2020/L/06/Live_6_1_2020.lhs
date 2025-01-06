Higher-order Derivatives and their Applications

Yet another type for representing functions.

\begin{code}
module DSLsofMath.Live_6_2 where
import DSLsofMath.FunExp    -- Syntax for functions
import DSLsofMath.Derive    -- Syntactic computation of derivatives
import DSLsofMath.Simplify  -- Algebraic simplification

d :: FunExp -> FunExp
d = simplify . derive

-- eval :: FunExp -> Func
type DS = []
evalAll :: FunExp -> DS Func
evalAll e = eval e : evalAll (d e)

-- testing
si = evalAll (Sin Id)
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

Question:
  Exists mul. H2(evalAll, (:*:), mul)  ?

Expand the definition:
  H2(evalAll, (:*:), mul) =
    forall fe, ge. evalAll (fe :*: ge) = mul (evalAll fe) (evalAll ge)

Start simplifying the expression(s)

  evalAll (fe :*: ge)
= -- def. of evalAll
  eval (fe :*: ge)    : evalAll (d (fe:*:ge))
= -- H2(eval,(:*:),(*)) on the left; derive of product on the right
  (eval fe * eval ge) : evalAll ( (fe:*:(d ge)) :+: ((d fe):*:ge))
= -- naming + Lemma1 below
  --   f = eval fe == head (evalAll fe)
  --   g = eval ge == head (evalAll ge)
  (f * g) : add (mul (evalAll fe)  (tail (evalAll ge))
                (mul (tail (evalAll fe)  (evalAll ge))
= -- naming continued:
  --   f:fs = evalAll fe;
  --   g:gs = evalAll ge;
  (f * g) : add (mul (f:fs)  (tail (g:gs))
                (mul (tail (f:fs)  (g:gs))
= -- def. of tail
  (f*g) : add (mul (f:fs) gs) (mul fs (g:gs))
= -- TODO make this true
  mul (f:fs) (g:gs)
= -- naming
  mul (evalAll fe) (evalAll ge)


Lemma1:

  evalAll (d (fe:*:ge))
= -- def. derivative of product
  evalAll ((fe :*: d ge) :+: (d fe :*: ge))
= -- H2(evalAll, (:+:), add)
  add (evalAll (fe :*: d ge)) (evalAll (d fe :*: ge))
= -- H2(evalAll,(:*:),mul) -- "co-induction"
  add (mul (evalAll    fe)  (evalAll (d ge))
      (mul (evalAll (d fe)  (evalAll    ge))
= -- H1(evalAll,d,tail)
  add (mul (evalAll    fe)  (tail (evalAll ge))
      (mul (tail (evalAll fe)  (evalAll    ge))

Thus, if we just make the "TODO" step true by definition of mul we
  have (informally) proved H2(evalAll,(:*:),mul).

\begin{code}
type MulFun = DS Func
mul :: MulFun->MulFun->MulFun
mul (f:fs) (g:gs) = (f*g) :  addDS  (mul (f:fs)  gs)
                                    (mul fs  (g:gs))

fromIntegerDS :: Integer -> MulFun
fromIntegerDS i = fromInteger i : repeat 0
\end{code}


Some testing code
\begin{code}
e1 :: FunExp -- 1 + 2x + x²
e1 = Const 1 :+: (Const 2 :*: Id) :+: (Id:*:Id)
es :: DS FunExp
es = e1 : map d es
fs :: DS Func
fs = evalAll e1

testfs :: [Func]
testfs = take 5 fs

testvs1 :: [REAL]
testvs1 = apply 1 testfs
testvs2 = apply 2 testfs

apply :: a -> [a -> b] -> [b]
apply c = map (\f->f c)
\end{code}

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

Next: the multiplication case (a longer argument for what we did above).

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
