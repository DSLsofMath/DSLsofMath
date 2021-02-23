Higher-order Derivatives and their Applications

Yet another type for representing functions.

\begin{code}
module DSLsofMath.Live_6_1 where
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..))
import DSLsofMath.FunExp    -- Syntax for functions + derivatives
import DSLsofMath.Simplify  -- Algebraic simplification
import DSLsofMath.Algebra   -- Numeric type classes
import DSLsofMath.W05 (Poly(..),PowerSeries, evalPS)
import DSLsofMath.DerAll hiding
  (constDS, addDS, negateDS, mulDS, recipDS)

d :: FunExp -> FunExp
d = simplify . derive

type Func = REAL->REAL
-- eval :: FunExp -> Func
-- type DS = []

-- Not executable in general (due to infinite lists)
propDerAll :: FunExp -> Bool
propDerAll e =   head (derAll e) == e
             &&  tail (derAll e) == derAll (d e)


evalAll :: Transcendental a => FunExp -> DS (a->a)
evalAll = map eval . derAll

-- propEvalAll e =  evalAll e == eval e : evalAll (d e)

-- testing
si :: Transcendental a => DS (a->a)
si = evalAll (Sin X)


type Maclaurin = DS
evalToM :: Transcendental a => FunExp -> Maclaurin a
evalToM = apply zero . evalAll

evalFromM :: Field a => Int -> Maclaurin a -> (a->a)
evalFromM n = evalPS n . fromMaclaurin

fromMaclaurin :: Field a => Maclaurin a -> PowerSeries a
fromMaclaurin as = Poly (zipWith (/) as factorials)
-- divFact

toMaclaurin :: Ring a => PowerSeries a -> Maclaurin a
toMaclaurin (Poly as) = zipWith (*) factorials as
-- mulFact

factorials :: Ring a => [a]
factorials = factorialsFrom zero one
factorialsFrom :: Ring a => a -> a -> [a]
factorialsFrom n factn = factn : factorialsFrom n' (factn * n')
  where n' = n+one
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





derAll (Mul f g) = mul (derAll f) (derAll g)

\begin{code}
type MulFun = DS Func

mul :: Ring a => DS a -> DS a -> DS a
mul fs@(f:fs') gs@(g:gs') = m:ms'
  where  m   = f*g
         ms' = (mul fs' gs) + (mul fs gs')   -- :: DS a
mul []         gs  = []
mul fs         []  = []

-- (+) :: DS a -> DS a -> DS a
-- (+) = zipWithLonger (+)


  -- f    = head fs;
  -- fs'  = tail fs = derAll (derive f)

constDS :: Additive a => a -> DS a
constDS c = c : repeat zero
\end{code}


Some testing code
\begin{code}
e1 :: FunExp -- 1 + 2x + x²
e1 = Const 1 :+: (Const 2 :*: X) :+: (X:*:X)
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

xDS :: Ring a => DS a
xDS = zero:constDS one
\end{code}

\begin{spec}
instance Additive a => Additive [a] where
  (+) = addDS; zero = constDS zero
instance Ring a => Multiplicative [a] where
  (*) = mulDS; one = constDS one
instance AddGroup a => AddGroup [a] where
  negate = negateDS
instance Field a => MulGroup [a] where
  recip = recipDS
\end{spec}

\begin{code}
negateDS :: AddGroup a => DS a -> DS a
negateDS = map negate


recipDS :: Field a => DS a -> DS a
recipDS (a:as) = b:bs
  where  b   = recip a
         bs  = negate (b:bs)*(b:bs)*as
-- D (recip f) = -(recip f)^2 * D f
-- H1(eval,tail,D)

ones :: Ring a => DS a
ones = one:ones

scale :: Multiplicative a => a -> DS a -> DS a
scale c = map (c*)
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

derDS :: DS a -> DS a
derDS = tail

addDS :: Additive a => DS a -> DS a -> DS a
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
\begin{spec}
mulDS :: Ring a => DS a -> DS a -> DS a
mulDS fs gs = h : t
  where  h = (head fs) * (head gs)
         t = ((derDS fs)*gs) + (fs*(derDS gs))
\end{spec}
or equivalently:
\begin{code}
mulDS :: Ring a => DS a -> DS a -> DS a
mulDS fs@(f:fs') gs@(g:gs') =
  (f*g) : addDS (mulDS fs' gs) (mulDS fs gs')

\end{code}
