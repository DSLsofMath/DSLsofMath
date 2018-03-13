2. Consider the following quote from ``A Companion to Analysis'' by Thomas Körner:

    Definition 1.43. Let |U| be an open set in |REAL|. We say that a
    function |f : U -> REAL| is differentiable at |t `elem` U| with
    derivative |f'(t)| if, given |epsilon > 0|, we can find a
    |delta(t,epsilon)>0| such that
    |(t-delta(t,epsilon),t+delta(t,epsilon)) included U| and

      absBar (frac (f(t+h)-f(t)) h - f'(t)) < epsilon

    whenever |0 < absBar h < delta(t,epsilon)|.


2a. Give types for the symbols |U|, |f|, |t|, |f'|, |epsilon|, |delta|.
2b. Define the corresponding logical predicate |DifferentiableAt(f,f',t)|.
2c. Define a function |delta2| that shows |DifferentiableAt(sq,tw,t)| where |sq x = x*x| and |tw x = 2*x|.
2d. Consider |P => Q| with |P = DifferentiableAt(f,f',t) & DifferentiableAt(g,g',t)| and |Q = DifferentiableAt(f+g,f'+g',t)|.
    How is |deltaplus| (needed in |Q|) defined in in terms of the
    |deltaf| and |deltag| (used in |P|).

----------------
2a.
\begin{code}
module P2 where
type REAL = Double
data U    -- or |U :: PowerSet REAL|
f :: U -> REAL
t :: U
f':: U -> REAL
type RPos = REAL -- should be real numbers > 0
epsilon :: RPos
delta :: (U,RPos)->RPos

(f,t,f',epsilon,delta) = undefined
\end{code}

2b.
\begin{spec}
DifferentiableAt(f,f',t) =
  Exists delta.
    Forall epsilon.
        (t-delta(t,epsilon),t+delta(t,epsilon)) `included` U
      &&
       Forall h.
         (0 < abs h < delta(t,epsilon)) =>
         abs ((f(t+h)-f(t))/h - f'(t)) < epsilon
\end{spec}

2c.

The key expression in the formula is

\begin{spec}
  (f(t+h)-f(t))/h - f'(t)
= let f=sq; f'=tw
  (sq(t+h)-sq(t))/h - tw(t)
= -- def. of sq, def. of tw
  ((t+h)*(t+h)-t*t)/h - 2*t
= -- simplification
  (t^2+2*t*h+h^2-t^2)/h - 2*t
= -- simplification
  2*t+h - 2*t
= -- simplification
  h
\end{spec}

Thus the inner part is

\begin{spec}
  Forall h. (0 < abs h < delta2(t,epsilon)) => (abs h < epsilon)
\end{spec}

We can see that this is guaranteed if we defined

\begin{code}
delta2 :: (U,RPos)->RPos
delta2 (t,epsilon) = epsilon
\end{code}

2d.
We want to find a function |dfg| such that

  Forall h.  (0 < abs h < dfg(t,epsilon)) =>
             abs (((f+g)(t+h)-(f+g)(t))/h - (f+g)'(t)) < epsilon

and we know there is a |df| such that
  Forall h.  (0 < abs h < df(t,epsilon)) =>
             abs ((f(t+h)-f(t))/h - f'(t)) < epsilon
and a |dg| such that
  Forall h.  (0 < abs h < dg(t,epsilon)) =>
             abs ((g(t+h)-g(t))/h - g'(t)) < epsilon


We compute with the core expression:

  ((f+g)(t+h)-(f+g)(t))/h - (f+g)'(t)
= -- def. of + for functions, derive for +
  (f(t+h)+g(t+h)-f(t)-g(t))/h - (f'(t)+g'(t))
= -- rearrange terms
  (f(t+h)-f(t))/h - f'(t)  +  (g(t+h)-g(t))/h - g'(t)
< -- [XX]: require that (abs h < df(t,epsilon/2)) and that (abs h < dg(t,epsilon/2))
  epsilon/2  +  epsilon/2
=
  epsilon

To fulfill [XX] we define

dfg(t,epsilon) = min (df(t,epsilon/2)) (dg(t,epsilon/2))

----------------
