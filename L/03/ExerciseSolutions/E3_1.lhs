# Question text:

To get a feeling for the Lagrange equations, let

  L(t, q, v) = m*v^2/2 - m*g*q

compute |expand w|, perform the derivatives and check if the equation is satisfied for

  w1 = id
  w2 = sin
  w3 = (q0-).(g*).(/2).(^2)

----------------
From the lecture notes:

  L  :  (T, Q, V)  ->  REAL
  Lagrange(L, w) =  D (D3 L . expand w) == D2 L . expand w
  expand : (T -> Q) -> (T -> (T, Q, V))
  expand w t  =  (t, w t, D w t)

and

  f . g = \x -> f (g x)

----

Solution (in quite a high level of detail):

We compute the parts step by step, starting with the L because it is
the same for all three paths. Note that |D3 L| is a function of the
same type as |L|. We can write it using an anonymous function:

  D3 L
= {- As a function of v, the first term is quadratic and the second constant. -}
  \(t, q, v) -> 2*m*v/2
= {- Simplify -}
  \(t, q, v) -> m*v

Next we compute |D2 L| - also a function of the same type as |L|:

  D2 L
= {- As a function of q, the first term is constant and the second linear. -}
  \(t, q, v) -> -m*g

Lemma 1: We can do one more step of computation with is common: we
have |F . expand w| in two places, where |F : (T, Q, V) -> REAL|.

  F . expand w
= {- Def. of |.| -}
  \t -> F (expand w t)
= {- Def. of |expand| -}
  \t -> F (t, w t, D w t)
= {- Assume |F = \(t, q, v) -> b| -}
  \t -> (\(t, q, v) -> b|) (t, w t, D w t)
= {- Simplify -}
  \t -> let (t, q, v) = (t, w t, D w t)
        in b
= {- Simplify -}
  \t -> let  q = w t
             v = D w t
        in b

In our two uses we thus get Lemma 2:

  D3 L . expand w
= {- Lemma 1 with |F = D3 L|, thus |b = m*v| -}
  \t -> let  q = w t
             v = D w t
        in m*v
= {- Substitute -}
  \t -> m * D w t
= {- Def. of |*| for functions -}
  const m  *  D w

and Lemma 3:

  D2 L . expand w
= {- Lemma 1 with |F = D2 L|, thus |b = -m*g| -}
  \t -> let  q = w t
             v = D w t
        in -m*g
= {- Substitute -}
  \t -> -m*g

Note that this means the RHS is independent of the path |w|:

  RHS = const (-m*g)

Finally we can simplify the whole LHS a bit (Lemma 4):

  LHS
= {- Def. -}
  D (D3 L . expand w)
= {- Lemma 2 -}
  D (const m  *  D w)
= {- Def. of D for |*| and |const| -}
  const m * D (D w)

Thus, for our L the Lagrange predicate becomes

  Lagrange(L, w) =  const m * D (D w) == const (-m*g)

Now we are ready to take one path at a time.

Path 1:
  w1 = id
  D w1 = const 1
  D (D w1) = const 0

Thus we get

  Lagrange(L, w1)
= {- By def. -}
  const m * D (D w1) == const (-m*g)
= {- D computation -}
  const m * const 0 == const (-m*g)
= {- Simplify -}
  const 0 == const (-m*g)
=
  (m==0) OR (g==0)

Thus, |w1=id| is a path if the particle is massless, or the
  gravitational field is zero.

Path 2:
  w2 = sin
  D w2 = cos
  D (D w2) = -sin

Thus we get

  Lagrange(L, w2)
= {- By def. -}
  const m * D (D w2) == const (-m*g)
= {- By def. -}
  const m * (-sin) == const (-m*g)
= {- Simplify -}
  (m == 0) OR (sin == const g)
= {- Simplify -}
  m == 0

Thus, |w2=sin| is a path if the particle is massless.

Path 3:
  w3 = (q0-).(g*).(/2).(^2)
  D w3 = ((-g)*)             -- see below for motivation
  D (D w3) = const (-g)

Thus we get

  Lagrange(L, w2)
= {- By def. -}
  const m * D (D w2) == const (-m*g)
= {- By def. -}
  const m * const (-g) == const (-m*g)
= {- Simplify -}
  const (-m*g) == const (-m*g)
= {- Simplify -}
  True

Thus |w3| is a path of the system for any |m| and |g|.

----------------

Below the line are two ways to compute the derivative of |w3|:

Way 1:
  w3 t = ((q0-).(g*).(/2).(^2)) t = q0-g*t^2/2
  D w3 t = -2*g*t/2 = -g*t
  D (D w3) t = -g
or equivalently:
  D (D w3) = const (-g)

Way 2: Use the chain rule

  D (f.g) = (D f . g) * D g

and some helpers:

  D (q0-) = const (-1)
  D (g*)  = const g
  D (/2)  = const (1/2)
  D (^2)  = (2*)

Const lemma: if D f = const c then

  D (f.g) == (D f . g) * D g == (const c . g) * D g == const c * D g

Thus

  D ((q0-).(g*).(/2).(^2))
=
  const (-1) * D ((g*).(/2).(^2))
=
  const (-1) * const g * D ((/2).(^2))
=
  const (-g) * const (1/2) * D (^2)
=
  const (-g/2) * (2*)
=
  ((-g)*)

and finally

  D (D w3) = const (-g)
