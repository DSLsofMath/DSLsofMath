P3 [30p]: Polynomial function composition

Consider the following code for polynomials represented as lists of coefficients:
\begin{code}
type P a = [a]
type S a = a -> a

eval :: Num a => P a -> S a
eval []      x = 0
eval (a:as)  x = a + x * eval as x

scaleP :: Num a => a -> P a -> P a
scaleP a = map (a*)

addP :: Num a => P a -> P a -> P a
addP  []      bs      = bs
addP  as      []      = as
addP  (a:as)  (b:bs)  = (a+b) : addP as bs

mulP :: Num a => P a -> P a -> P a
mulP  []      bs      = []
mulP  as      []      = []
mulP  (a:as)  (b:bs)  = (a*b) : addP (scaleP a bs) (mulP as (b:bs))
\end{code}

\begin{spec}
comP :: Num a => P a -> P a -> P a
comP  []      bs      = error "TODO: your task 1"
comP  (a:as)  []      = error "TODO: your task 2"
comP  (a:as)  bs      = error "TODO: your task 3"
\end{spec}

Spec. of |comP|:

H2(eval,comP,(.)) = Forall p (Forall q (eval (comP p q) == eval p . eval q))

a) [6p] As a warm-up, compute the special cases |cs1 = comP p q| and |cs2 = comP q p| where |eval p x = 1+x| and |eval q x = x^2-1|. (Thus |p=[1,1]| and |q=[-1,0,1]|.)
b) [4p] Use the specification to calculate task 1 above.
c) [4p] Use the specification to calculate task 2 above.
d) [7p] Use the specification to calculate |c = comP [a0,a1] [b0,b1]|
e) [9p] Use the specification to calculate the general case (task 3 above). You may use |H2(eval,addP,(+))|, |H2(eval,mulP,(*))|, and the spec.\ of |comP| for shorter lists.

================================================================
Start of suggested solutions:

The common technique used here is to start from the polynomial
function (eval someExpr) and calculate a chain of (motivated)
equalities until we reach eval someSolution after which we
conclude that a suitable polynomial representation (as a list
of coefficients) is someExpr == someSolution.

a) [6p] We calculate:

\begin{spec}
  eval cs1
= -- def. cs1
  eval (comP p q)
= -- spec. comP
  eval p . eval q
= -- spec. p and q
  (\x->1+x) . (\x->x^2-1)
= -- Def. of (.)
  \x->1+(x^2-1)
= -- Algebraic simplification
  \x->x^2
= -- def. of eval (a few times)
  eval [0,0,1]
\end{spec}

Thus cs1 = [0,0,1].

In a similar way (some steps omitted):

  eval b = (\x->x^2-1).(\x->1+x) = \x->(1+x)^2-1 = \x->2x+x^2 = eval [0,2,1]

Thus cs2 = [0,2,1].

----------------

b) [4p] We calculate

  eval (comP [] bs) x
= -- spec. comP
  (eval [] . eval bs) x
= -- Def. (.)
  eval [] (eval bs x)
= -- Def. eval []
  0
= -- Def. eval []
  eval [] x

Thus

  comP [] bs = []

----------------

c) [4p] We calculate (with p = (a:as))

  eval (comP p []) x
= -- Spec. of comP
  (eval p . eval []) x
= -- Def. of (.)
  eval p (eval [] x)
= -- Def. of eval []
  eval p 0
= -- Def. of p and of eval (a:as)
  a + 0*eval as 0
= -- Alg. simplification
  a
= -- Lemma Const (backwards)
  eval [a] x

Thus   comP (a:as) [] = [a]

Lemma Const:

  eval [a] x
= -- List notation
  eval (a:[]) x
= -- Def. of eval (a:as)
  a + x*eval [] x
= -- Def. of eval []
  a + x*0
= -- Alg. simplification
  a


----------------
d) [7p] We calculate:
  eval (comP [a0,a1] [b0,b1]) x
= -- Spec. of comP
  (eval [a0,a1] . eval [b0,b1]) x
= -- Def. of (.)
  eval [a0,a1] (eval [b0,b1] x)
= -- Lemma from exam script
  eval [a0,a1] (b0+x*b1)
= -- Lemma from exam script
  a0 + (b0+x*b1)*a1
= -- Simplification
  (a0 + b0*a1) + x*(b1*a1)
= -- Lemma from exam script (backwards)
  eval [a0 + b0*a1, b1*a1] x

Thus comP [a0,a1] [b0,b1] = [a0 + b0*a1, b1*a1]

----------------

e) [9p] We calculate

Let P(as) = eval (comP as bs) == (eval as) . (eval bs)
We fix bs and use induction over as to prove
  Forall as. P(as)
We have already proved the base case P([]) in b) above.

Now for the step case we assume the induction hypothesis IH=P(as) and
    prove P(a:as) for a free x by calculation from the
    RHS = ((eval (a:as)) . (eval bs)) x .

We start with the Step Lemma:

  (eval (a:as) . eval bs) x
= -- Def. of (.)
  eval (a:as) (eval bs x)
= -- Def. of eval (a:as)
  a + (eval bs x)*eval as (eval bs x)
= -- Lemma Const and def. of (.)
  eval [a] x + (eval bs x)*(eval as . eval bs) x
= -- Use IH
  eval [a] x + (eval bs x)*(eval (comP as bs) x)
= -- H2(eval,mulP,(*))
  eval [a] x + eval (mulP bs (comP as bs)) x
= -- H2(eval,addP,(+))
  eval (addP [a] (mulP bs (comP as bs))) x

Thus, if we define

  comP (a:as) bs = addP [a] (mulP bs (comP as bs))

we get the last step of the induction proof.
  LHS
= -- Local name
  eval (comP (a:as) bs) x
= -- Our new definition
  eval (addP [a] (mulP bs (comP as bs))) x
= -- Step Lemma (backwards)
  (eval (a:as) . eval bs) x
=
  RHS

QED.

================================================================

To sum up, we have calculated:

\begin{code}
comP :: Num a => P a -> P a -> P a
comP  []      q   = []
comP  (a:as)  []  = [a]
comP  (a:as)  bs  = addP [a] (mulP bs (comP as bs))
\end{code}
