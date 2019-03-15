E4_1:

Notation:

  -- |h : A -> B| is a homomorphism from |Op : A->A->A| to |op : B->B->B|
  H2(h,Op,op)  =  Forall x, y.  h(Op x y) == op (h x) (h y)

  -- |h : A -> B| is a homomorphism from |F : A -> A| to |f : B -> B|
  H1(h,F,f)    =  Forall x.  h(F x) == f (h x)

  -- |h : A -> B| is a homomorphism from |E : A| to |e : B|
  H0(h,E,e)    =  h E == e

Prove or disprove the following claims:




4.1a) H2((2*),(+),(+))
Typing: let (2*) :: X -> Y where X and Y are subsets of Integer

Note that typing this in Haskell is optional - it is a pure maths / logic exercise.

  H2((2*),(+),(+))
= -- Def. of H2
  Forall x, y. (2*)((+) x y) == (+) ((2*) x) ((2*) y)
= -- Expand the notation: (2*) c = 2*c; (+) a b = a+b
  Forall x, y.  2*(x+y) == (2*x)+(2*y)
= -- Distributivity of * over +
  Forall x, y.  (2*x)+(2*y) == (2*x)+(2*y)
= -- a == a holds trivially
  True

Thus 4.1a is True.

----------------
4.1b) H2((2*),(*),(*))

Typing: let (2*) :: X -> Y as above

  H2((2*),(*),(*))
= -- Def. of H2
  Forall x, y. (2*)((*) x y) == (*) ((2*) x) ((2*) y)
= -- Expand the notation
  Forall x, y.  2*(x*y) == (2*x)*(2*y)
= -- Simplification
  Forall x, y.  2*x*y == 4*x*y

This does not look True but strictly speaking we are not done.  First,
the types matter: if x is restrited to be in X = {0} then

  2*x*y==4*x*y==0

and the homomorphism property holds. But let's assume we have at least
1 as well in X (the domain of x). Then we can prove the negation:

  not H2((2*),(*),(*))
= -- simplification above
  not (Forall x, y.  2*x*y == 4*x*y )
= -- push not through Forall, twice
  Exists x, y. not (2*x*y == 4*x*y)
= -- def. of /=
  Exists x, y.      2*x*y /= 4*x*y

To prove this we only need to provide some concrete evidence (values
for x and y). It is enough to pick x=y=1 - then

  2*x*y == 2 /= 4 == 4*x*y.

Thus 4.1b is False.

----------------
4.1c) H2(exp,(+),(*))
Typing: exp : REAL -> RPos  -- exp x > 0 for all real x

  H2(exp,(+),(*))
= -- Expand H2
  Forall x, y. exp((+) x y) == (*) (exp x) (exp y)
= -- Expand notation
  Forall x, y. exp(x+y) == (exp x)*(exp y)
= -- exponentional law
  True

Thus 4.1c is True.

----------------
4.1d) H2(eval',(:+:),(+))
Typing: eval' : FunExp -> Func   where Func = REAL -> REAL
thus this (+) : Func -> Func -> Func

We know that
  eval' = eval . derive
which means that
  eval' e = (eval . derive) e = eval (derive e)


  H2(eval',(:+:),(+))
= -- Expand H2, expand notation. Note that x, y :: FunExp.
  Forall x, y.  eval'(x:+:y) == (eval' x)+(eval' y)
= -- expand eval'
  Forall x, y.  eval (derive (x:+:y)) == eval (derive x) + eval (derive y)
= -- Def. of derive (f:+:g) = derive f :+: derive g
  Forall x, y.  eval ((derive x):+:(derive y)) == eval (derive x) + eval (derive y)
= -- Def. of eval (f:+:g) = eval f + eval g
  Forall x, y.  eval (derive x) + eval (derive y) == eval (derive x) + eval (derive y)
= -- By Forall e. e == e
  True

Thus 4.1d is True.

----------------
4.1e) H1(sqrt,(4*),(2*))
Typing: sqrt : RPos -> RPos

  H1(sqrt,(4*),(2*))
= -- Expand H1
  Forall x.  sqrt((4*) x) == (2*) (sqrt x)
= -- Simplify
  Forall x.  2*(sqrt x) == 2*(sqrt x)
= -- equality is reflexive
  True

Thus 4.1e is True

----------------
4.1f) Exists f. H1(f,(2*).(1+),(1+).(2*))
Typing: we can take f : Integer -> Integer

  Exists f. H1(f,(2*).(1+),(1+).(2*))
= -- Expand H1
  Exists f. Forall x.  f(((2*).(1+)) x) == ((1+).(2*)) (f x)
= -- Def. of (.) and expanding notation
  Exists f. Forall x.  f (2*(1+x)) == 1+2*(f x)

Here we need to do some exploring. We could start by guessing

  f = const c

Then we get
  const c (2*(1+x)) == 1+2*(const c x)
= -- Def. of const
  c == 1+2*c
= -- subtract c and 1 from both sides
  -1 == c

Lucky guess: if we pick f = const (-1) our equation is satisfied for all x.

Thus 4.1f is True and f = const (-1) is concrete evidence for that.

Not that we don't need to solve the more general question "what is the
set F of functions for which H1(f,(2*).(1+),(1+).(2*)) is True", we
only need to show that F has at least one element.

----------------
Exploring further (just for fun):

  Could f = lin k m = \x -> k*x+m  work?

We calculate:

  f (2*(1+x)) == 1+2*(f x)
= -- Expand lin k m
  k*(2*(1+x))+m == 1+2*(k*x+m)
= -- collect terms on each side
  (2*k+m)+2*k*x == (1+2*m)+2*k*x
= -- subtract 2*k*x and m
  2*k == 1+m

Thus, we have a whole family of solutions: m=2*k-1 for all k.

Note that this includes k=0 and m=-1 which we found above, but also
  f x = x+1
  f x = 2*x+3
  etc.
