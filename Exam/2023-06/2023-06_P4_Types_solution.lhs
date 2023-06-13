Typing maths: Uniform convergence


Consider the following (lightly edited) quote from Conway [2017]:

    If {f_n} is a sequence of functions from a subset |X| of |RR| into
    |RR|, say that {f_n} *converges uniformly* to a function |g| if
    for every |eps > 0| there is an |N| such that |abs (f_n(x) - g(x))
    < eps| for all |x| in |X| and all |n >= N|.  In symbols this is
    written as |f_n ->_u g| on |X|.

+ [7p] Define the first-order logic predicate |ConUni(X,f,g)|
    encoding the property of uniform convergence |f_n ->_u g| on |X|.

ConUni(X,f,g) =
  Forall eps : REAL+. Exists N : Nat.
  Forall x : X. Forall n >= N. 
  abs (f_n(x) - g(x)) < eps

Note that |N| is chosen before |x|, that is the essence of "uniform"
in the definition of convergence. It is a stronger requirement than
just convergence for each |x|.

+ [8p] Give the types of |X|, |n|, |f|, |g|, |eps|, and
    |N|. Explain your reasoning.

X : Pow REAL   -- |X| is a subset of the reals, thus an element of the set
               -- of all subsets, Pow REAL
n : Nat        -- Implicit in the definition of "sequence" is a natural number index |n|.

f : Nat -> X -> REAL  -- We can treat the subscript |n| as the first
                      -- function argument and |x : X| as the second.

g :        X -> REAL  -- to match the type of |f_n|

eps : REAL+           -- Let |REAL+ = {r : REAL | r > 0}|. In the text: 
                      -- implicitly a real number, explicitly >0.

N : Nat               -- to match with the type of |n|

+ [5p] Simplify the predicate |not ConUni(X,f,g)| by pushing
    the negation through all the way.

  not ConUni(X,f,g)
= -- Def.
  not (Forall eps : REAL+. Exists N : Nat.
       Forall x : X. Forall n >= N. 
       abs (f_n(x) - g(x)) < eps)
= -- de Morgan for Forall, Exists.
  Exists eps : REAL+. Forall N : Nat.
  Exists x : X. Exists n >= N. 
  not (abs (f_n(x) - g(x)) < eps))
= -- not (a<b) = a>=b
  Exists eps : REAL+. Forall N : Nat.
  Exists x : X. Exists n >= N. 
  abs (f_n(x) - g(x)) >= eps

+ [10p] Prove |not ConUni(REAL, sq, zero)| where
    |sq n x = pow (x/n) 2| and |zero x = 0|.

Expand the definition + simplify:
P = not ConUni(REAL, sq, zero)
  = Exists eps : REAL+. Forall N : Nat.
    Exists x : X. Exists n >= N. 
    abs ((x/n)^2) >= eps

A proof p : P can be seen as a pair of an eps and a function from N.
We can try with eps=1 to see where that leads us.

p = (1, foo)

Now we need the function foo to return a triple of an x, an n>=N and a
proof that |abs ((x/n)^2) >= eps = 1|. We can choose x=n=N to satisfy
that requirement:

foo N = (N, (N, obvious))

where |obvious| is the proof that |abs ((N/N)^2) >= 1|.

(Note that, for every fixed |x|, the limit of |sq n x| as |n -> inf|
is zero. But in the definition of |ConUni| the |x| is not fixed, and
this gives us the possibility to choose an |x| as big as |n| which
means |sq n| is not uniformly convergent to the zero function.)

