Problem 1: Types

1a) [10p] Give the types of the symbols involved:
      f, u, g, x, (.) and |D|, where (|D f = f'|).

Step 1: invent types based on first use:

f   : U -> V   -- U and V are subsets of REAL
u   : U        -- Fixed by f(u)
g   : X -> U   -- X subset of REAL, U fixed by f(u)
x   : X        -- Fixed by g(x)
(.) : (U->V) -> (X->U) -> (X->V) -- fixed by (f.g)(x)
D   : (X->V) -> (X'->V) -- First arg. type fixed by D (f.g)
  where X' is a subset of X

Step 2: Collect other requirements:

The RHS of the definition of D (f.g) (x) is f'(g(x))*g'(x) (where we
expand the implicit use of (*)::R->R->R) and from there we can note

  D : (U->V) -> (U'->V)  -- from f'(g(x)), U' subset of U
  D : (X->U) -> (X'->U)  -- from g'(x)

We don't have to restrict more, but it is often convenient to restrict
the inputs to D so that D : (a->b) -> (a->b). (In this way we get rid
of the types X' and U'.) There are no other clear requirements so we
can sum up the result as follows:

f   : U -> V   -- U and V are subsets of REAL
u   : U
g   : X -> U   -- X subset of REAL
x   : X
(.) : (b->c) -> (a->b) -> (a->c)   -- polymorphic - here a=X, b=U, c=V
D   : (a->b) -> (a->b)             -- polymorphic - here used at different types

----------------

Alt. 2: (a bit too weak)

f   : R -> R
u   : R
g   : R -> R
x   : R
(.) : (R->R) -> (R->R) -> (R->R)
D   : (R->R) -> (R->R)

... there are more variants possible.

----------------------------------------------------------------

1b) [5p]

D (f . g) = ((D f) . g) * D g
