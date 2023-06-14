-- Let |f x = x^2|. Prove or disprove |Exists op (H2(f,(+),op))|.

op : Nat -> Nat -> Nat

Start by unrolling the definition:
  H2(f,(+),op)
=
  Forall a, b. f (a+b) == op (f a) (f b)
=
  Forall a, b. (a+b)^2 == op (a^2) (b^2)
=
  Forall a, b. a^2+2*a*b+b^2 == op (a^2) (b^2)

This looks difficult in general because |op| cannot know the sign of
|a| or |b|.

Try with a=1, b=1:

  (1+1)^2 == op (1^2) (1^2)  =>  c = op 1 1 = 4

Try with a=-1, b=1:

  (-1+1)^2 == op ((-1)^2) (1^2) => c = op 1 1 = 0

But then we get 0 = 4 which is a contradiction.

Thus the claim has been disproven.


-- Let |g x = (-x, x^2)|. Prove or disprove |Exists op (H2(g,(-),op))|.

op : (Z,N) -> (Z,N) -> (Z,N)

Start by unrolling the definition:
  H2(g,(-),op)
=
  Forall a, b. g (a-b) == op (g a) (g b)
=
  Forall a, b. (-(a-b), (a-b)^2) == op (-a, a^2) (-b, b^2)
=
  Forall a, b. (-a-(-b), a^2-2*a*b+b^2) == op (-a, a^2) (-b, b^2)
= Forall a, b.
  let a' = -a, a2 = a^2
      b' = -b, b2 = b^2
  -- We have a'*b' = (-a)*(-b) = a*b
  in (a' - b', a2-2*a'*b'+b2) == op (a', a2) (b', b2)

Solved by defining our |op| to be
  op (a',a2) (b',b2) = (a'-b', a2 - 2*a'*b' + b2)
or even
  op (a',_)  (b',_)  = (a'-b', a'^2-2*a'*b'+b'^2)

Some testing code below (not part of the exam question).
\begin{code}
h2 h opA opB x y =  h (opA x y) == opB (h x) (h y)

type ZZ = Int
type NN = Int
pow x n = x^n

f :: ZZ -> NN
f x = pow x 2

g :: ZZ -> (ZZ, NN)
g x = (-x, pow x 2)

prop1 :: (NN -> NN -> NN) -> ZZ -> ZZ -> Bool
prop1 op = h2 f (+) op

prop2 :: ((ZZ, NN) -> (ZZ, NN) -> (ZZ, NN)) -> ZZ -> ZZ -> Bool
prop2 op = h2 g (-) op

-- there is no op for which prop1 op is always true

-- But there is an op for which prop2 op is always true
myop (a',a2) (b',b2) = (a'-b', a2 - 2*a'*b' + b2)
myprop2 = prop2 myop
-- Check a few cases:
check2 = and [myprop2 x y | x <- [-3,-2..3], y <- [-3,-2..3]]
\end{code}
