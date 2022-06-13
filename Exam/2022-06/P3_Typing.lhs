(a) Types of symbols
\begin{code}
type REAL = Double
type RPos = REAL -- with side-condition >=0
type Index = Int
type Vec r = Index -> r
data Set r -- abstract type for a set of values of type r

nI    :: (Vec REAL, RPos) -> Set (Vec REAL)
x     :: Vec REAL
xst   :: Vec REAL
delta :: RPos -- also required to be >0
f     :: Vec REAL -> REAL
norm  :: Vec REAL -> RPos

nI (xst, delta) = error "TODO" -- not part of the question
(x, xst, delta, f) = undefined

n = 3 -- just for testing
norm v = sqrt (sum (map v [1..n]))
\end{code}

----------------
(b) FOL predicate

LocMin(f,xst) = Exists delta > 0. Forall x in I(xst,delta).  x/=xst  =>  f(x) > f(xst)

All scopes extend to the end of the definition

f and xst are bound on the LHS;
delta is bound in Exists;
x is bound in Forall

(I is defined earlier, as well as (/=) and (>).)

----------------
(c)

We look for a proof p : P
where P = LocMin(norm,zero) = Exists delta > 0 . Forall x in I(zero,delta) . x /= zero => sin(norm(x)) > sin(norm(zero)) = 0

We have I(zero, delta) = {x | norm (x-zero) < delta} = {x | norm x < delta}

We have (norm(v))^2 = sum (v_i)^2 = 0 iff all v_i=0

The proof p is a pair of a delta - which we can take as = pi - and a
function foo from an x in I(zero, delta) to a proof of the property

  x /= zero => f(x)>f(zero)

we know f(x) = sin(norm(x))
and we know that for x in I(zero, pi), norm(x)<pi.
Further, as x/=0 we also know 0<norm(x).
We also know that sin y > 0 for all 0<y<pi.
Thus f(x)=sin(norm(x))>0=f(zero).
QED.

---- (d)
We first simplify the statement:

  not(LocMin(g,zero))
= -- Def.
  not(Exists delta > 0. Forall (x,y) in I(zero,delta) . (x,y) /= zero => g(x,y) > g(0,0) = 0)
= -- not-Exists
  Forall delta > 0. not (Forall (x,y) in I(zero,delta) . (x,y) /= zero => x*y > 0)
= -- not-Forall
  Forall delta > 0. Exists (x,y) in I(zero,delta) . not ((x,y) /= zero => x*y > 0)
= -- not(A=>B) == not (not A || B) = A && not B
  Forall delta > 0. Exists (x,y) in I(zero,delta) . (x,y) /= zero && x*y <= 0

Thus a proof is a function bar from delta > 0 to some pair (x,y)/=0
which is in I(zero,delta), and for which x*y <= 0.

Let bar delta = (delta/2, 0). Then x*y=delta/2*0=0<=0.

And
  norm (delta/2,0)
= sqrt ((delta/2)^2+0^2)
= abs (delta/2)
= delta/2
< delta.

Thus bar delta is always in I(zero, delta) and also /=0 (because delta >0).
QED.
