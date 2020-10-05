\section{Elements of Linear Algebra}
\label{sec:LinAlg}
\begin{code}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RebindableSyntax #-}
module DSLsofMath.W07 where
import DSLsofMath.Algebra
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..),sum)
import Data.List(nub)
type REAL = Double
\end{code}

Often, especially in engineering textbooks, one encounters the
following definition: a vector is an \(n+1\)-tuple of real or complex
numbers, arranged as a column:
%
\[v = \colvec{v}\]
%
Other times, this is supplemented by the definition of a row vector:
%
\[v = \rowvec{v}\]
%

The |vi|s are real or complex numbers, or, more generally, elements of
a \emph{field}.\jp{track down the first time we use fields and define |Field| there.}
%

However, following our theme, we will first characterize vectors
algebraically.  From this perpective a \emph{vector space} is an
algebraic structure that captures a set of vectors, with zero,
a commutative addition, and scaling by a set of scalars (i.e., elements of the
field). In terms of typeclasses, we can characterize this structure as
follows:
\begin{spec}
class (Field s, AddGroup v) => VectorSpace v s where
  (*^) :: s -> v -> v
\end{spec}
Additionally, vector scaling (|s *^|) must be a homomorphism over (from and to)
the additive group structure of |v|:
\begin{spec}
  s *^ (a + b)     = s *^ a + s *^ b
  s *^ zero        = zero
  s *^ (negate a)  = negate (s *^ a)
\end{spec}
And, on the other side, |(*^ a)| is a homomorphism from the additive group structure of |s| to the group structure of |v|:
\begin{spec}
  (s + t) *^ a     = s *^ a + t *^ a
  zero *^ a        = zero
  negate s *^ a    = negate (s *^ a)
\end{spec}
The multiplicative structure of |s| interacts with |(*^ a)| as follows:
\begin{spec}
  one *^ a        = a
  (s * t) *^ a    = s *^ (t *^ a)
\end{spec}
\footnote{Traditionally some of the above laws are omitted because they are consequences of other laws.}

An important consequence of their algebraic structure is
that vectors can be expressed as a simple sort of
combination of other special vectors.  More precisely, we can \emph{uniquely}
represent any vector |v| in the space in terms of a fixed set of
\emph{basis} vectors |{b0, ..., bn}| which cover the whole space and are \emph{linearly
  independent}:

\begin{spec}
  (s0 *^ b0 + ... + sn *^ bn = 0) <=> (s0 = ... = sn = 0)
\end{spec}
One can prove the uniqueness of representation as follows.
\begin{proof}
  Assume two representations of |v|, given by |si| and |ti|. The difference of those representations is given by |si-ti|. But because they represent the same vector,
  they must be equal to the zero vector:  |(s0-t0) *^ b0 + ... + (sn-tn) *^ bn = 0|.
By the basis being linearly independent, we find |si-ti=0|, and |si=ti|.
\end{proof}

This representation is what justifies the introduction of vectors as
columns (or rows) of numbers. (According to our red thread, this
representation is akin to the notion of ``syntax''.) Indeed, we can
define:
\[v = \colvec{v} = |v0 *^| \colveccc{1 \\ 0 \\ \vdots \\ 0} +
                   |v1 *^| \colveccc{0 \\ 1 \\ \vdots \\ 0} + \cdots +
                   |vn *^| \colveccc{0 \\ 0 \\ \vdots \\ 1}
\]

So, for our column vectors, we can define the operations as follows:
%
\[v + w = \colvec{v} + \colvec{w} = \colvecc{v_0 + w_0}{v_n + w_n}\]
%
\[ |s *^ v| = \colvecc{s*v_0}{s*v_n}\]
%
In the following we denote by
%
\[e_k = \colveccc{0\\\vdots\\0\\1 \makebox[0pt][l]{\qquad $\leftarrow$ position $k$} \\0\\\vdots\\0}\]
%
the canonical base vectors, ie. the vector that is everywhere |0|
except at position |k|, where it is |1|, so that |v = v0 *^ e0 + ... +
vn *^ en|. This formula maps the syntax (coefficients) to the
semantics (a vector).

\subsection{Representing vectors as functions}
In what follows we will systematically use the represention of vectors
as a linear combination of basis vectors.
%
There is a temptation to model the corresponding set of coefficients
as a lists or tuples, but a more general (and conceptually simpler)
way is to view them as \emph{functions} from a set of indices |G|:
%
\begin{code}
newtype Vector s g    = V (g -> s) deriving (Additive,AddGroup)
\end{code}
%*TODO Perhaps explain "deriving ... here"

We define right away the notation |a ! i| for the coefficient of the base vector |e i|, as follows:
\begin{code}
infix 9 !
(!) :: Vector s g -> g -> s
V f ! i = f i
\end{code}

As discussed, the |S| parameter in |Vector S| has to be a field (|REAL|,
or |Complex|, or |Zn|, etc.) for values of type |Vector S G| to
represent elements of a vector space.

The cardinality of |G|, which we sometimes denote |card G|, is number
of basis vectors, and thus the dimension of the vector space.  Often
|G| is finite and in the examples so far we
have used indices from \(G = \{0, \ldots, n\}\).
%
Thus the dimension of the space
would be \(n+1\).

In Haskell finiteness of |G| can be captured by the conjunction of |Bounded| (there is a minimum and a maximum element in |G|)
and |Enum|erable (there is a notion of enumeration from a given element of |G|) and |Eq|. Hence, the list of all elements of |G| can be extracted:
\begin{code}
type Finite g = (Bounded g, Enum g, Eq g)
finiteDomain :: Finite a => [a]
finiteDomain = [minBound..maxBound]
\end{code}

We know from the previous lectures that if |S| is an instance of
|AddGroup| then so is |G -> S|, with the pointwise definitions.
However, multiplication of vectors does not in general work
pointwise. In fact, attempting to lift multiplication from the |Multiplicative| class would give a homogenous
multiplication operator |(*) :: v -> v -> v|, but such an operator is
not part of the definition of vector spaces. Consequently, vector
spaces are \emph{not} rings.

Instead, the scaling operator |(*^) :: s -> v -> v|, is inhomogenous:
the first argument is a scalar and the second one is a vector. For our
representation it can be defined as follows:

\begin{code}
infixr 7 *^
(*^) :: Multiplicative s => s -> Vector s g -> Vector s g
s *^ V a = V $ \i -> s * (a i)
\end{code}
% $ emacs

The canonical basis vectors are given by
%
\begin{code}
e :: (Eq g, Ring s) => g -> Vector s g
e i = V (\j -> i `is` j)
\end{code}
%
In linear algebra textbooks, the function |is| is often referred to as
the Kronecker-delta function and |is i j| is written $\delta_{i,j}$.
\begin{code}
is :: (Eq g, Ring s) => g -> g -> s
is i j = if i == j then 1 else 0
\end{code}
It is 1 if its arguments are equal and 0 otherwise. Thus |e i| has
zeros everywhere, except at position |j| where it has a 1.

This way, every |v : G -> S|
is a linear combination of vectors |e i|:
%
\begin{spec}
    v =  v 0 *^ e 0 + ... + v n *^ e n
\end{spec}

\subsection{Linear transformations}

As we have seen in earlier chapters, morphisms between structures are often important.
%
Vector spaces are no different: if we have two vector spaces |Vector S
G| and |Vector S G'| for the same set of scalars |S|, we can study
functions |f : Vector S G -> Vector S G'|:
%
\begin{spec}
f v  =  f (v 0 *^ e 0 + ... + v n *^ e n)
\end{spec}
% that
It is particularly interesting to study vector-space homomorphisms,
which are more commonly called ``linear maps'' (to avoid unnecessary
confusion with the Haskell |map| function we will refer to them by the
slightly less common name ``linear transformation''.).  The function
|f| is a linear transformation if it maps the operations in |Vector S
G| into operations in |Vector S G'| as follows:
\begin{spec}
f (u + v) =  f u + f v
f (s *^ u) =  s *^ f u
\end{spec}
Since $v = (v 0 *^ e 0 + ... + v n *^ e n)$, we also have:
%
\begin{spec}
f v =  f (v 0 *^ e 0 + ... + v n *^ e n) = v 0 *^ f (e 0) + ... + v n *^ f (e n)
\end{spec}
%
But this means that we can determine the values of
%
|f : Vector S G -> Vector S G'|
%
from just the values of
%
|f . e : G -> Vector S G'|,
%
which has a much smaller domain.
%
Let |m = f . e|.
%
Then
%
\begin{spec}
f v =  v 0 *^ m 0 + ... + v n *^ m n
\end{spec}
%
Each of the |m k| is a |Vector S G'|, as is the resulting |f v|.
%
We have
%
\begin{spec}
  f v g'                                             = {- as above -}

  (v 0 *^ m 0 + ... + v n *^ m n) g'                 = {- Def. of |(*^)| and |(+)| -}

  v 0 * m 0 g' + ... + v n * m n g'                  = {- using |sum|, and |(*)| commutative -}

  sum [m j g' * v j | j <- [0 .. n]]
\end{spec}
That is, it suffices to know the behaviour of |f| on the basis vectors
to know its behaviour on the whole vector space.

%
It is enlightening to compare the above sum with the standard vector-matrix multiplication.
Let us define |M| as follows:
%
\begin{spec}
M = [m 0 | ... | m n]     -- where |m : G -> Vector S G'|
\end{spec}
%
That is, the columns of |M| are the images of the canonical base vectors |e i|
through |f| (or, in other words, the columns of |M| are |f (e i)|).
%
Every |m k| has |card G'| elements, and it has become standard
to write |M i j| to mean the |i|th element of the |j|th column, i.e., |M i
j = m j i|, so that, with the usual matrix-vector multiplication
%
\begin{spec}
  (M * v) i = sum [M i j * v j | j <- [0 .. n]]
\end{spec}
%
therefore, one has
%
\begin{spec}
  (M * v) i                                   = -- by def. of matrix-vector multiplication
  sum [M i j * v j | j <- [0 .. n]]           = -- by def. of M i j
  sum [m j i * v j | j <- [0 .. n]]           = -- by |f v g' = sum [m j g' * v j || j <- [0 .. n]]| with |g' = i|
  f v i
\end{spec}
%
If we take |Matrix| to be just a synonym for functions of type |G -> Vector S G'|:
%
\begin{code}
type Matrix s g g' = g' -> Vector s g
\end{code}
%
then we can implement matrix-vector multiplication as follows:
%
\begin{code}
mulMV ::  (Finite g, Ring s) => Matrix s g g'  ->  Vector s g  ->  Vector s g'
mulMV m v  = V (\i -> sum [m i!j  *  v!j | j <- finiteDomain])
\end{code}
%

%*TODO:
% I think we might end up with a mixture of definitions given in terms of
% |sum map| and definitions given in terms of list comprehension, at the
% end we should perhaps clean up and stick to one notation. At the
% specification/conceptual level we should perhaps stick to the usual
% $\sum$ notation.

Note that in the terminology of the earlier chapters we can see |Matrix
s g g'| as a type of syntax and the linear transformation (of type
|Vector S G -> Vector S G'|) as semantics.
%
With this view, |mulMV| is just another |eval :: Syntax -> Semantics|.
%

%
\begin{example}
  Consider the multiplication of a matrix with a basis vector:
\begin{spec}
(M * e k) i = sum [M i j * e k j | j <- [0 .. n]] = sum [M i k] = M i k
\end{spec}
\end{example}
%
i.e., |e k| extracts the |k|th column from |M| (hence the notation
``e'' for ``extract'').

We have seen how a linear transformation |f| can be fully described by a matrix
of scalars, |M|.
%
Similarly, in the opposite direction, given an arbitrary matrix |M|,
we can define
%
\begin{spec}
f v = M * v
\end{spec}
%
and obtain a linear transformation |f = (M*)|.
%
Moreover |((M*) . e) g g' = M g' g|, i.e., the matrix constructed as
above for |f| is precisely |M|.

Exercise~\ref{exc:Mstarcompose}: compute |((M*) . e ) g g'|.

Therefore, every linear transformation is of the form |(M*)| and every
|(M*)| is a linear transformation. There is a bijection between these two sets.
%
Matrix-matrix multiplication is defined in order to ensure associativity (note here the overloading of the operator |*|):
%
\begin{spec}
(M' * M) * v = M' * (M * v)
\end{spec}
%
that is
%
\begin{spec}
((M' * M)*) = (M' *) . (M *)
\end{spec}

Exercise~\ref{exc:Mstarhomomorphismcompose}: work this out in detail.
\jp{Note that |Matrix| form a category with mulMV being the composition and |e| as the identity.}

Exercise~\ref{exc:MMmultAssoc}: show that matrix-matrix multiplication is associative.
\jp{Not sure what the difference is from the previous exercise.}

A simple vector space is obtained for |G = ()|, the
singleton index set.
%
In this case, the vectors |s : () -> S| are functions that can take
exactly one value as argument, therefore they have exactly one value: |s ()|, so
they are isomorphic with |S|.
%
But, for any |v : G -> S|, we have a function |fv : G -> (() -> S)|,
namely
%
\begin{spec}
fv g () = v g
\end{spec}
%
|fv| is similar to our |m| function above.
%
The associated matrix is
%
\begin{spec}
M = [m 0 | ... | m n] = [fv 0 | ... | fv n]
\end{spec}
%
having |n+1| columns (the dimension of |Vector G|) and one row
(dimension of |Vector ()|).
%
Let |w :: Vector S G|:
%
\begin{spec}
M * w = w 0 *^ fv 0 + ... + w n *^ fv n
\end{spec}
%
|M * v| and each of the |fv k| are ``almost scalars'': functions of
type |() -> S|, thus, the only component of |M * w| is
%
\begin{spec}
(M * w) () = w 0 * fv 0 () + ... + w n * fv n () = w 0 * v 0 + ... + w n * v n
\end{spec}
%
i.e., the scalar product of the vectors |v| and |w|.

\textbf{Remark:} We have not discussed the geometrical point of view.
%
\lnOnly{For the connection between matrices, linear transformations, and
geometry, I warmly recommend binge-watching the ``Essence of linear
algebra'' videos on youtube (start here:
\url{https://www.youtube.com/watch?v=kjBOesZCoqc}).
}

%*TODO: Perhaps it would be interesting to show that some linear transformations
% can also be interpreted as changes of basis.

\subsection{Dot products}

An important concept is the dot product between vectors.
%
\begin{code}
dot :: (Ring s, Finite g) => Vector s g -> Vector s g -> s
dot v w = sum [v!i * w!i | i <- finiteDomain]
\end{code}
\jp{If we take the algebraic view then this definition is only correct if the canonical basis is orthonormal.}
Dot products have (at least) two aspects. First, they yield a notion of how
``big'' a vector is, the |norm|.

\begin{code}
sqNorm :: (Ring s, Finite g) => Vector s g -> s
sqNorm v = dot v v

norm v = sqrt (sqNorm v)
\end{code}

Additionally, the dot product often serves as a measure of how much
vectors are similar to (or correlated with) each other.

For two non-zero vectors |u| and |v|, we can define:
\begin{code}
similarity u v = dot u v / norm u / norm v
\end{code}
Dividing by the norms mean that |abs (similarity u v)| is at most 1
--- in the [-1,1] interval.\jp{For real fields. For complex ones one would use the inner product instead.}

In fact, for Euclidean spaces |similarity u v| is the cosine of the
angle between |u| and |v|.

For this reason, one says that two vectors are orthogonal when their
dot product is 0 --- even in non-Euclidean spaces.

\paragraph{Orthogonal transformations}

An important subclass of the linear transformations are those which
preserve the dot product.
\begin{spec}
  dot (f u) (f v) = dot u v
\end{spec}

In Euclidean spaces, such a transformation preserve angles. In general, they are called orthogonal transformations.

\begin{exercise}
Can you express this condition as a homomorphism condition?
\end{exercise}

Such transformations necessarily preserve the dimension of the space
(otherwise at least one base vector would be squished to nothing and
dot products involving it become zero). (When the dimension is
preseved, one often uses the term ``linear operator''.) The
corresponding matrices are square.

\begin{exercise}
  Prove that orthogonal operators form a monoid.
\end{exercise}

If angles are preserved what about distances? An isometry |f| is a
distance-preserving transformation:

\begin{spec}
  norm (f v) = norm v
\end{spec}

We can prove that |f| is orthogonal iff. it is an isometry. The proof
in the left-to-right direction is easy and left as an exercise. In the
other direction one uses the equality:
\begin{spec}
  4 * dot u v = sqNorm (u + v) - sqNorm (u - v)
\end{spec}

In Euclidean spaces, this means that preserving angles and preserving
distances go hand-in-hand.

Orthogonal transformations enjoy many more useful properties: we have barely scrached the surface here.
Among others, their rows (and columns) are orthogonal to each other.
The are also invertible (and so they form a group), and the inverse is the (conjugate-) transpose of the matrix.
(In the context of a complex scalar field, one would use the word ``unitary'' instead of ``orthogonal'', but it's a straightforward generalisation.)

\subsection{Examples of matrix algebra}

\subsubsection{Polynomials and their derivatives}

We have represented polynomials of degree |n+1| by the list of their
coefficients.
%
This is the same representation as the vectors represented
by |n+1| coordinates which we referred to in the introduction to this chapter.
%
This suggests that polynomials of degree |n| form a vector space,
and we could interpret that as |{0, ..., n} -> REAL| (or, more
generally, |Field a => {0, ..., n} -> a|).
%
The operations, |(+)| for vector addition and |(*^)| for vector scaling, are
defined in the same way as they are for functions.
%

To give an intuition for the vector space it is useful to consider the
interpretation of the canonical base vectors. Recall that they are:
%
\begin{spec}
e i : {0, ..., n} -> REAL, e i j = i `is` j
\end{spec}
%
but how do we interpret them as polynomial functions?

When we represented a polynomial by its list of coefficients, we saw
that the polynomial function |\x -> x^3| could be represented as
|[0,0,0,1]|, where |1| is the coefficient of |x^3|.
%
Similarly, representing this list of coefficients as a vector (a
function from |{0, ..., n} -> REAL|), we get the vector |\j -> if j ==
3 then 1 else 0|, which is |\j -> 3 `is` j| or simply |e 3|.

In general, |\x -> x^i| is represented by |e i|, which is another way
of saying that |e i| should be interpreted as |\x -> x^i|, a monomial.
%
Any other polynomial function |p| equals the linear combination of
monomials, and can therefore be represented as a linear combination of
our base vectors |e i|.
%
For example, |p x = 2+x^3| is represented by |2 *^ e 0 + e 3|.
%

In general, the evaluator from the |Vector g s| representation to polynomial
functions is as follows:
%
% \begin{code}
% evalM :: G -> (REAL -> REAL)
% evalM (G i) = \x -> x^i
%
% evalP :: Vector REAL G -> (REAL -> REAL)
% evalP (V v) x = sum (map (\i -> v i * evalM i x) finiteDomain)
% \end{code}
%
\begin{spec}
evalP :: Vector REAL {0, ..., n} -> (REAL -> REAL)
evalP (V v) x = sum (map (\ i -> v i * x^i) [0..n])
\end{spec}
%
The |derive| function takes polynomials of degree |n+1| to polynomials
of degree |n|, and since |D (f + g) = D f + D g| and |D (s *^ f) = s *^
D f|, we know that it is a linear transformation.
%
% What is its associated matrix?

The associated matrix will be obtained by appling the linear transformation to every base vector:
%
\begin{spec}
M = [ derive (e 0), derive (e 1), ..., derive (e n) ]
\end{spec}
%
where each |derive (e i)| has length |n|.
%
The vector |e (i + 1)| represents |\x -> x^(i + 1)| and thus we
want |derive (e (i + 1))| to represent the derivative of |\x -> x^(i + 1)|:
%
\begin{spec}
evalP (derive (e (i + 1)))  =  {- by spec. -}
D (evalP (e (i + 1)))       =  {- by def. of |e|, |evalP| -}
D (\x -> x^(i + 1))         =  {- properties of |D| from lecture 3 -}
\x -> (i + 1) * x^i         =  {- by def. of |e|, |evalP|, |(*^)| -}
evalP ((i + 1) *^ (e i))
\end{spec}
%
Thus
%
\begin{spec}
derive (e (i + 1)) = (i + 1) *^ (e i)
\end{spec}
%
Also, the derivative of |evalP (e 0) = \x -> 1| is |\x -> 0| and thus
|derive (e 0)| is the zero vector:
%
\begin{spec}
derive (e 0) = 0
\end{spec}

Example: |n+1 = 3|:
%
\begin{displaymath}
M =
  \begin{bmatrix}
    0 & 1 & 0 \\
    0 & 0 & 2
  \end{bmatrix}
\end{displaymath}

Take the polynomial function |p x = 1 + 2 * x + 3 * x^2|
%
as a vector
%
\[
 v = \colveccc{1\\2\\3}
\]
%
and we have
%
\[
   M  * v  = \rowveccc{\colveccc{0\\0} & \colveccc{1\\0} & \colveccc{0\\2}}  * \colveccc{1\\2\\3} = \colveccc{2\\6}
\]
% TODO: Perhaps explain this "row of columns" view of a matrix in contrast with the "column of rows" view.
% TODO: Perhaps also (or instead) just make the matrix be a two-dimensional grid of scalars.
%
representing the polynomial function |p' x = 2 + 6*x|.

Exercise~\ref{exc:Dmatrixpowerseries}: write the
(infinite-dimensional) matrix representing |D| for power series.

Exercise~\ref{exc:matrixIntegPoly}: write the matrix |In| associated with
integration of polynomials.

\subsubsection{Dot product for functions and Fourier series}

We said before that the dot product yields a notion of norm and
similarity. Unfortunately, the dot product (as defined above) is not
very useful in this respect for polynomials represented as monomial
coefficients: it is not clear what kind of similarity it corresponds
to. To find a more useful dot product, we can return to the semantics
of polynomials in terms of functions. But for now we consider them
over the restricted domain $I = [-\pi,\pi]$.

Assume for a moment that we would define the dot product of
functions $u$ and $v$ as follows:
\[
  |dotF u v| = \int_I |eval u|x |eval v|x dx
\]

Then, the norm of a function would be a measure of how far it gets from
zero, using a quadratic mean. Likewise, the corresponding similarity
measure corresponds to how much the functions ``agree'' on the
interval.  That is, if the signs of |eval u| and |eval v| are the same
on a sub-interval |I| then the integral is positive on |I|, and
negative if they are different.

As we suspected, using |dot = dotF|, the straightforward
representation of polynomials as list of coefficients is not an
orthogonal basis. There is, for example, a positive correlation
between |x| and |x^3|.

If we were using instead a set of basis polynomials |bn| which are
orthogonal using the above semantic-oriented definition of |dot|, then
we could compute it by multiplying pointwise and summing as before,
and this would be a lot more efficient than to compute the integral by
1. computing the product using |polyMul| 2. integrating using
|integ|. 3. using |eval| on the end points of the domain.

Let us consider as a base the functions |bn = sin (n*x)|, prove
that they are orthogonal.

We first use trigonometry to rewrite the product of bases:
\begin{spec}
   2 * (bi * bj)
=  2 * sin (i*x) * sin (j*x)
=  cos ((i-j)*x) - cos ((i+j)*x)
\end{spec}
%
Assuming |i/=j|, we can take the indefinite integral of both sides, safely
ignoring any constant term:
%
\begin{spec}
   2 \int bi bj dx
=  sin ((i-j)*x)/(i-j) - sin ((i+j)*x) / (i+j)
\end{spec}

But |sin (k*pi) = 0| for any integer |k|, and thus the definite
integral over |I| is also equal to zero.

We can now compute |sqNorm| of |bi|. Trigonometry says:

\begin{spec}
   2 * (bi * bi)
=  2 * sin (i*x) * sin (i*x)
=  cos (0*x) - cos (2i*x)
=  1 - cos (2i*x)
\end{spec}

When taking the integral on |I|, the cosine disappears using the same
argument as before, and there remains: |2*sqNorm bi = 2 pi|. Thus to
normalise the base vectors we need to scale them by |1 / sqrt pi|. In
sum |b_i = sin (i*x)/sqrt pi| is an orthonormal basis:

\begin{spec}
  bj `dot` bi = is i j
\end{spec}

As interesting as it is, this basis does not cover all functions over I. To
start, |eval bi 0 == 0| for every |i|, and thus linear
combinations can only ever be zero at the origin.

But if we were to include  |cos (n*x) / sqrt pi| in the set of base vectors, then the
space would cover all periodic functions with period $2 \pi$. This 
representation is called the Fourier series. Let us define a meaningful
index (|G|) for the basis:

\begin{code}
data Periodic where
  Sin :: Int -> Periodic
  Cos :: Int -> Periodic
\end{code}

A useful property of an orthonormal basis is that its representation
as coefficients can be obtained by taking the dot product with each base
vectors. Indeed:\footnote{The proof can be easily adapted to infinite sums.}

\begin{spec}
     v           = sum [vi *^ bi | i <- finiteDomain]
=>   v `dot` bj  = sum [vi *^ bi | i <- finiteDomain] `dot` bj
=>   v `dot` bj  = sum [vi *^ (bi `dot` bj) | i <- finiteDomain] 
=>   v `dot` bj  = sum [vi *^ is i j | i <- finiteDomain] 
=>   v `dot` bj  = vj
\end{spec}

Thus, in our application, given a periodic function |f|, one can
compute its Fourier series by taking the |dotF| product of it with
each of |sin (n*x) / sqrt pi| and |cos (n*x) / sqrt pi|.

\begin{exercise}
Derive |derive| for this representation.
\end{exercise}

\subsubsection{Simple deterministic systems (transition systems)}

Simple deterministic systems are given by endo-functions%
\footnote{An \emph{endo-function} is a function from a set |X| to
  itself: |f : X -> X|.}%
on a finite set |next : G -> G|.
%
They can often be conveniently represented as a graph, for example

\tikz [nodes={circle,draw}] {

  \node (n0) at (1,1) {0};
  \node (n1) at (2,2) {1};
  \node (n3) at (3,3) {3};
  \node (n6) at (4,2) {6};
  \node (n4) at (3,1) {4};
  \node (n5) at (4,0) {5};
  \node (n2) at (2,0) {2};

  \graph {
  (n0) -> (n1) -> (n3) -> (n6) -> (n5) -> (n4) -> (n6);
  (n2) -> (n5);
  };
}

Here, |G = {0, ..., 6}|.
%
A node in the graph represents a state.
%
A transition |i -> j| means |next i = j|.
%
Since |next| is an endo-function, every node must
be the source of exactly one arrow.

We can take as vectors the characteristic functions of subsets of |G|,
i.e., |G -> {0, 1}|.
%
|{0, 1}| is not a field w.r.t. the standard arithmetical operations
(it is not even closed w.r.t. addition), and the standard trick to
workaround this issue is to extend the type of the functions to |REAL|.

The canonical basis vectors are, as usual,
|e i = V (is i)|.
Each |e i| is the characteristic function of a singleton set, |{i}|.


We can interpret |e (next 0), ..., e (next 6)| as the images of the
basis vectors |e 0, ..., e 6| of |Vector REAL G| under the
transformation
%
\begin{spec}
f : Vector REAL G -> Vector REAL G
f (e i) = e (next i)
\end{spec}

To write the matrix associated to |f|, we have to compute what vector
is associated to each canonical base vector vector:
%
\begin{spec}
M = [ f (e 0), f (e 1), ..., f (e n) ]
\end{spec}

Therefore:
%
\[
  M =
  \bordermatrix{
         & c_0 & c_1 & c_2 & c_3 & c_4 & c_5 & c_6 \cr
    r_0  &  0  &  0  &  0  &  0  &  0  &  0  &  0 \cr
    r_1  &  1  &  0  &  0  &  0  &  0  &  0  &  0 \cr
    r_2  &  0  &  0  &  0  &  0  &  0  &  0  &  0 \cr
    r_3  &  0  &  1  &  0  &  0  &  0  &  0  &  0 \cr
    r_4  &  0  &  0  &  0  &  0  &  0  &  1  &  0 \cr
    r_5  &  0  &  0  &  1  &  0  &  0  &  0  &  1 \cr
    r_6  &  0  &  0  &  0  &  1  &  1  &  0  &  0 \cr
  }
\]

Notice that row 0 and row 2 contain only zero, as one would expect from
the graph of |next|: no matter where we start from, the system will
never reach node 0 or node 2.

Starting with a canonical base vector |e i|, we obtain |M * e i = f (e
i)|, as we would expect.
%
The more interesting thing is if we start with something different
from a basis vector, say |[0, 0, 1, 0, 1, 0, 0] == e 2 + e 4|.
%
We obtain |{f 2, f 4} = {5, 6}|, the image of |{2, 4}| through |f|.
%
In a sense, we can say that the two transitions happened in
parallel.
%
But that is not quite accurate: if start with |{3, 4}|, we no longer
get the characteristic function of |{f 3, f 4} = {6}|, instead, we get
a vector that does not represent a characteristic function at all:
|[0, 0, 0, 0, 0, 0, 2] = 2 *^ e 6|.
%
In general, if we start with an arbitrary vector, we can interpret
this as starting with various quantities of some unspecified material
in each state, simultaneously.
%
If |f| were injective, the respective quantities would just get
shifted around, but in our case, we get a more general behaviour.

What if we do want to obtain the characteristic function of the image
of a subset?
%
In that case, we need to use other operations than the standard
arithmetical ones, for example |min| and |max|.

However, |({0, 1}, max, min)| is not a field, and neither is
|(REAL, max, min)|.
%
This means that we do not have a vector space, but rather a \emph{module}.
One can still do a lot with modules: for example the definition of multiplication only demands a |Ring| rather than a |Field|.
Therefore, having just a module is not a problem if all we want is to compute the evolutions of
possible states,
but we cannot apply most of the deeper results of
linear algebra.\jp{But perhaps we have not defined those so far... Here it begs to say that the behaviour at the limit is given by the
                   inverted matrix. But where does it fit in the chapter?}

%*TODO: (NiBo)
% But even if we take |(REAL, max, min)|, the problem is that we
% have introduced a transformation |f : Vector REAL G -> Vector REAL G| which
% is more difficult to understand than just |next : G -> G|. Can we take
% advantage of |f| for formulating and answering questions about |next|?
% For instance, to understand how to compute the trajectories induced by
% |next|? Or how to find subsets of |G| such that for all |g : G| there
% exists |N : Nat| such that for all |n > N|, |next^n g)| is in that
% subset? Perhaps there is an added value in looking at dynamical systems
% from the perspective of vector spaces but this is not obvious (to me) at
% this point.


In the example above, we have:
%
\begin{code}
newtype G = G Int deriving (Eq, Show, Additive, Multiplicative, AddGroup)

instance Bounded G where
  minBound  =  G 0
  maxBound  =  G 6

instance Enum G where
  toEnum          =  G
  fromEnum (G n)  =  n

\end{code}
Note that the |Ring G| instance is given just for convenient notation (integer literals):
vector spaces in general do not rely on any numeric structure on the indices (|G|).
%
The transition function has type |G -> G| and the following implementation:
%
\begin{code}
next1 :: G -> G
next1 0 = 1; next1 1 = 3; next1 2 = 5; next1 3 = 6; next1 4 = 6; next1 5 = 4; next1 6 = 5
\end{code}
%
Its associated matrix is
%
\begin{spec}
m g'
  = {- |m| is the matrix associated with |f| -}
V (\ g -> toF (f (e g)) g')
  = {- by the spec. of |f| -}
V (\ g -> toF (e (next g)) g')
  = {- by def. of |e| -}
V (\ g -> toF (V (is (next g))) g')
  = {- by def. of |toF| -}
V (\ g -> is (next g) g')
\end{spec}
%
where
%
\begin{code}
toF :: Vector s g -> g -> s
toF (V v) = v
\end{code}
\jp{This is the same as indexing |(!)|}
%
Thus we can implement |m| as:
%
\begin{code}
m1 :: Ring s => G -> Vector s G
m1 g' = V (\ g -> (next1 g) `is` g')
\end{code}

Test:
%
\begin{code}
t1' :: Vector Int G
t1'  = mulMV m1 (e 3 + e 4)
t1   = toL t1'               -- |[0,0,0,0,0,0,2]|
\end{code}
\jp{Fold this implementation in the text}


%**TODO (NiBo):
%if False
% This could go to into the file for the live sessions:
%
\begin{code}
poss :: (Finite g, Ring s) => Int -> Matrix s g g -> Vector s g -> [Vector s g]
poss n m v = take (n + 1) (iterate (mulMV m) v)

testPoss0 :: [Vector Int G]
testPoss0 = poss 6 m1 (e 3 + e 4)
\end{code}
%
%
%
% Perhaps as an exercise to show how the notion of |Vector S| can
% be applied to seamlessly treat dynamical systems (contrast the
% implementation of |poss| with those of |poss1|, |poss2| and |poss3| for
% different dynamical systems):
% %
\begin{code}
poss1 :: Int -> (a -> a) -> [a] -> [[a]]
poss1 n next xs  = take (n + 1) (iterate (map next) xs)

testPoss1 = poss1 6 next1 [3, 4]

poss1' :: (Eq a) => Int -> (a -> a) -> [a] -> [[a]]
poss1' n next xs = take (n + 1) (iterate (nub . map next) xs)

testPoss1' = poss1' 6 next1 [3, 4]
\end{code}
%
% *DSLsofMath.W07> |poss1 6 next1 [3, 4]|
%
%endif False


\subsubsection{Non-deterministic systems}
\label{sec:NonDetSys}

Another interpretation of the application of |M| to characteristic
functions of a subset is the following: assuming that all I know is
that the system is in one of the states of the subset, where can it
end up after one step?  (this assumes the |max|-|min| algebra as
above).
%

The general idea for non-deterministic systems, is that the result of
applying the step function a number of times from a given starting
state is a list of the possible states one could end up in.

In this case, the uncertainty is entirely caused by the fact that we
do not know the exact initial state.
%
However, there are cases in which the output of |f| is not known, even
when the input is known.
%
Such situations are modelled by endo-relations: |R : G -> G|, with |g
R g'| if |g'| is a potential successor of |g|.
%
Endo-relations can also be pictured as graphs, but the restriction
that every node should be the source of exactly one arrow is lifted.
%
Every node can be the source of one, none, or many arrows.

For example:

\tikz [nodes={circle,draw}] {

  \node (n0) at (1,1) {0};
  \node (n1) at (2,2) {1};
  \node (n3) at (3,3) {3};
  \node (n6) at (4,2) {6};
  \node (n4) at (3,1) {4};
  \node (n5) at (4,0) {5};
  \node (n2) at (2,0) {2};

  \graph {
    (n0) -> { (n1), (n2) };
    (n1) -> (n3);
    (n2) -> { (n4), (n5) };
    (n3) -> (n6);
    (n4) -> { (n1), (n6) };
    (n5) -> (n4);
  };
}

Now, starting in |0| we might end up either in |1| or |2| (but not
both!).
%
Starting in |6|, the system breaks down: there is no successor state.

The matrix associated to |R| is built in the same fashion: we need to
determine what vectors the canonical base vectors are associated with:

\[
  M =
  \bordermatrix{
         & c_0 & c_1 & c_2 & c_3 & c_4 & c_5 & c_6 \cr
    r_0  &  0  &  0  &  0  &  0  &  0  &  0  &  0 \cr
    r_1  &  1  &  0  &  0  &  0  &  1  &  0  &  0 \cr
    r_2  &  1  &  0  &  0  &  0  &  0  &  0  &  0 \cr
    r_3  &  0  &  1  &  0  &  0  &  0  &  0  &  0 \cr
    r_4  &  0  &  0  &  1  &  0  &  0  &  1  &  0 \cr
    r_5  &  0  &  0  &  1  &  0  &  0  &  0  &  0 \cr
    r_6  &  0  &  0  &  0  &  1  &  1  &  0  &  0 \cr
  }
\]

Exercise~\ref{exc:NonDetExample1}: start with |e 2 + e 3| and iterate
a number of times, to get a feeling for the possible evolutions.
%
What do you notice?
%
What is the largest number of steps you can make before the result is
the zero vector?
%
Now invert the arrow from |2| to |4| and repeat the exercise.
%
What changes?
%
Can you prove it?

Implementation:

The transition relation has type |G -> (G -> Bool)|:
%
\begin{code}
f2 :: G -> (G -> Bool)
f2 0 g      =   g == 1 || g == 2
f2 1 g      =   g == 3
f2 2 g      =   g == 4 || g == 5
f2 3 g      =   g == 6
f2 4 g      =   g == 1 || g == 6
f2 5 g      =   g == 4
f2 6 g      =   False
\end{code}
%
The associated matrix:
%
\begin{code}
m2 g' = V (\ g -> f2 g g')
\end{code}

% The original formulation was potentially very confusing: |f2| was
% claimed to have type |G -> (G -> Bool)|. But the implementation of
% |m2| was not consistent with this typing. It worked only because |f2|
% was polymorphic.


%
% TODO (by DaHe): Should probably elaborate on why we needed a field before, and
% now a Ring instance
%
We need a |Ring| instance for |Bool| (not a field!):
%
\begin{code}
instance Additive Bool where
  (+)  =  (||)
  zero = False

instance AddGroup Bool where
  negate         =  not

instance Multiplicative Bool where
  one = True
  (*)  =  (&&)
\end{code}

Test:
%
\begin{code}
t2' = mulMV m2 (e 3 + e 4)
t2 = toL t2'  -- |[False,True,False,False,False,False,True]|
\end{code}
\jp{Fold in the text}

%**TODO (NiBo):
% This could go to into the file for the live sessions:
%
% *DSLsofMath.W07> |poss 6 m2 (e 2 + e 3)|
%
% Perhaps as an exercise to show how the notion of |Vector S| can
% be applied to seamlessly treat dynamical systems (contrast the
% implementation of |poss| with those of |poss1|, |poss2| and |poss3| for
% different dynamical systems):
%
% \begin{code}
% next2 :: G -> [G]
% next2 0 = [1,2]
% next2 1 = [3]
% next2 2 = [4,5]
% next2 3 = [6]
% next2 4 = [1,6]
% next2 5 = [4]
% next2 6 = []
%
% poss2 :: (Eq a) => Int -> (a -> [a]) -> [a] -> [[a]]
% poss2 n next xs = take (n + 1) (iterate (nub . concat . (map next)) xs)
% \end{code}
%
% *DSLsofMath.W07> |poss2 6 next2 [2,3]|

\subsubsection{Stochastic systems}
\label{sec:StocSys}

Quite often, we have more information about the transition to possible
future states.
%
In particular, we can have \emph{probabilities} of these transitions.
%
For example
%

\tikz [nodes={circle,draw}, scale=1.2]{

  \node (n0) at (1,1) {0};
  \node (n1) at (2,2) {1};
  \node (n3) at (3,3) {3};
  \node (n6) at (4,2) {6};
  \node (n4) at (3,1) {4};
  \node (n5) at (4,0) {5};
  \node (n2) at (2,0) {2};

  \graph [edges={ ->,>=latex,nodes={draw=none}}] {
     (n0) ->[".4"] (n1);
     (n0) ->[".6",swap] (n2);
     (n1) ->["1"]  (n3);
     (n2) ->[".7"] (n4);
     (n2) ->[".3"] (n5);
     (n3) ->["1"]  (n6);
     (n4) ->[".5",swap] (n1);
     (n4) ->[".5"] (n6);
     (n5) ->["1",swap]  (n4);
%     (n6) ->[loop right] (n6);
  };
  \path[->,>=latex,nodes={draw=none}] (n6) edge ["1",loop right] node {} (n6);
}

One could say that this case is a generalisation of the previous one,
in which we can take all probabilities to be equally distributed among
the various possibilities.
%
While this is plausible, it is not entirely correct.
%
For example, we have to introduce a transition from state |6| above.
%
The nodes must be sources of \emph{at least} one arrow.

In the case of the non-deterministic example, the ``legitimate''
inputs were characteristic functions, i.e., the ``vector space'' was
|G -> {0, 1}| (the scare quotes are necessary because, as discussed,
the target is not a field).
%
In the case of stochastic systems, the inputs will be
\emph{probability distributions} over |G|, that is, functions |p : G
-> [0, 1]| with the property that
%
\begin{spec}
    sum [p g | g <- G] = 1
\end{spec}

If we know the current probability distributions over states, then we
can compute the next one by using the \emph{total probability formula},
normally expressed as
%
\begin{spec}
    p a = sum [p (a | b) * p b | b <- G]
\end{spec}
%
This formula in itself is worth a lecture (see \cref{ch:probability-theory}).
%
But for now, let's just remark that the notation is extremely suspicious.
%
|(a || b)|, which is usually read ``|a|, given |b|'', is clearly not
of the same type as |a| or |b|, so cannot really be an argument to
|p|. We discuss this notation at length in \cref{ch:probability-theory}.
%
Additionally, the |p a| we are computing with this formula is not the
|p a| which must eventually appear in the products on the right hand
side.
%
We do not know how this notation came about: it is neither in Bayes'
memoir, nor in Kolmogorov's monograph.\jp{citations needed}

Regardless, at this stage, what we need to know is that the
conditional probability |p (a || b)| gives us the probability that the
next state is |a|, given that the current state is |b|.
%
But this is exactly the information summarised in the graphical
representation.
%
Moreover, it is clear that, at least formally, the total probability
formula is identical to a matrix-vector multiplication.

As usual, we write the associated matrix by looking at how the
canonical base vectors are transformed.
%
% TODO (by DaHe): Again, I find this a little confusing: If e = is, then how is
% e i the probability distribution concentrated in i? PaJa: show that the type is right and the sum is 1.
In this case, the canonical base vector |e i = \ j -> i `is` j| is the
probability distribution \emph{concentrated} in |i|.
%
This means that the probability to be in state |i| is 100\% and the
probability of being anwhere else is |0|.
%
\[
  M =
  \bordermatrix{
         & c_0 & c_1 & c_2 & c_3 & c_4 & c_5 & c_6 \cr
    r_0  &  0  &  0  &  0  &  0  &  0  &  0  &  0 \cr
    r_1  &  .4 &  0  &  0  &  0  &  .5 &  0  &  0 \cr
    r_2  &  .6 &  0  &  0  &  0  &  0  &  0  &  0 \cr
    r_3  &  0  &  1  &  0  &  0  &  0  &  0  &  0 \cr
    r_4  &  0  &  0  &  .7 &  0  &  0  &  1  &  0 \cr
    r_5  &  0  &  0  &  .3 &  0  &  0  &  0  &  0 \cr
    r_6  &  0  &  0  &  0  &  1  &  .5 &  0  &  1 \cr
  }
\]

Exercise~\ref{exc:StocExample1}: starting from state 0, how many steps
do you need to take before the probability is concentrated in state 6?
Reverse again the arrow from 2 to 4.\jp{what does that mean how do we make it so that columns sum to 1? Also NiBo:There are many ways of reversing the arrow from 2 to 4.}
%
What can you say about the long-term behaviour of the system now?

Exercise~\ref{exc:StocExample1Impl}: Implement the example.
%
You will need to define:

The transition function (giving the probability of getting to |g'| from |g|)
%
\begin{code}
f3 :: G -> Vector REAL G   -- but we want only |G -> Vector [0, 1] G|, the unit interval
\end{code}
%
and the associated matrix
%
\begin{code}
m3 ::  G -> Vector REAL G
\end{code}

%**TODO (NiBo):
% This could go to into the file for the live sessions:

%if False
\begin{code}
-- f3 :: G -> Vector REAL G
f3 0 = V (\ g -> if g == 1 then 0.4 else if g == 2 then 0.6 else 0.0)
f3 1 = V (\ g -> if g == 3 then 1.0 else 0.0)
f3 2 = V (\ g -> if g == 4 then 0.7 else if g == 5 then 0.3 else 0.0)
f3 3 = V (\ g -> if g == 6 then 1.0 else 0.0)
f3 4 = V (\ g -> if g == 1 then 0.5 else if g == 6 then 0.5 else 0.0)
f3 5 = V (\ g -> if g == 4 then 1.0 else 0.0)
f3 6 = V (\ g -> if g == 6 then 1.0 else 0.0)

f3' :: G -> Vector REAL G
f3' 0 = V (\ g -> if g == 1 then 0.4 else if g == 2 then 0.6 else 0.0)
f3' 1 = V (\ g -> if g == 3 then 1.0 else 0.0)
f3' 2 = V (\ g -> if g == 5 then 1.0 else 0.0)
f3' 3 = V (\ g -> if g == 6 then 1.0 else 0.0)
f3' 4 = V (\ g -> if g == 1 then 0.4 else if g == 6 then 0.4 else if g == 2 then 0.2 else 0.0)
f3' 5 = V (\ g -> if g == 4 then 1.0 else 0.0)
f3' 6 = V (\ g -> if g == 6 then 1.0 else 0.0)

m3 g' = V (\ g -> toF (f3 g) g')
\end{code}

*DSLsofMath.W07> |last (poss 6 m3 (e 0))|

%endif


% Perhaps as an exercise to show how the notion of |Vector S| can
% be applied to seamlessly treat dynamical systems (contrast the
% implementation of |poss| with those of |poss1|, |poss2| and |poss3| for
% different dynamical systems):
%
% \begin{code}
% next3 :: G -> [(G, REAL)]
% next3 0 = [(1,0.4), (2,0.6)]
% next3 1 = [(3,1.0)]
% next3 2 = [(4,0.7), (5,0.3)]
% next3 3 = [(6,1.0)]
% next3 4 = [(1,0.5), (6,0.5)]
% next3 5 = [(4,1.0)]
% next3 6 = [(6,1.0)]
%
% step :: (Eq a) => (a -> [(a, REAL)]) -> [(a, REAL)] -> [(a, REAL)]
% step sys aps = concat (map g (map f aps))
%   where f (a, p) = (sys a, p)
%         g (aps, p) = map (\ (a', p') -> (a', p' * p)) aps
% \end{code}
%
% What are the types of |f| and |g|?
%
% \begin{code}
% poss3 :: (Eq a) => Int -> (a -> [(a, REAL)]) -> [(a, REAL)] -> [[(a, REAL)]]
% poss3 n next xps = take (n + 1) (iterate (step next) xps)
%
% prob :: (Eq a) => a -> [(a, REAL)] -> REAL
% prob a [] = 0
% prob a ((x,p) : xps) = if a == x
%                        then p + prob a xps
%                        else     prob a xps
% \end{code}
%
% What does |prob| compute? Evaluate |prob 6 (last (poss3 n next3
% [(0,1.0)]))| for n = 1, ..., 6. What do you observe?


\subsubsection{Quantum Mechanics}

Instead of real numbers for probabilities, we could consider using
complex numbers --- one then speaks of ``amplitudes''. An amplitude
represented by a complex number |z| is converted to a probability by
taking the square of the modulus of |z|:
\begin{spec}
p z = conj z * z
\end{spec}

We can then rewrite the law of total probability as follows:
\begin{spec}
     sum [p!i | i <- finiteDomain]
=    sum [conj (z!i) * (z!i) | i <- finiteDomain]
=    inner z z
\end{spec}

Where the |inner| product generalises the dot product for vector spaces
with complex scalars (the left operand is conjugated indexwise).  Hence, rather
conveniently, the law of total probability is replaced by conservation
of the norm of state vectors.
In particular, norms are conserved if the transition matrix is unitary.


The unitary character of the transition matrix
defines valid systems from the point of view of quantum mechanics.
%
Because all unitary matrices are invertible,
it follows that all quantum mechanical systems have an invertible dynamics.
Furthermore, the inverted matrix is also unitary,
and therefore the inverted system is also valid as a quantum dynamical system.

\jp{An example as a graph is daunting. One can construct an example by combining generator matrices $e^{i \theta}$ (one coordinate rotates in complex plane), and 2-d rotations (the "cos theta/sin thetha/-sin theta/cos theta" matrix). But there wont be "mostly zeros everywhere". The form $U = e^{iH}$ is also a possibility, but then there is a whole bunch of more theory to cover.}

\subsection{Monadic dynamical systems}

This section is not part of the intended learning outcomes of the
course, but it presents a useful unified view of the previous
sections which could help your understanding.

All the examples of dynamical systems we have seen in the previous
section have a similar structure.
%
They work by taking a state (which is one of the generators) and
return a structure of possible future states of type |G|:

\begin{itemize}
\item deterministic: there is exactly one possible future state: we
  take an element of |G| and return an element of |G|.
  %
  The transition function has the type |f : G -> G|, the structure of
  the target is just |G| itself.
\item non-deterministic: there is a set of possible future states,
  which we have implemented as a characteristic function |G -> {0,
    1}|.
  %
  The transition function has the type |f : G -> (G -> {0, 1})|.
  %
  The structure of the target is the \emph{powerset} of |G|.
\item stochastic: given a state, we compute a probability distribution
  over possible future states.
  %
  The transition function has the type |f : G -> (G -> Complex)|, the
  structure of the target is the probability distributions over |G|.
\item quantum: given an observable state, we compute an (orthogonal) superposition
  of possible future states.
\end{itemize}

Therefore:
%
\begin{itemize}
\item deterministic: |f : G -> Id G|
\item non-deterministic: |f : G -> Powerset G|, where |Powerset G = G -> {0, 1}|
\item stochastic: |f : G -> Prob G|, where |Prob G = G -> [0, 1]|
\item quantum: |f : G -> Super G|, where |Super G = G -> Complex|
\end{itemize}

We have represented the elements of the various structures as vectors.
%
We also had a way of representing, as structures of possible states,
those states that were known precisely: these were the canonical base
vectors |e i|.
%
Due to the nature of matrix-vector multiplication, what we have done
was in effect:
%
\begin{spec}
    M * v       -- |v| represents the current possible states

= {- |v| is a linear combination of the base vectors -}

    M * (v 0 *^ e 0 + ... + v n *^ e n)

= {- homomorphism -}

    v 0 *^ (M * e 0) + ... + v n *^ (M * e n)

= {- |e i| represents the perfectly known current state |i|, therefore |M * e i = f i| -}

    v 0 *^ f 0 + ... + v n *^ f n
\end{spec}
%
So, we apply |f| to every state, as if we were starting from precisely
that state, obtaining the possible future states starting from that
state, and then collect all these hypothetical possible future
states in some way that takes into account the initial uncertainty
(represented by |v 0|, ..., |v n|) and the nature of the uncertainty
(the specific |(+)| and |(*^)|).

If you examine the types of the operations involved
%
\begin{spec}
e : G -> Possible G
\end{spec}
%
and
%
\begin{spec}
    flip (*) : Possible G -> (G -> Possible G) -> Possible G
\end{spec}
%
\jp{We need a proper introduction to monads somewhere}
you see that they are very similar to the monadic operations
%
\begin{spec}
    return  :  g -> m g
    (>>=)   :  m g -> (g -> m g') -> m g'
\end{spec}
%
which suggests that the representation of possible future states might
be monadic.
%
Indeed, that is the case.

Since we implemented all these as matrix-vector multiplications, this
raises the question: is there a monad underlying matrix-vector
multiplication, such that the above are instances of it (obtained by
specialising the scalar type |S|)?

Exercise: write |Monad| instances for |Id|, |Powerset|, |Prob|, |Supp|.


\subsection{The monad of linear algebra}
\jp{Todo: connect this with the monad of probability theory chapter.}
The answer is yes, up to a point.
%
Haskell |Monad|s, just like |Functor|s, require |return| and |>>=| to
be defined for every type.
%
This will not work, in general.
%
Our definition will work for \emph{finite types} only.
%
\begin{code}
class FinFunc f where
  func :: (Finite a, Finite b) => (a -> b) -> f a -> f b

class FinMon f where
  embed   ::  Finite a => a -> f a
  bind    ::  (Finite a, Finite b) => f a -> (a -> f b) -> f b
\end{code}

The idea is that vectors on finite types are finite functors and monads:
%
\begin{code}
instance Ring s => FinFunc (Vector s) where
  func f (V v) = V (\ g' -> sum [v g | g <- finiteDomain, g' == f g])

instance Ring s => FinMon (Vector s) where
  embed g       =  V (is g)
  bind (V v) f  =  V (\ g' -> sum [toF (f g) g' * v g | g <- finiteDomain])
\end{code}
%
Note that, if |v :: Vector S G| and |f :: G -> Vector S G'| then
both |func f v| and |bind v f| are of type |Vector S G'|. How do
these operations relate to linear algebra and matrix-vector multiplication?

Remember that |e g| is that vector whose components are zero except for
the |g|th one which is one. In other words
%
\begin{spec}
    e g = V (is g) = embed g
\end{spec}
%
and thus |embed = e|.
%
In order to understand how matrix-vector multiplication relates to the
monadic operations, remember that matrixes are just functions of type |G -> Vector S G'|:
%
\begin{spec}
  type Matrix s g g' = g' -> Vector s g
\end{spec}

According to our earlier definition, we can rewrite matrix-vector
multiplication in terms of dot products
%
\begin{spec}

  mulMV m v

= {- earlier definition -}

  V (\i -> sum [(m i ! j) * (v!j) | j <- finiteDomain])

= {- def. of |dot| -}

  V (\i -> dot (m i') v)

\end{spec}
%
Now, with
%
\begin{code}
toMatrix :: (g -> Vector s g') -> Matrix s g g'
toMatrix f = \ g' -> V (\ g -> toF (f g) g')
\end{code}
%
we have:
%
\begin{spec}

  mulMV (toMatrix f) (V v)

= {- def. of |mulMV| -}

  V (\ g' -> dot ((toMatrix f) g') (V v))

= {- def. of |toMatrix| -}

  V (\ g' -> dot (V (\ g -> toF (f g) g')) (V v))

= {- def. of |dot| -}

  V (\ g' -> sum [toF (f g) g' * v g | g <- finiteDomain])

= {- def. of |bind| -}

  bind (V v) f

\end{spec}
%
Thus we see that |bind v f| is ``just'' a matrix-vector
multiplication.

Perhaps for extra exercises:\jp{clarify status}

It is worth pointing out the role of |f| in |func f v|.
%
We can rewrite the |g'|th component of |func f v| in terms of the dot
product
%
\begin{spec}
  dot v (V (\ g -> is g' (f g)))
=
  dot v (V (is g' . f))
\end{spec}
%
This shows that the role of |f| in |func f v| is that of re-distributing
the values of |v| onto the new vector.

\begin{exercise}
show that if |w = func f v| then the sum of the components of
|w| is equal to the sum of the components of |v|.
\end{exercise}


\begin{exercise}
\begin{enumerate}
\item Prove that the functor laws hold, i.e.
%
\begin{spec}
func id       =  id
func (g . f)  =  func g . func f
\end{spec}

\item Prove that the monad laws hold, i.e.
%
\begin{spec}
bind v return      =  v
bind (return g) f  =  f g
bind (bind v f) h  =  bind v (\ g' -> bind (f g') h)
\end{spec}

\item What properties of |S| have you used to prove these properties?
%
  Define a new type class |GoodClass| that accounts for these (and
  only these) properties.
\end{enumerate}
\end{exercise}

%*TODO: Proving that |func| preserves composition and that |bind|
% associates gets very messy if one directly operates with their
% definitions.
% %
% It is probably simpler to go back to standard linear algebra notation
% (with indexed |sum|, etc.) which in a sense seems to defeat the purpose
% of focusing on the syntax.

\subsection{Associated code}

Conversions and |Show| functions so that we can actually see our vectors.
%
\begin{code}
toL :: Finite g => Vector s g -> [s]
toL (V v) = map v finiteDomain


instance (Finite g, Show s) => Show (g->s)        where  show = showFun
instance (Finite g, Show s) => Show (Vector s g)  where  show = showVector

showVector :: (Finite g, Show s) => Vector s g -> String
showVector (V v) = showFun v
showFun :: (Finite a, Show b) => (a->b) -> String
showFun f = show (map f finiteDomain)
\end{code}

%*TODO: perhaps convert to using the |newtype Vector|.

The scalar product of two vectors is a good building block for matrix
multiplication:
%
\begin{code}
dot' ::  (Finite g, Ring s) =>
        (g->s) -> (g->s) -> s
dot' v w = sum (map (v * w) finiteDomain)
\end{code}
%
Note that |v * w :: g -> s| is using the |FunNumInst|.

Using it we can shorten the definition of |mulMV|

\begin{spec}
  mulMV m v g'
= -- Earlier definition
  sum [m g' g * v g | g <- finiteDomain]
= -- replace list comprehension with |map|
  sum (map (\g -> m g' g * v g) finiteDomain)
= -- use |FunNumInst| for |(*)|
  sum (map (m g' * v) finiteDomain)
= -- Def. of |dot'|
  dot' (m g') v
\end{spec}
%
Thus, we can define matrix-vector multiplication by
%
\begin{spec}
mulMV m v g' =  dot' (m g') v
\end{spec}
%
We can even go one step further:
%
\begin{spec}
  mulMV m v
= -- Def.
  \g' -> dot' (m g') v
= -- |dot'| is commutative
  \g' -> dot' v (m g')
= -- Def. of |(.)|
  dot' v . m
\end{spec}
%
to end up at
%
\begin{code}
mulMV' ::  (Finite g, Ring s) =>
           Mat s g g' ->  Vec s g  ->  Vec s g'
mulMV' m v  =  dot' v . m
type Mat s r c = c -> r -> s
type Vec s r = r -> s
\end{code}

Similarly, we can define matrix-matrix multiplication:
%
\begin{code}
mulMM' ::  (Finite b, Ring s) =>
           Mat s b c   ->  Mat s a b  ->  Mat s a c
mulMM' m1 m2 = \r c -> mulMV' m1 (getCol m2 c) r

transpose :: Mat s g g' -> Mat s g' g
transpose m i j = m j i
getCol :: Mat s g g' -> g -> Vec s g'
getCol = transpose
getRow :: Mat s g g' -> g' -> Vec s g
getRow = id
\end{code}

% -- Specification: (a * b) * v == a * (M * v)
% -- Specification: mulMV (mulMM a b) v == mulMV a (mulMV b v)
% -- Specification: mulMV (mulMM a b) == (mulMV a) . (mulMV b)
% -- Specification: eval (mulMM a b) == (eval a) . (eval b)
%
%   eval (mulMM a b)
% = -- spec.
%   (eval a) . (eval b)
% = -- def. of |eval|
%   (\w -> dot w . a) . (\v -> dot v . b)
% = -- def. of |(.)|
%   \v -> (\w -> dot w . a) ((\v -> dot v . b) v)
% = -- simplification
%   \v -> (\w -> dot w . a) (dot v . b)
% = -- simplification
%   \v -> (dot (dot v . b) . a)
% ... gets too complicated for this chapter

%include E7.lhs
