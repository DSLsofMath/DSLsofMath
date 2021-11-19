\chapter{Elements of Linear Algebra}
\label{sec:LinAlg}
%if False
\begin{code}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DSLsofMath.W07 where
import DSLsofMath.Algebra
import Prelude hiding (Num(..),(/),(^),Fractional(..),Floating(..),sum)
import Data.List(nub)
type REAL = Double
type ℕ = Int
\end{code}
%endif

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
a \emph{field} (\cref{sec:fields-defintion}).
%

However, following our theme, we will first characterize vectors
algebraically.
%
From this perpective a \emph{vector space} is an algebraic structure
that captures a set of vectors, with zero, a commutative addition, and
scaling by a set of scalars (i.e., elements of the field).
%
In terms of typeclasses, we can characterize this structure as
follows:
\begin{code}
class (Field s, AddGroup v) => VectorSpace v s where
  (*^) :: s -> v -> v
\end{code}
Additionally, vector scaling (|s *^|) must be a homomorphism over
(from and to) the additive group structure of |v|:
\begin{spec}
  s *^ (a + b)     = s *^ a + s *^ b
  s *^ zero        = zero
  s *^ (negate a)  = negate (s *^ a)
\end{spec}
And, on the other side, |(*^ a)| is a homomorphism from the additive
group structure of |s| to the group structure of |v|:
\begin{spec}
  (s + t)   *^ a   = s *^ a + t *^ a
  zero      *^ a   = zero
  negate s  *^ a   = negate (s *^ a)
\end{spec}
The multiplicative monoid of |s| maps to the monoid of endofunctions
(see \cref{ex:endofunction}) homomorphically:
\begin{spec}
  one      *^ a    = id                  a  =  a
  (s * t)  *^ a    = ((s *^) . (t *^))   a  =  s *^ (t *^ a)
\end{spec}
Traditionally some of the above laws are omitted because they are
consequences of other laws.

An important consequence of the algebraic structure of vectors is that
they can be expressed as a simple sort of combination of other special
vectors.
%
More precisely, we can \emph{uniquely} represent any vector |v| in the
space in terms of a fixed set of \emph{basis} vectors |{b0, ..., bn}|.
By definition, basis vectors
cover the whole space:
\begin{spec}
  ∀ |v|, ∃s0, …, sn. v = s0 *^ b0 + ... + sn *^ bn 
\end{spec}
They are also \emph{linearly independent}:
%
\begin{spec}
  (s0 *^ b0 + ... + sn *^ bn = 0) <=> (s0 = ... = sn = 0)
\end{spec}
One can prove the uniqueness of representation as follows.
\begin{proof}
  Assume two representations of |v|, given by |si| and |ti|.
  %
  The difference of those representations is given by |si-ti|.
  %
  But because they represent the same vector, they must be equal to
  the zero vector: |(s0-t0) *^ b0 + ... + (sn-tn) *^ bn = 0|.
  %
  By the bases being linearly independent, we find |si-ti=0|, and
  |si=ti|.
\end{proof}

According to our red thread, this representation (coefficients) is
akin to the notion of syntax.
%
But this is a case where the representation is \emph{equivalent} to
the algebraic definition: the evaluator is not only a homomorphism,
but an isomorphism.

%
This equivalence is what justifies the introduction of vectors as
columns (or rows) of numbers.
%
Indeed, we can define:
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
the canonical base vectors, i.e.\ $e_k$ is the vector that is
everywhere zero except at position |k|, where it is one, so that |v =
v0 *^ e0 + ... + vn *^ en|.
%
This formula maps the syntax (coefficients) to the semantics (a
vector).

\begin{exercise}
  Define a function which takes as input a vector |v| and a set of
  (non-canonical) basis vectors $b_i$ and returns the coefficients of
  |v| in that basis.
\end{exercise}
% One way to go would be to take projections on each base vector, one at a time. If the coefficients of v in the new basis are x_i, then:
% x_0 =  v · b_0
% v_0 =  v  -  x_0 *^ b_0
% x_1 =  v_0 · b_1
% v_1 =  v_0 - x_1 *^ b_1
% ...

\section{Representing vectors as functions}
In what follows we will systematically use the represention of vectors
as a linear combination of basis vectors.
%
There is a temptation to model the corresponding collection of
coefficients as a list, or a tuple, but a more general (and
conceptually simpler) way is to view them as a \emph{function} from a
set of indices |G|:
%
\begin{code}
newtype Vector s g    = V (g -> s) deriving (Additive, AddGroup)
\end{code}
%
The addition of |Vectors| is defined indexwise.
%
Because indexwise addition is already our definition of addition for
functions (|g -> s|), we can simply reuse this definition.
%
(Likewise for |zero| and |negate|.)
%
This is what the |deriving| clause amounts to here.

We define right away the notation |a ! i| for the coefficient of the
canonical base vector |e i|, as follows:
\begin{code}
infix 9 !
(!) :: Vector s g -> g -> s
V f ! i = f i
\end{code}
%
We sometimes omit the constructor |V| and the indexing |(!)|,
treating vectors as functions without the |newtype|.

As discussed above, the |S| parameter in |Vector S| has to be a field
(|REAL|, or |CC|, or |Zp|, etc.)\footnote{The set |Zp| is the set of
  integers modulo |p|. We let the reader lookup the appropriate notion
  of division for it.} for values of type |Vector S G| to represent
elements of a vector space.

The cardinality of |G|, which we sometimes denote |card G|, is the number
of basis vectors, and thus the dimension of the vector space.
%
Often |G| is finite, and in the examples so far we have used indices
from \(G = \{0, \ldots, n\}\).
%
Thus the dimension of the space would be \(n+1\).

In Haskell finiteness of |G| can be captured by the conjunction of
|Bounded| (there is a minimum and a maximum element in |G|) and
|Enum|erable (there is a notion of enumeration from a given element of
|G|) and |Eq|.
%
Hence, the list of all elements of |G| can be extracted:
\begin{code}
type Finite g = (Bounded g, Enum g, Eq g)
finiteDomain :: Finite a => [a]
finiteDomain = [minBound..maxBound]
\end{code}

We know from \cref{sec:FunNumInst} that if |S| is an instance of
|AddGroup| then so is |G -> S|, with the pointwise definitions.
%
However, multiplication of vectors does not in general work
pointwise.
%
In fact, attempting to lift multiplication from the |Multiplicative|
class would give a homogenous multiplication operator |(*) :: v -> v
-> v|, but such an operator is not part of the definition of vector
spaces.
%
Consequently, vector spaces are in general \emph{not} rings.

Indeed, the scaling operator |(*^) :: s -> v -> v|, is inhomogenous:
the first argument is a scalar and the second one is a vector.
%
For our representation it can be defined as follows:

\pj{This should be a |VectorSpace| class instance.}
\jp{Investigate}
%{
%format *^^ = *^
\begin{code}
infixr 7 *^
(*^^) :: Multiplicative s => s -> Vector s g -> Vector s g
s *^^ V a = V (\i -> s * a i)
\end{code}
% Equivalent definition: s *^^ v = V (\i -> s * v ! i)
%}
%if False
\begin{code}
instance Field s => VectorSpace (Vector s g) s where (*^) = (*^^)
-- Overlapping instances:
-- instance VectorSpace v s => VectorSpace (Vector v g) s where (*^) = scaleVec
-- scaleVec :: VectorSpace v s => s -> Vector v g -> Vector v g
-- scaleVec s (V a) = V (\i -> s *^ a i)
instance Field s => VectorSpace s s where (*^) = (*)
-- Overlapping instances:
-- instance VectorSpace v s => VectorSpace (t->v) s where (*^) = scaleFun
-- scaleFun :: VectorSpace v s => s -> (t->v) -> (t->v)
-- scaleFun s f = \x-> s*^f x
\end{code}
%endif

%
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
is i j = if i == j then one else zero
\end{code}
It is 1 if its arguments are equal and 0 otherwise. Thus |e i| has
zeros everywhere, except at position |i| where it has a one.

This way, every |v : G -> S| is a linear combination of vectors |e i| where the coefficient of base vector |e i| is the scalar |v i|:
\begin{spec}
    v  ==  (v 0 *^ e 0) + ... + (v n *^ e n)
\end{spec}
To clarify, the above equation is a property of every vector |v|. Such
a vector can always be written as a linear combination of base
vectors. The coefficients come from applying |v| (seen as a function)
to the possible indices.
%
As we will work with many such linear combinations we introduce a
helper function |linComb|:
%
\begin{code}
linComb :: (Finite g, VectorSpace v s) => (g->s) -> (g->v) -> v
linComb a v = sum (map (\j -> a j *^ v j) finiteDomain)
\end{code}
Using |linComb| the characterising equation for vectors reads:
%
\begin{equation}
  \label{eq:vector-lincomb}
    |v == linComb v e|
\end{equation}
%if False
\begin{code}
linComb1 :: (Finite g, Ring s) => (g->s) -> (g->s) -> s
linComb1 as vs = sum [as j * vs j | j <- finiteDomain]

linComb2 :: (Finite g, VectorSpace v s) => (g->s) -> (g->v) -> v
linComb2 as vs = sum [as j *^ vs j | j <- finiteDomain]
\end{code}
%endif

\begin{exercise}
  Using the elements defined above, sketch the isomorphism between an
  abtract vector space and its representation. Recall the definition
  of isomorphism in \cref{sec:isomorphism}.
\end{exercise}

\section{Linear transformations}

As we have seen in earlier chapters, morphisms between structures are
often important.
%
Vector spaces are no different: if we have two vector spaces |Vector S
G| and |Vector S G'| for the same set of scalars |S|, we can study
functions |f : Vector S G -> Vector S G'|:
%
\begin{spec}
f v  =  f (v 0 *^ e 0 + ... + v n *^ e n)
\end{spec}
% that
It is particularly interesting to study such functions when they preserve the vector space structure: vector-space homomorphisms.
Such functions are more commonly called ``linear maps'', but
to avoid unnecessary confusion with the Haskell |map| function we will
refer to vector-space homomorphisms by the slightly less common name
``linear transformation''.
%
The function |f| is a linear transformation if it maps the operations
in |Vector S G| into operations in |Vector S G'| as follows:
\begin{spec}
f (u + v)   =  f u + f v
f (s *^ u)  =  s *^ f u
\end{spec}
Because |v = linComb v e = (v 0 *^ e 0 + ... + v n *^ e n)|, we also have:
%
\begin{spec}
f v =  f (  v 0 *^ e 0      + ... +  v n *^ e n)     = {- |f| is linear -}
    =       v 0 *^ f (e 0)  + ... +  v n *^ f (e n)  = {- def. of |linComb| -}
    =  linComb v (f . e)
\end{spec}
%if False
\begin{code}
linLaw :: (Finite g,
           Eq v',
           VectorSpace v s,
           VectorSpace v' s) =>
  (g -> v) -> (v -> v') -> (g -> s) -> Bool
linLaw e f v = f (linComb v e) == linComb v (f . e)

linLaw' ::
  (Finite g, Field s, VectorSpace v' s, Eq v') =>
  (Vector s g -> v') -> (g -> s) -> Bool
linLaw' f v = linLaw e f v

linLawS ::
  (Finite g, Field s, Eq s) =>
  (Vector s g -> s) -> (g -> s) -> Bool
linLawS = linLaw'

linLawV ::
  (Finite g, Field s, Eq (Vector s g')) =>
  (Vector s g -> Vector s g') -> (g -> s) -> Bool
linLawV = linLaw'

checkTypes :: (Finite g, Field s, Eq s) =>
  (Vector s g -> Vector s g') -> Vector s g -> g' -> [s]
checkTypes f (V v) g' =
  let m = f . e in
  [   f (V v) ! g'
  ,   (linComb v m) ! g'
  ,   linComb v (\g -> m g ! g')
  ,   linComb v (\g -> f (e g) ! g')
  ]


checkTypes2 :: (Finite g, Field s) => Matrix s g g -> g -> g -> [s]
checkTypes2 m k i = let V ek = e k in
  [ (mulMV m (V ek)) ! i
  , (linComb ek (transpose m)) ! i
  , m i ! k
  ]

\end{code}
%endif
%
But this means that we can determine
%
|f : Vector S G -> Vector S G'|
%
from just
%
|f . e : G -> Vector S G'|,
%
which has a much smaller domain.
%
Let |m = f . e|
That is, each |m i| is the image of the canonical base
vector |e i| through |f|.
Then
%
\begin{spec}
f v = linComb v m = v 0 *^ m 0 + ... + v n *^ m n
\end{spec}
%
Each of the |m k| is a |Vector S G'|, as is the resulting |f v|.
%
If we look at component |g'| of |f v| we have
%
\begin{spec}
  f v g'                           = {- as above -}

  (linComb v m) g'                 = {- |linComb|, |(*^)|, |(+)| are all linear -}

  linComb v (\g -> m g g')         {-"\ "-}
\end{spec}
That is, it suffices to know the behaviour of |f| on the basis vectors
to know its behaviour on the whole vector space.

%
It is enlightening to compare the above sum with the standard
vector-matrix multiplication.
%
Let us define |M| as follows:
%
\begin{spec}
M = [m 0 | ... | m n]     -- where |m : G -> Vector S G'|
\end{spec}
%
That is, the columns of |M| are |m 0| to |m n|, or, in other words, the columns of |M| are
|f (e i)|.
%
Every |m k| has |card G'| elements, and it has become standard to
write |M i j| to mean the |i|th element of the |j|th column, i.e., |M
i j = m j i|, so that, if we denote the usual matrix-vector
multiplication by |mulMV|:
%
\begin{spec}
(mulMV M v) i = linComb v (M i)
\end{spec}
% (M*v) i =
therefore, one has
%
\begin{spec}
(mulMV M v) i                            = -- by def. of |mulMV|
linComb v (M i)                          = -- by def. of |M i j|
linComb v (\j -> m j i)                  = -- earlier computation (linearity)
f v i
\end{spec}
%|f v g' = sum [m j g' * v j || j <- [0 .. n]]| with |g' = i|
%
If we take |Matrix| to be just a synonym for functions of type |G ->
Vector S G'|:
%
\begin{code}
type Matrix s g g' = g' -> Vector s g
\end{code}
%
then we can implement matrix-vector multiplication as follows:
%
\begin{code}
mulMV :: (Finite g, Field s) => Matrix s g g'  ->  Vector s g  ->  Vector s g'
mulMV m (V v)  =  linComb v (transpose m)

transpose :: Matrix s i j -> Matrix s j i
transpose m i = V (\j -> m j ! i)
\end{code}
%
%*TODO:
% I think we might end up with a mixture of definitions given in terms of
% |sum map| and definitions given in terms of list comprehension, at the
% end we should perhaps clean up and stick to one notation. At the
% specification/conceptual level we should perhaps stick to the usual
% $\sum$ notation.

In the terminology of the earlier chapters, we can see
|Matrix s g g'| as a type of syntax and the linear transformation (of
type |Vector S G -> Vector S G'|) as semantics.
%
With this view, |mulMV| is just another evaluation function from syntax to semantics.
%
However, again, given a fixed basis we have an isomorphism rather than
a mere homomorphism: for a given linear transformation, the matrix
representation is unique.
%
\begin{example}
  Consider the multiplication of a matrix with a basis vector:
\begin{spec}
(M * e k) ! i = (linComb (is k) (transpose M)) ! i = M i ! k
\end{spec}
%  (M * e k) i = sum [M i j * e k j | j <- [0 .. n]] = sum [M i k] = M i k
i.e., |e k| extracts the |k|th column from |M| (hence the notation
``e'' for ``extract'').
\end{example}
%

We have seen how a linear transformation |f| can be fully described by
a matrix of scalars, |M|.
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
|(M*)| is a linear transformation.
%
There is a bijection between these two sets.
%
Matrix-matrix multiplication is defined in order to ensure
associativity (note here the overloading of the operator |*|):
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

You may want to refer to Exercise~\ref{exc:Mstarhomomorphismcompose}, which asks you to work this out in detail.
%
% \jp{We do not note that |Matrix| form a category with mulMV being the composition and |e| as the identity because we have not talked about categories!}
Additionally, exercise~\ref{exc:MMmultAssoc} is about associativity of matrix-matrix multiplication.
%
\jp{Not sure what the difference is from the previous exercise.}

A simple vector space is obtained for |G = ()|, the singleton index
set.
%
In this case, the vectors |s : () -> S| are functions that can take
exactly one value as argument, therefore they have exactly one value:
|s ()|, so they are isomorphic with |S|.
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
\lnOnly{For the connection between matrices, linear transformations,
  and geometry, I warmly recommend binge-watching the ``Essence of
  linear algebra'' videos on youtube (start here:
  \url{https://www.youtube.com/watch?v=kjBOesZCoqc}).  }

%*TODO: Perhaps it would be interesting to show that some linear transformations
% can also be interpreted as changes of basis.

\section{Inner products}

An important concept is the inner product between vectors. We define
inner product space as a vector space equipped with an inner product,
as follows:
\begin{code}
class VectorSpace v s => InnerSpace v s where
  inner :: v -> v -> s
\end{code}

%
Inner products have (at least) two aspects.
%
First, they yield a notion of how ``big'' a vector is, the |norm|.
%
\begin{code}
sqNorm v = inner v v

norm v = sqrt (sqNorm v)
\end{code}
%
Additionally, the inner product often serves as a measure of how much
vectors are similar to (or correlated with) each other.

For two non-zero vectors |u| and |v|, we can define:
\begin{code}
similarity u v = inner u v / norm u / norm v
\end{code}
Dividing by the norms mean that |abs (similarity u v)| is at most |1|
--- the similarity is always in the interval |[-1,1]|.
%

For example, in Euclidean spaces, one defines the inner product to be
the product of the cosine of the angle between the vectors and their
norms. Consequently, similarity is the cosine of the angle betwen
vectors.

For this reason, one says that two vectors are orthogonal when their
inner product is |0| --- even in non-Euclidean spaces.


\paragraph{Dot product}

An often used inner product is the dot product, defined as follows:
\begin{code}
dot :: (Field s, Finite g) => Vector s g -> Vector s g -> s
dot (V v) (V w) = linComb v w
\end{code}

We should note that the dot product acts on the representations
(syntax). This means that it will \emph{change} dependending on the
basis chosen to represent vectors. Thus, the dot product is a
syntactic concept, and it should be clearly identified as such. This
can be somewhat counterintuitive, because so far it was fine to use
representations (they were unique given the basis). 
%
To further confuse matters, in Euclidean spaces (which are often used
as illustration) if the basis vectors are orthogonal, then the dot
product coincides with the inner product. But, according to our
methodology, one should start by defining a suitable inner product,
and then check if the dot product is equivalent to it. See
\cref{sec:inner-product-fourier} for an example.


\paragraph{Orthogonal transformations}

An important subclass of the linear transformations are those which
preserve the inner product.
\begin{spec}
  inner (f u) (f v) = inner u v
\end{spec}

So, in Euclidean spaces, such a transformation preserve angles. In the
context of linear algebra they are called orthogonal transformations.

\begin{exercise}
  Can you express this condition as a homomorphism condition?
  % H2(f,inner,inner)
\end{exercise}

Such transformations necessarily preserve the dimension of the space
(otherwise at least one base vector would be squished to nothing and
inner products involving it become zero).
% %
% When the dimension is preserved, one often uses the term ``linear
% operator''.
% %
The corresponding matrices are square.

\begin{exercise}
  Prove that orthogonal operators form a monoid with multiplication as an operator.
\end{exercise}

If angles are preserved what about distances?
%
An isometry |f| is a distance-preserving transformation:
%
\begin{spec}
  norm (f v) = norm v
\end{spec}
%
We can prove that |f| is orthogonal iff.\ it is an isometry.
%
The proof in the left-to-right direction is easy and left as an
exercise.
%
In the other direction one uses the equality:
\begin{spec}
  4 * inner u v = sqNorm (u + v) - sqNorm (u - v)
\end{spec}
%
In Euclidean spaces, this means that preserving angles and preserving
distances go hand-in-hand.

Orthogonal transformations enjoy many more useful properties: we have
barely scrached the surface here.
%
Among others, their rows (and columns) are orthogonal to each other.
%
The are also invertible (and so they form a group), and the inverse is
the (conjugate-) transpose of the matrix.
%
(In the context of a complex scalar field, one would use the word
``unitary'' instead of ``orthogonal'', but it's a straightforward
generalisation once we have define the appropriate inner product.)

\section{Examples of matrix algebra}

\subsection{Polynomials and their derivatives}

We have represented polynomials of degree |n+1| by the list of their
coefficients.
%
This is the same representation as the vectors represented by |n+1|
coordinates which we referred to in the introduction to this chapter.
%
This suggests that polynomials of degree |n| form a vector space, and
we could interpret that as |{0, ..., n} -> REAL| (or, more generally,
|Field a => {0, ..., n} -> a|).
%
The operations, |(+)| for vector addition and |(*^)| for vector
scaling, are defined in the same way as they are for functions.
%

To give an intuition for the vector space it is useful to consider the
interpretation of the canonical base vectors.
%
Recall that they are:
%
\begin{spec}
e i : {0, ..., n} -> REAL, e i j = i `is` j
\end{spec}
%
but how do we interpret them as polynomial functions?

When we represented a polynomial by its list of coefficients in \cref{sec:poly}, we saw
that the polynomial function |\x -> x^3| could be represented as
|[0,0,0,1]|, where |1| is the coefficient of |x^3|.

This representation suggests to use as canonical base vectors |e i|
the monomials |\x -> x^i|.  Representing the above list of
coefficients as a vector is then a matter of converting lists to
functions |{0, ..., n} -> REAL|). This way, the vector |\j -> if j ==
3 then 1 else 0| is equal to |\j -> 3 `is` j| or simply |e 3|.
%
Any other polynomial function |p| equals the linear combination of
monomials, and can therefore be represented as a linear combination of
our base vectors |e i|.
%
For example, |p x = 2+x^3| is represented by |2 *^ e 0 + e 3|.
%

The evaluator from the |Vector g s| representation to
polynomial functions is as follows:
%
%if False
\begin{code}
evalM :: G -> (REAL -> REAL)
evalM (G i) = \x -> x^i

evalP, evalP' :: Vector REAL G -> (REAL -> REAL)
evalP (V v) x = sum (map (\i -> v i * evalM i x) finiteDomain)

evalP' (V v) = linComb v evalM
instance VectorSpace (REAL->REAL) REAL where
  (*^) = scaleR2R
scaleR2R s f = (s*).f
\end{code}
%endif
\jp{In the latex code, there is an instance VectorSpace (REAL->REAL) REAL which we use! See the tex code. I have created a label and used it when we reference this space.
Should we show this code?}
\label{sec:functions-vector-space}
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

The associated matrix will be obtained by appling the linear
transformation to every base vector:
%
\begin{spec}
M = [ derive (e 0), derive (e 1), ..., derive (e n) ]
\end{spec}
%
where each |derive (e i)| has length |n|.
%
The vector |e (i + 1)| represents |\x -> x^(i + 1)| and thus we want
|derive (e (i + 1))| to represent the derivative of |\x -> x^(i + 1)|:
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

Exercise~\ref{exc:matrixIntegPoly}: write the matrix |In| associated
with integration of polynomials.

\subsection{\extraMaterial Inner product for functions and Fourier series}
\label{sec:inner-product-fourier}
We said before that the inner product yields a notion of norm and
similarity.
%
Can we use the dot product as inner product for power series (if the base is |e i = x ^ i|  )?
We could, but then it would not be very useful.
For example, it would not yield a useful notion of similarity between the represented function.
%
To find a more useful inner product, we can return to the semantics of
power series in terms of functions.
%
But for now we consider them over the restricted domain
$I = [-\pi,\pi]$.

Assume for a moment that we would define the inner product of functions
$u$ and $v$ as follows:
\[
  |innerF u v| = \int_I |(eval u x)*(eval v x)| dx
\]

Then, the norm of a function would be a measure of how far it gets from
zero, using a quadratic mean.
%
Likewise, the corresponding similarity measure corresponds to how much
the functions ``agree'' on the interval.
%
That is, if the signs of |eval u| and |eval v| are the same on a
sub-interval |I| then the integral is positive on |I|, and negative if
they are different.

As we suspected, using |inner = innerF|, the straightforward
representation of polynomials as list of coefficients is not an
orthogonal basis.
%
There is, for example, a positive correlation between |x| and |x^3|.

If we were using instead a set of basis polynomials |bn| which are
orthogonal using the above definition of |inner|, then
we could simply let |inner = dot|,
and this would be a lot more efficient than to compute the integral by the following series of steps:
1) compute the product using |mulPoly|, 2) integrate using
|integ|, 3) use |eval| on the end points of the domain.

Let us consider as a base the functions |bn x = sin (n*x)|, prove that
they are orthogonal.

We first use trigonometry to rewrite the product of bases:
\begin{spec}
   2 * (bi x * bj x)
=  2 * sin (i*x) * sin (j*x)
=  cos ((i-j)*x) - cos ((i+j)*x)
\end{spec}
%
Assuming |i/=j|, we can take the indefinite integral, and find:
%{
%format integral = "\int"
\begin{spec}
sin ((i-j)*x)/(i-j) - sin ((i+j)*x) / (i+j) + K
\end{spec}
%}
Taking the definite integral over the domain |I| yields:
\begin{spec}
  2 * (inner bi bj) =
       sin ((i-j)*π)/(i-j) - sin ((i+j)*π) / (i+j) + K
     - sin ((i-j)*π)/(i-j) + sin ((i+j)*π) / (i+j) - K
\end{spec}

%
But |sin (k*pi) = 0| for any integer |k|, and thus |inner bi bj = 0| 

We can now compute |sqNorm| of |bi|.
%
Trigonometry says:
\begin{spec}
   2 * (bi x * bi x)
=  2 * sin (i*x) * sin (i*x)
=  cos (0*x) - cos (2*i*x)
=  1 - cos (2*i*x)
\end{spec}
%
When taking the integral on |I|, the cosine disappears using the same
argument as before, and there remains: |2*sqNorm bi = 2 pi|.
%
Thus to normalise the base vectors we need to scale them by |1 / sqrt
pi|.
%
In sum |bi x = sin (i*x)/sqrt pi| is an orthonormal basis:

\begin{spec}
  bi `innerF` bj = is i j
\end{spec}

As interesting as it is, this basis does not cover all functions over
|I|.
%
To start, |eval bi 0 == 0| for every |i|, and thus linear combinations
can only ever be zero at the origin.

But if we were to include |cos (n*x) / sqrt pi| in the set of base
vectors, it would remain orthogonal, and the space would cover all
periodic functions with period $2 \pi$.
%
A representation of a function in this basis is called the Fourier series.
%
Let us define a meaningful index (|G|) for the basis:
%
\begin{code}
data Periodic where
  Sin  :: ℕ -> Periodic
  Cos  :: ℕ -> Periodic
\end{code}
%
A useful property of an orthonormal basis is that its representation
as coefficients can be obtained by taking the inner product with each
base vectors.
%
Indeed, using \cref{eq:vector-lincomb} as a starting point, we can calculate:\footnote{The proof can be easily adapted to infinite sums.}
\begin{spec}
     v           == linComb v b
=>   v           == sum [vi *^ bi | i <- finiteDomain]
=>   v `inner` bj  == sum [vi *^ bi | i <- finiteDomain] `inner` bj
=>   v `inner` bj  == sum [vi *^ (bi `inner` bj) | i <- finiteDomain]
=>   v `inner` bj  == sum [vi *^ is i j | i <- finiteDomain]
=>   v `inner` bj  == vj
\end{spec}

Thus, in our application, given a periodic function |f|, one can
compute its Fourier series by taking the |innerF| product of it with
each of |sin (n*x) / sqrt pi| and |cos (n*x) / sqrt pi|.

\begin{exercise}
Derive |derive| for this representation.
\end{exercise}

\subsection{Simple deterministic systems (transition systems)}

Simple deterministic systems are given by endo-functions%
\footnote{An \emph{endo-function} is a function from a set |X| to
  itself: |f : X -> X|.}
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
Now, |{0, 1}| is not a field w.r.t. the standard arithmetical
operations (it is not even closed w.r.t. addition), and the standard
trick to workaround this issue is to extend the type of the functions
to |REAL|.

The canonical basis vectors are, as usual, |e i = V (is i)|.
%
Each |e i| is the characteristic function of a singleton set, |{i}|.


We can interpret |e (next 0), ..., e (next 6)| as the images of the
basis vectors |e 0, ..., e 6| of |Vector REAL G| under the
transformation
%
\begin{spec}
f :: Vector REAL G -> Vector REAL G
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
But that is not quite accurate: if we start with |{3, 4}|, we no longer
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
This means that we do not have a vector space, but rather a
\emph{module}.
%
One can still do a lot with modules:
%
for example the definition of matrix multiplication only demands a
|Ring| rather than a |Field| (and none of the |VectorSpace| laws
demand scalar division).
%
Therefore, having just a module is not a problem if all we want is to
compute the evolutions of possible states, but we cannot apply most of
the deeper results of linear algebra.
%
\footnote{For instance, such deeper result would give ways to easily
  compute the stable state of a dynamic system.}

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
% newtype G = G Int deriving (Eq, Show, Additive, Multiplicative, AddGroup)
\begin{code}
newtype G = G Int deriving (Eq, Show)

instance Bounded G where
  minBound  =  G 0
  maxBound  =  G 6

instance Enum G where
  toEnum          =  G
  fromEnum (G n)  =  n
\end{code}
%
Note that the |Ring G| instance is given just for convenient notation
(integer literals): vector spaces in general do not rely on any
numeric structure on the indices (|G|).
%
The transition function has type |G -> G| and the following
implementation:
%
\begin{code}
next1 :: G -> G
next1 (G 0)  = G 1;  {-"\qquad"-}  next1 (G 1) =  G 3;
next1 (G 2)  = G 5;                next1 (G 3) =  G 6;
next1 (G 4)  = G 6;                next1 (G 5) =  G 4;
next1 (G 6)  = G 5
\end{code}
%
Its associated matrix is
%
\begin{spec}
m g'                                 {-"\qquad"-}  {- |m| is the matrix version of |f| -}
V (\ g -> (f (e g))           ! g')                {- by the spec. of |f| -}
V (\ g -> (e (next1 g))       ! g')                {- by def. of |e| -}
V (\ g -> (V (is (next1 g)))  ! g')                {- by def. of |(!)| -}
V (\ g -> is (next1 g) g')                         {- |is| is symmetric -}
V (\ g -> is g' (next1 g))                         {- by def. of |(.)| -}
V (is g' . next1)
\end{spec}
%
%if False
\begin{code}
proofSteps m g' =
  let  f :: Vector REAL G -> Vector REAL G
       f (V v) = linComb v (e . next1)
  in
  [ m g'                                   {- |m| is the matrix version of |f| -}
  , V (\ g -> (f (e g))           ! g')    {- by the spec. of |f| -}
  , V (\ g -> (e (next1 g))       ! g')    {- by def. of |e| -}
  , V (\ g -> (V (is (next1 g)))  ! g')    {- by def. of |(!)| -}
  , V (\ g -> is (next1 g) g')
  , V (\ g -> is g' (next1 g))
  , V (is g' . next1)
  ]

testProofSteps = proofSteps m1
\end{code}
%endif
%
Thus we can implement |m| as:
%
\begin{code}
m1 :: Ring s => G -> Vector s G
m1 g' = V (is g' . next1)
\end{code}

%if False
Test:
%
\begin{code}
t1' :: Vector REAL G
t1'  = mulMV m1 (e (G 3) + e (G 4))
t1   = toL t1'               -- |[0,0,0,0,0,0,2]|
\end{code}
%endif

%**TODO (NiBo):
%if False
% This could go to into the file for the live sessions:
%
\begin{code}
poss :: (Finite g, Field s) => Int -> Matrix s g g -> Vector s g -> [Vector s g]
poss n m v = take (n + 1) (iterate (mulMV m) v)

testPoss0 :: [Vector REAL G]
testPoss0 = poss 6 m1 (e (G 3) + e (G 4))
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

testPoss1 = poss1 6 next1 [G 3, G 4]

poss1' :: (Eq a) => Int -> (a -> a) -> [a] -> [[a]]
poss1' n next xs = take (n + 1) (iterate (nub . map next) xs)

testPoss1' = poss1' 6 next1 [G 3, G 4]
\end{code}
%
% *DSLsofMath.W07> |poss1 6 next1 [3, 4]|
%
%endif False


\subsection{Non-deterministic systems}
\label{sec:NonDetSys}

Another interpretation of the application of |M| to characteristic
functions of a subset is the following: assuming that all I know is
that the system is in one of the states of the subset, where can it
end up after one step?
%
(This assumes the |max|-|min| algebra as above.)
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
Every node can be the source of zero, one, or many arrows.

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

\paragraph{Implementation:}
%
The transition relation has type |G -> (G -> Bool)|:
%
\begin{code}
f2 :: G -> (G -> Bool)
f2 (G 0) (G g)      =   g == 1 || g == 2
f2 (G 1) (G g)      =   g == 3
f2 (G 2) (G g)      =   g == 4 || g == 5
f2 (G 3) (G g)      =   g == 6
f2 (G 4) (G g)      =   g == 1 || g == 6
f2 (G 5) (G g)      =   g == 4
f2 (G 6) (G g)      =   False
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
Even though |Bool| is not a |Field| (not even a |Ring|) the
computations we need go through with these instances:
%
\begin{code}
instance Additive Bool where
  zero    =  False
  (+)     =  (||)

instance AddGroup Bool where
  negate  =  error "negate: not used"

instance Multiplicative Bool where
  one     =  True
  (*)     =  (&&)

instance MulGroup Bool where
  recip   =  id
\end{code}

As a test we compute the state after one step from ``either 3 or 4'':
%
\begin{code}
t2' = mulMV m2 (e (G 3) + e (G 4))
t2 = toL t2'  -- |[False,True,False,False,False,False,True]|
\end{code}

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

\subsection{Stochastic systems}
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
can compute the next one by using the \emph{total probability
  formula}, which can be expressed as
%
\begin{spec}
    p a = sum [p (a | b) * p b | b <- G]
\end{spec}
%
We study probability extensively in 
\cref{ch:probability-theory},
but for now, let's just remark that the notation is extremely suspicious.
%
|(a || b)|, which is usually read ``|a|, given |b|'', is clearly not
of the same type as |a| or |b|, so cannot really be an argument to
|p|.
%
Additionally, the |p a| we are computing with this formula is not the
|p a| which must eventually appear in the products on the right hand
side.
%
% We do not know how this notation came about\jp{According to Jaynes in
%   "Probability Theory, The Logic of Science, p. 17. The notation A||B
%   for "A given B" is attributable to Keynes (1921)."}: it is neither
% in Bayes' memoir, nor in Kolmogorov's monograph.\jp{citations needed}

In any case, at this stage, what we need to know is that the
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
%
Reverse again the arrow from 2 to 4 (so that |2->5| has probability
|1|, |4->2| probability |0.7|, |4->6| and |4->1| have probability
|0.15| each.
%
What can you say about the long-term behaviour of the system now?

Exercise~\ref{exc:StocExample1Impl}: Implement the example.
%
You will need to define:

The transition function (giving the probability of getting to |g'| from |g|)
%
\begin{code}
f3 :: G -> Vector REAL G
\end{code}
%
and the associated matrix
%
\begin{code}
m3 ::  G -> Vector REAL G
\end{code}
(We want only |G -> Vector [0, 1] G|, using the unit interval in place of |REAL|.)


%**TODO (NiBo):
% This could go to into the file for the live sessions:

%if False
\begin{code}
mkV :: Eq g => [(g,s)] -> s -> Vector s g
mkV tab def = V (\g -> maybe def id (lookup g tab))

-- f3 :: G -> Vector REAL G
f3 (G 0) = mkV [(G 1, 0.4), (G 2, 0.6)] 0
f3 (G 1) = mkV [(G 3, 1.0)] 0
f3 (G 2) = mkV [(G 4, 0.7), (G 5, 0.3)] 0
f3 (G 3) = mkV [(G 6, 1.0)] 0
f3 (G 4) = mkV [(G 1, 0.5), (G 6, 0.5)] 0
f3 (G 5) = mkV [(G 4, 1.0)] 0
f3 (G 6) = mkV [(G 6, 1.0)] 0

f3' :: G -> Vector REAL G
f3' (G 0) = mkV [(G 1, 0.4), (G 2, 0.6)] 0.0
f3' (G 1) = mkV [(G 3, 1.0)] 0.0
f3' (G 2) = mkV [(G 5, 1.0)] 0.0
f3' (G 3) = mkV [(G 6, 1.0)] 0.0
f3' (G 4) = mkV [(G 1, 0.4), (G 6, 0.4), (G 2, 0.2)] 0.0
f3' (G 5) = mkV [(G 4, 1.0)] 0.0
f3' (G 6) = mkV [(G 6, 1.0)] 0.0

m3 g' = V (\ g -> (f3 g) ! g')
\end{code}

*DSLsofMath.W07> |last (poss 6 m3 (e (G 0)))|

%endif


% Perhaps as an exercise to show how the notion of |Vector S| can
% be applied to seamlessly treat dynamical systems (contrast the
% implementation of |poss| with those of |poss1|, |poss2| and |poss3| for
% different dynamical systems):
%
% \begin{code}
% next3 :: G -> [(G, REAL)]
% next3 (G 0) = [(1,0.4), (2,0.6)]
% next3 (G 1) = [(3,1.0)]
% next3 (G 2) = [(4,0.7), (5,0.3)]
% next3 (G 3) = [(6,1.0)]
% next3 (G 4) = [(1,0.5), (6,0.5)]
% next3 (G 5) = [(4,1.0)]
% next3 (G 6) = [(6,1.0)]
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


\subsection{\extraMaterial Quantum Systems}

Instead of real numbers for probabilities, we could consider using
complex numbers --- one then speaks of ``amplitudes''.
%
An amplitude represented by a complex number |z| is converted to a
probability by taking the square of the modulus of |z|:
\begin{spec}
p z = conj z * z
\end{spec}

We can then rewrite the law of total probability as follows:
\begin{spec}
     sum [p!i | i <- finiteDomain]
=    sum [conj (z!i) * (z!i) | i <- finiteDomain]
=    inner z z
\end{spec}

Indeed, for 
spaces with complex scalars, one should conjugate
coefficients (of an orthonormal basis) when computing the inner products.
%
Hence, rather conveniently, the law of total probability is replaced
by conservation of the norm of state vectors.
%
In particular, norms are conserved if the transition matrix is
unitary.

The unitary character of the transition matrix defines valid systems
from the point of view of quantum mechanics.
%
Because all unitary matrices are invertible, it follows that all
quantum mechanical systems have an invertible dynamics.
%
Furthermore, the inverted matrix is also unitary, and therefore the
inverted system is also valid as a quantum dynamical system.

Here is an example unitary matrix
  \[
  M =
  \bordermatrix{
         & c_0 & c_1 & c_2 & c_3 & c_4 & c_5 & c_6 \cr
    r_0  &  0  &  0  &  1  &  0  &  0  &  0  &  0 \cr
    r_1  &  1  &  0  &  0  &  0  &  0  &  0  &  0 \cr
    r_2  &  0  &  1  &  0  &  0  &  0  &  0  &  0 \cr
    r_3  &  0  &  0  &  0  & \sqrt 2/2 &-\sqrt 2/2 &  0  &  0 \cr
    r_4  &  0  &  0  &  0  & \sqrt 2/2 & \sqrt 2/2 &  0  &  0 \cr
    r_5  &  0  &  0  &  0  &  0  &  0  &  1/2  &  \sqrt 3 / 2 \cr
    r_6  &  0  &  0  &  0  &  0  &  0  &  \sqrt 3 / 2  &  -1/2 \cr
  }
\]

In this example the amplitudes of states 0, 1, and 2 are permuted at
every step.
%
States 3 and 4 get mixed into one another, and one can note that the
sign of their amplitudes may get inverted.
%
A similar situation happens between states 5 and 6, but at a higher
rate.

\section{\extraMaterial Monadic dynamical systems}

This section is meant to give perspective for the readers who are
already familiar with monads. Even though it can be safely skipped, it
presents a useful unified view of the previous sections which could
help understanding the material.

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
  The transition function has the type |f : G -> (G -> REAL)|, the
  structure of the target is the probability distributions over |G|.
\item quantum: given an observable state, we compute a
  superposition of possible orthogonal future states. 
\end{itemize}

Therefore:
%
\begin{itemize}
\item deterministic: |f : G -> Id G|
\item non-deterministic: |f : G -> Powerset G|, where |Powerset G = G -> {0, 1}|
\item stochastic: |f : G -> Prob G|, where |Prob G = G -> [0, 1]|
\item quantum: |f : G -> Super G|, where |Super G = G -> Complex|. (Additionally |f| must be invertible)
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
state, and then collect all these hypothetical possible future states
in some way that takes into account the initial uncertainty
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

Exercise: write |Monad| instances for |Id|, |Powerset|, |Prob|, |Super|.


\section{\extraMaterial The monad of linear algebra}
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

instance Field s => FinMon (Vector s) where
  embed  = embedFinM
  bind   = bindFinM

embedFinM :: (Eq a, Ring s) => a -> Vector s a
embedFinM g = V (is g)

bindFinM :: (Field s, Finite a) => Vector s a -> (a -> Vector s b) -> Vector s b
bindFinM (V v) f  =  V (\ g' -> linComb v (\g -> f g ! g'))
\end{code}
%
Note that, if |v :: Vector S G| and |f :: G -> Vector S G'| then
both |func f v| and |bind v f| are of type |Vector S G'|.
%
How do these operations relate to linear algebra and matrix-vector
multiplication?

Remember that |e g| is that vector whose components are zero except
for the |g|th one which is one. In other words
%
\begin{spec}
    e g = V (is g) = embed g
\end{spec}
%
and thus |embed = e|.
%
In order to understand how matrix-vector multiplication relates to the
monadic operations, remember that matrices are just functions of type
|G -> Vector S G'|:
%
\begin{spec}
  type Matrix s g g' = g' -> Vector s g
\end{spec}
% a -> Vector s b = Matrix s b a
According to our earlier definition, we can rewrite matrix-vector
multiplication in terms of |linComb|
%
\begin{spec}
  mulMV m (V v)

= {- earlier definition -}

  linComb v (transpose m)
\end{spec}
%if False
\begin{code}
testBindCalc1 :: (Finite i, Field s) => Matrix s i j -> Vector s i -> [Vector s j]
testBindCalc1 m (V v) =
  [ mulMV m (V v)
  , linComb v (transpose m)
  ]
testBindCalc2 m (V v) =
  [ mulMV m (V v)
  , linComb v (transpose m)
  , linComb v (\i -> V (\j -> m j ! i))
  , linComb v (\i -> V (\j -> m j ! i))
  , V (\ j -> linComb v (\i -> f i ! j))
  ]
  where f = transpose m

-- ``poor man's proofs'' of the |linComb|-|V| lemma
qq :: (Finite j, Field s) => (j -> s) -> (j -> Vector s i) -> [Vector s i]
qq a v = let v0 = linComb a v in map (\v -> v - v0) $
  [ linComb a v                                             -- def. |linComb|
  , sum (map (\j -> a j *^ v j) finiteDomain)               -- def. |(*^)|
  , sum (map (\j -> V (\i -> a j * v j ! i)) finiteDomain)  -- lemma |qq2|
  ]
  ++
    qq2 a (\j i -> v j ! i) finiteDomain
  ++
  [ V (\i -> sum (map (\j -> a j *  v j ! i) finiteDomain)) -- def. |(*^)| for |REAL|
  , V (\i -> sum (map (\j -> a j *^ v j ! i) finiteDomain)) -- def. |linComb|
  , V (\i -> linComb a (\j -> v j ! i))
  ]

qq2 :: Ring s => (j->s) -> (j->i->s) -> [j] -> [Vector s i]
qq2 a f js = let y j = \i -> a j * f j i in
  [ sum (map (\j -> V (\i -> a j * f j i))  js)  -- introduce shorthand |y|
  , sum (map (\j -> V (y j))                js)  -- def. map for (:)
  , V (\i -> sum (map (\j -> y j i)         js)) -- def. |y|
  , V (\i -> sum (map (\j -> a j * f j i)   js))
  ]

qq3 :: Additive s => (j -> i -> s) -> [j] -> [Vector s i]
-- qq3 y js =
--       sum (map (\j -> V (y j)) js)
--    == V (\i -> sum (map (\j -> y j i) js))
qq3 y [] = let q1 j = V (\i -> y j i)
               q2 i = \j -> y j i      in
  [ sum (map q1 [])                -- def. |map| for |[]|
  , sum []                         -- def. |sum| for |[]|
  , zero                           -- def. |zero| for |Vector|
  , V zero                         -- def. |zero| for functions
  , V (\i -> zero)                 -- def. |sum| for |[]|
  , V (\i -> sum [])               -- def. |map| for |[]|
  , V (\i -> sum (map (q2 i) []))  -- def. |map| for |[]|
  ]
qq3 y (j:js) =
  [ sum (map (\j -> V (y j)) (j:js))                -- def. |map| for |(:)|
  , sum (V (y j) : map (\j -> V (y j)) js)          -- def. |sum| for |(:)|
  , V (y j) + sum (map (\j -> V (y j)) js)          -- ind. hyp
  , V (y j) + V (\i -> sum (map (\j -> y j i) js))  -- def. |(+)| for Vector
  , V (y j  +  \i -> sum (map (\j -> y j i) js))    -- def. |(+)| for functions
  , V (\i -> y j i + sum (map (\j -> y j i) js))    -- def. |sum| for |(:)|
  , V (\i -> sum (y j i : map (\j -> y j i) js))    -- def. |map| for |(:)|
  , V (\i -> sum (map (\j -> y j i) (j:js)))
  ]
\end{code}
%endif
%
Now we have:
%
\begin{spec}
  mulMV (transpose m) (V v)

= {- def. of |mulMV| -}

  linComb v (transpose (transpose m))

= {- Property of |transpose| -}

  linComb v m

= {- |linComb|-|V| lemma -}

  V (\i -> linComb v (\j -> m j ! i))

= {- def. of |bind| -}

  bind (V v) m
\end{spec}
%
Thus we see that |bind v f| is ``just'' a matrix-vector
multiplication.

The |linComb|-|V| lemma says that for |a :: j -> s| and |v :: j ->
Vector s i| we have |linComb a v == V (\i -> linComb a (\j -> v j !
i))|.
%
The proof uses the definitions of |linComb|, |(*^)|, and the
|Additive| instances for |Vector| and functions but is omitted here
for brevity.
\pj{Show parts of the proof which is in |qq|, |qq2|, |qq3| above.}

% Perhaps for extra exercises:\jp{clarify status}
%
% It is worth pointing out the role of |f| in |func f v|.
% %
% We can rewrite the |g'|th component of |func f v| in terms of the dot
% product
% %
% \begin{spec}
%   dot v (V (\ g -> is g' (f g)))
% =
%   dot v (V (is g' . f))
% \end{spec}
% %
% This shows that the role of |f| in |func f v| is that of
% re-distributing the values of |v| onto the new vector.
%
% \begin{exercise}
%   show that if |w = func f v| then the sum of the components of |w| is
%   equal to the sum of the components of |v|.
% \end{exercise}
%
%
% \begin{exercise}
% \begin{enumerate}
% \item Prove that the functor laws hold, i.e.
% %
% \begin{spec}
% func id       =  id
% func (g . f)  =  func g . func f
% \end{spec}
%
% \item Prove that the monad laws hold, i.e.
% %
% \begin{spec}
% bind v return      =  v
% bind (return g) f  =  f g
% bind (bind v f) h  =  bind v (\ g' -> bind (f g') h)
% \end{spec}
%
% \item What properties of |S| have you used to prove these properties?
% %
%   Define a new type class |GoodClass| that accounts for these (and
%   only these) properties.
% \end{enumerate}
% \end{exercise}

%*TODO: Proving that |func| preserves composition and that |bind|
% associates gets very messy if one directly operates with their
% definitions.
% %
% It is probably simpler to go back to standard linear algebra notation
% (with indexed |sum|, etc.) which in a sense seems to defeat the purpose
% of focusing on the syntax.

\section{Associated code}

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
multiplication:\jp{Do we really need to have this? In linear algebra,
  linear spaces are a more primitive concept than inner product
  spaces.  So defining matrix multiplication in terms of dot product
  is bad from an algebraic perspective.  It would be ok to define in
  terms of |linComb| if we really want some refactoring. We could keep
  this as an example of a bad idea of a refactoring; but since we're
  already in the ``associated code'' section, I suggest to just remove
  this.

  Decision: remove.
}
%
\begin{code}
dot' ::  (Finite g, Ring s) =>
         (g->s) -> (g->s) -> s
dot' v w = sum (map (v * w) finiteDomain)
\end{code}
%
Note that |v * w :: g -> s| is using the function instance of |Multiplicative|.

Using it we can shorten the definition of |mulMV|

\begin{spec}
  mulMV m v g'
= -- Classical definition
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
%if False
\begin{code}
linComb' :: (Finite g, VectorSpace v s) => (g->s) -> (g->v) -> v
linComb' a v = sum (map (\j -> a j *^ v j) finiteDomain)

mulMV'' ::  (Finite g, Field s) =>
           Mat s g g' ->  Vec s g  ->  Vec s g'
mulMV'' m v  =  linComb' v . m

checkTypes3 :: (Finite b, Field s) => Mat s a b -> Mat s b c -> a -> [Vec s c]
checkTypes3 m1 m2 i =
  [ getCol (mulM m2 m1) i
  , evalMV m2 (getCol m1 i)
  ]

mulM :: (Finite b, Field s) => Mat s b c -> Mat s a b -> Mat s a c
mulM m2 m1 = flip (evalMV m2 . flip m1)

evalMV :: (Finite a, Field s) => Mat s a b -> Vec s a -> Vec s b
evalMV m v = linComb v . m

\end{code}
%endif

Similarly, we can define matrix-matrix multiplication:
%
\begin{code}
mulMM' ::  (Finite b, Ring s) =>
           Mat s b c   ->  Mat s a b  ->  Mat s a c
mulMM' m1 m2 = \r c -> mulMV' m1 (getCol m2 c) r

transpos :: Mat s g g' -> Mat s g' g
transpos m i j = m j i

getCol :: Mat s g g' -> g -> Vec s g'
getCol = transpos

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
