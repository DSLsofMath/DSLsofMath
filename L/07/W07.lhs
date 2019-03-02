\section{Matrix algebra and linear transformations}
\label{sec:LinAlg}

Often, especially in engineering textbooks, one encounters the
definition: a vector is an \(n+1\)-tuple of real or complex numbers,
arranged as a column:
%
\[v = \colvec{v}\]
%
Other times, this is supplemented by the definition of a row vector:
%
\[v = \rowvec{v}\]
%
The |vi|s are real or complex numbers, or, more generally, elements of
a \emph{field} (analogous to being an instance of |Fractional|).
%
Vectors can be added point-wise and multiplied with scalars, i.e.,
elements of the field:
%
\[v + w = \colvec{v} + \colvec{w} = \colvecc{v_0 + w_0}{v_n + w_n}\]
%
\[s * v = \colvecc{s*v_0}{s*v_n}\]
%**TODO: use a different symbol for scaling than field multiplication
The scalar |s| scales all the components of |v|.

But, as you might have guessed, the layout of the components on paper
(in a column or row) is not the most important feature of a vector.
%
In fact, the most important feature of vectors is that they can be
\emph{uniquely} expressed as a simple sort of combination of other
vectors:
%
\[v = \colvec{v} = v_0 * \colveccc{1 \\ 0 \\ \vdots \\ 0} +
                   v_1 * \colveccc{0 \\ 1 \\ \vdots \\ 0} + \cdots +
                   v_n * \colveccc{0 \\ 0 \\ \vdots \\ 1}
\]
%
We denote by
%
\[e_k = \colveccc{0\\\vdots\\0\\1 \makebox[0pt][l]{\qquad $\leftarrow$ position $k$} \\0\\\vdots\\0}\]
%
the vector that is everywhere |0| except at position |k|, where it is
|1|, so that |v = v0 * e0 + ... + vn * en|.

We could represent a vector |v| in terms of any set of \emph{basis}
vectors |{b0, ..., bn}| which are \emph{linearly independent}:
%
\begin{spec}
  (v0 * b0 + ... + vn * bn = 0) <=> (v0 = ... = vn = 0)
\end{spec}
%
The specification warrants that any vector has a unique representation
and it is easy to see that |{e0, ..., en}| fulfils the specification.

The algebraic structure that captures a set of vectors, with zero,
addition, and scaling is called a \emph{vector space}.
%
For every field |S| of scalars and every set |G| of indices, the set
|Vector S G = G -> S| can be given a vector space structure.

%*TODO: Perhaps cut this "noisy" intro
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DSLsofMath.W07 where
import DSLsofMath.FunNumInst
type REAL = Double
\end{code}

\subsection{Vectors as functions}

There is a temptation to model vectors by lists or tuples, but a more
general (and conceptually simpler) way is to view them as
\emph{functions} from a set of indices |G|:
%
\begin{code}
newtype Vector s g    = V (g -> s) deriving Num
\end{code}
%**TODO Check and perhaps remove "deriving Num"

As discussed, the |S| parameter in |Vector S| has to be a field (|REAL|,
or |Complex|, or |Zn|, etc.) for values of type |Vector S G| to
represent elements of a vector space.

Usually, |G| is finite, i.e., |Bounded| and |Enumerable| and in the
examples so far we have used indices from \(G = \{0, \ldots, n\}\).
%
We sometimes use |card G| to denote the \emph{cardinality} of the set
|G|, the number of elements (\(n+1\) in this case).

We know from the previous lectures that if |S| is an instance of
|Num|, |Fractional|, etc. then so is |G -> S|, with the pointwise
definitions.
%
In particular, the instance declarations for |+|, multiplication, and
embedding of constants, give us exactly the structure needed for the
vector operations.
%
For example
%
\begin{spec}
    s * v                      =  {- |s| is promoted to a function -}

    const s * v                =  {- |Num| instance definition -}

    \ g -> (const s) g * v g   =  {- definition of |const| -}

    \ g -> s * v g
\end{spec}

The canonical basis vectors are then
%
\begin{spec}
    e i  :  G -> S,    e i g = i `is` g
\end{spec}
%
and every
%
\begin{spec}
    v : G -> S
\end{spec}
%
is trivially a linear combination of vectors |e i|:
%
\begin{spec}
    v =  v 0 * e 0 + ... + v n * e n
\end{spec}

Implementation:
%
\begin{code}
is :: (Eq g, Num s) => g -> g -> s
is a b = if a == b then 1 else 0

e :: (Eq g, Num s) => g -> Vector s g
e g = V (is g)
\end{code}
%
In linear algebra textbooks, the function |is| is often referred to as
the Kronecker-delta function and |is i j| is written $\delta_{i,j}$.
\subsection{Functions on vectors}

As we have seen in earlier chapters, morphisms between structures are often important.
%
Vector spaces are no different: if we have two vector spaces |Vector S
G| and |Vector S G'| for the same set of scalars |S|, we can study
functions |f : Vector S G -> Vector S G'|:
%
\begin{spec}
f v  =  f (v 0 * e 0 + ... + v n * e n)
\end{spec}
%
For |f| to be a ``good'' function it should translate the operations
in |Vector S G| into operations in |Vector S G'|, i.e., should be a
homomorphism:
%
\begin{spec}
f v =  f (v 0 * e 0 + ... + v n * e n) = v 0 * f (e 0) + ... + v n * f (e n)
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
a much ``smaller'' function.
%
Let |m = f . e|.
%
Then
%
\begin{spec}
f v =  v 0 * m 0 + ... + v n * m n
\end{spec}
%
Each of |m k| is a |Vector S G'|, as is the resulting |f v|.
%
We have
%
\begin{spec}
  f v g'                                             = {- as above -}

  (v 0 * m 0 + ... + v n * m n) g'                   = {- |*| and |+| for functions are def. pointwise -}

  v 0 * m 0 g' + ... + v n * m n g'                  = {- using |sum|, and |(*)| commutative -}

  sum [m j g' * v j | j <- [0 .. n]]
\end{spec}

Implementation:
%
This is almost the standard vector-matrix multiplication:
%
\begin{spec}
M = [m 0 | ... | m n]     -- where |m : G -> Vector S G'|
\end{spec}
%
The columns of |M| are the images of the canonical base vectors |e i|
through |f| (or, in other words, the columns of |M| are |f (e i)|).
%
Every |m k| has |card G'| elements, and it has become standard
to use |M i j| to mean the |i|th element of the |j|th column, i.e., |M i
j = m j i|, so that, with the usual matrix-vector multiplication
%
\begin{spec}
  (M * v) i = sum [M i j * v j | j <- [0 .. n]]
\end{spec}
%
one has
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
we can implement matrix-vector multiplication as:
%
\begin{code}
mulMV ::  (Finite g, Num s) => Matrix s g g'  ->  Vector s g  ->  Vector s g'
mulMV m v  = V (\g' -> sumV (m g' * v))

sumV :: (Finite g, Num s) => Vector s g -> s
sumV (V v) = sum (map v finiteDomain)
\end{code}
%
As already mentioned, here |Finite| means |Bounded| and |Enumerable|:
%
\begin{code}
class (Bounded g, Enum g, Eq g) => Finite g  where
\end{code}

%*TODO:
% I think we might end up with a mixture of definitions given in terms of
% |sum map| and definitions given in terms of list comprehension, at the
% end we should perhaps clean up and stick to one notation. At the
% specification/conceptual level we should perhaps stick to the usual
% $\sum$ notation.

Note that in the terminology of the earlier chapter we can see |Matrix
s g g'| as a type of syntax and the linear transformation (of type
|Vector S G -> Vector S G'|) as semantics.
%
With this view, |mulMV| is just another |eval :: Syntax -> Semantics|.
%

Example:
%
\begin{spec}
(M * e k) i = sum [M i j * e k j | j <- [0 .. n]] = sum [M i k] = M i k
\end{spec}
%
i.e., |e k| extracts the |k|th column from |M| (hence the notation
``e'' for ``extract'').

We have seen how a homomorphism |f| can be fully described by a matrix
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
|(M*)| is a linear transformation.
%
Matrix-matrix multiplication is defined in order to ensure that
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

Exercise~\ref{exc:MMmultAssoc}: show that matrix-matrix multiplication is associative.

Perhaps the simplest vector space is obtained for |G = ()|, the
singleton index set.
%
In this case, the vectors |s : () -> S| are functions that can take
exactly one argument, therefore have exactly one value: |s ()|, so
they are often identified with |S|.
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
M * w = w 0 * fv 0 + ... + w n * fv n
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
For the connection between matrices, linear transformations, and
geometry, I warmly recommend binge-watching the ``Essence of linear
algebra'' videos on youtube (start here:
\url{https://www.youtube.com/watch?v=kjBOesZCoqc}).

%*TODO: Perhaps it would be interesting to show that linear transformations
% can also be interpreted as changes of basis.

\subsection{Examples of matrix algebra}

\subsubsection{Polynomials and their derivatives}

We have represented polynomials of degree |n+1| by the list of their
coefficients.
%
This is quite similar to standard geometrical vectors represented
by |n+1| coordinates.
%
This suggests that polynomials of degree |n+1| form a vector space,
and we could interpret that as |{0, ..., n} -> REAL| (or, more
generally, |Field a => {0, ..., n} -> a|).
%
The operations |+| (vector addition) and |*| (vector scaling) are
defined in the same way as they are for functions.
%

To explain the vector space it is useful to start by defining the canonical base vectors.
%
As for geometrical vectors, they are
%
\begin{spec}
e i : {0, ..., n} -> Real, e i j = i `is` j
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
of saying that |e i| should be interpreted as |\x -> x^i|.
%
Any other polynomial function |p| equals the linear combination of
monomials, and can therefore be represented as a linear combination of
our base vectors |e i|.
%
For example, |p x = 2+x^3| is represented by |2 * e 0 + e 3|.
%

In general, the evaluator from the vector representation to polynomial
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
of degree |n|, and since |D (f + g) = D f + D g| and |D (s * f) = s *
D f|, we expect it to be a linear transformation.
%
What is its associated matrix?

The associated matrix will be
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
\x -> (i + 1) * x^i
\end{spec}
%
Thus
%
\begin{spec}
derive (e (i + 1)) = (i + 1) * (e i)
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

Take the polynomial
%
\begin{spec}
1 + 2 * x + 3 * x^2
\end{spec}
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
representing the polynomial |2 + 6*x|.

Exercise~\ref{exc:Dmatrixpowerseries}: write the
(infinite-dimensional) matrix representing |D| for power series.

Exercise~\ref{exc:matrixIntegPoly}: write the matrix |In| associated with
integration of polynomials.

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
avoid this is to extend the type of the functions to |REAL|.

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
In a sense, we can say that the two computations were done in
parallel.
%
But that is not quite accurate: if start with |{3, 4}|, we no longer
get the characteristic function of |{f 3, f 4} = {6}|, instead, we get
a vector that does not represent a characteristic function at all:
|[0, 0, 0, 0, 0, 0, 2]|.
%
In general, if we start with an arbitrary vector, we can interpret
this as starting with various quantities of some unspecified material
in each state, simultaneously.
%
If |f| were injective, the respective quantities would just gets
shifted around, but in our case, we get a more interesting behaviour.

What if we do want to obtain the characteristic function of the image
of a subset?
%
In that case, we need to use other operations than the standard
arithmetical ones, for example |min| and |max|.

The problem is that |({0, 1}, max, min)| is not a field, and neither is
|(REAL, max, min)|.
%
This is not a problem if all we want is to compute the evolutions of
possible states, but we cannot apply most of the deeper results of
linear algebra.

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
newtype G = G Int deriving (Eq, Show)

instance Bounded G where
  minBound  =  G 0
  maxBound  =  G 6

instance Enum G where
  toEnum          =  G
  fromEnum (G n)  =  n

instance Num G where
  fromInteger = G . fromInteger
\end{code}
Note that this is just for convenient notation (integer literals):
|G| should normally not be used with the other |Num| operations.
%
The transition function has type |G -> G|:
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
%
Thus we can implement |m| as:
%
\begin{code}
m1 :: Num s => G -> Vector s G
m1 g' = V (\ g -> (next1 g) `is` g')
\end{code}

Test:
%
\begin{code}
t1'  = mulMV m1 (e 3 + e 4)
t1   = toL t1'               -- |[0,0,0,0,0,0,2]|
\end{code}


%**TODO (NiBo):
% This could go to into the file for the live sessions:
%
% \begin{code}
% poss :: (Finite g, Num s) => Int -> Matrix s g g -> Vector s g -> [Vector s g]
% poss n m v = take (n + 1) (iterate (mulMV m) v)
% \end{code}
%
% *DSLsofMath.W07> |poss 6 m1 (e 3 + e 4)|
%
%
%
% Perhaps as an exercise to show how the notion of |Vector S| can
% be applied to seamlessly treat dynamical systems (contrast the
% implementation of |poss| with those of |poss1|, |poss2| and |poss3| for
% different dynamical systems):
% %
% \begin{code}
% poss1 :: (Eq a) => Int -> (a -> a) -> [a] -> [[a]]
% poss1 n next xs = take (n + 1) (iterate (nub . (map next)) xs)
% \end{code}
%
% *DSLsofMath.W07> |poss1 6 next1 [3, 4]|
%


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
the origin vector?
%
Now invert the arrow from |2| to |4| and repeat the exercise.
%
What changes?
%
Can you prove it?

Implementation:

The transition function has type |G -> (G -> Bool)|:
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
% now a Num instance
%
We need a |Num| instance for |Bool| (not a field!):
%
\begin{code}
instance Num Bool where
  (+)  =  (||)
  (*)  =  (&&)
  fromInteger 0  =  False
  fromInteger 1  =  True
  negate         =  not
  abs            =  id
  signum         =  id
\end{code}

Test:
%
\begin{code}
t2' = mulMV m2 (e 3 + e 4)
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
This formula in itself would be worth a lecture.
%
For one thing, the notation is extremely suspicious.
%
|(a || b)|, which is usually read ``|a|, given |b|'', is clearly not
of the same type as |a| or |b|, so cannot really be an argument to
|p|.
%
For another, the |p a| we are computing with this formula is not the
|p a| which must eventually appear in the products on the right hand
side.
%
I do not know how this notation came about: it is neither in Bayes'
memoir, nor in Kolmogorov's monograph.

The conditional probability |p (a || b)| gives us the probability that
the next state is |a|, given that the current state is |b|.
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
%*TODO: NiBo: There are many ways of reversing the arrow from 2 to 4!
Reverse again the arrow from 2 to 4.
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

\subsection{Monadic dynamical systems}

This section is not part of the intended learning outcomes of the
course, but it presents a useful unified view of the three previous
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
  The transition function has the type |f : G -> (G -> [0, 1])|, the
  structure of the target is the probability distributions over |G|.
\end{itemize}

Therefore:
%
\begin{itemize}
\item deterministic: |f : G -> Id G|
\item non-deterministic: |f : G -> Powerset G|, where |Powerset G = G -> {0, 1}|
\item stochastic: |f : G -> Prob G|, where |Prob G = G -> [0, 1]|
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

    M * (v 0 * e 0 + ... + v n * e n)

= {- homomorphism -}

    v 0 * (M * e 0) + ... + v n * (M * e n)

= {- |e i| represents the perfectly known current state |i|, therefore |M * e i = f i| -}

    v 0 * f 0 + ... + v n * f n
\end{spec}
%
So, we apply |f| to every state, as if we were starting from precisely
that state, obtaining the possible future states starting from that
state, and then collect all these hypothetical possible future
states in some way that takes into account the initial uncertainty
(represented by |v 0|, ..., |v n|) and the nature of the uncertainty
(the specific |+| and |*|).

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

Exercise: write |Monad| instances for |Id|, |Powerset|, |Prob|.


\subsection{The monad of linear algebra}

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
instance  (Bounded a, Enum a, Eq a) => Finite a  where

instance Num s => FinFunc (Vector s) where
  func f (V v) = V (\ g' -> sum [v g | g <- finiteDomain, g' == f g])

instance Num s => FinMon (Vector s) where
  embed g       =  V (is g)
  bind (V v) f  =  V (\ g' -> sum [toF (f g) g' * v g | g <- finiteDomain])
\end{code}
%
Note that, if |v :: Vector S G| and |f :: G -> Vector S G'| then
both |func f v| and |bind v f| are of type |Vector S G'|. How do
these operations relate to LinAlg and matrix-vector multiplication?

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
monadic operations, it is useful to introduce the ``dot'' product
between vectors:
%
\begin{code}
dot :: (Num s, Finite g) => Vector s g -> Vector s g -> s
dot (V v) (V w) = sum [v g * w g | g <- finiteDomain]
-- or |sum (map (v * w) finiteDomain)| where |v * w :: g -> s| uses |FunNumInst|
\end{code}

Remember that matrixes are just functions of type |G -> Vector S G'|:
%
\begin{spec}
  type Matrix s g g' = g' -> Vector s g
\end{spec}

According to our earlier definition, we can rewrite matrix-vector
multiplication in terms of dot products
%
\begin{spec}

  mulMV m (V v)

= {- earlier definition -}

  V (\ g' -> sum [m g' g * v g | g <- finiteDomain])

= {- def. of |dot| -}

  V (\ g' -> dot (m g') (V v))

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

Perhaps for extra exercises:

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

Exercise: show that if |w = func f v| then the sum of the components of
|w| is equal to the sum of the components of |v|.



Exercises:
%
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

% *TODO: Proving that |func| preserves composition and that |bind|
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

finiteDomain :: Finite a => [a]
finiteDomain = [minBound..maxBound]

instance (Finite g, Show s) => Show (g->s)        where  show = showFun
instance (Finite g, Show s) => Show (Vector s g)  where  show = showVector

showVector :: (Finite g, Show s) => Vector s g -> String
showVector (V v) = showFun v
showFun :: (Finite a, Show b) => (a->b) -> String
showFun f = show (map f finiteDomain)
\end{code}

TODO: convert to using the |newtype Vector|.

The scalar product of two vectors is a good building block for matrix
multiplication:
%
\begin{code}
dot' ::  (Finite g, Num s) =>
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
mulMV' ::  (Finite g, Num s) =>
           Mat s g g' ->  Vec s g  ->  Vec s g'
mulMV' m v  =  dot' v . m
type Mat s r c = c -> r -> s
type Vec s r = r -> s
\end{code}

Similarly, we can define matrix-matrix multiplication:
%
\begin{code}
mulMM' ::  (Finite b, Num s) =>
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
