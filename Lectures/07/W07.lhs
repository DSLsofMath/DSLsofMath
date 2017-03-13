\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module DSLsofMath.W07 where
\end{code}

\section{Matrix algebra and linear transformations}


Often, especially in engineering textbooks, one encounters the
``definition'': a vector is an |n+1|-tuple of real or complex numbers,
arranged as a column:

\begin{verbatim}
        v0
         .
 v  =    .
         .
        vn
\end{verbatim}

Other times, this is suplemented by the definition of a ``row vector'':

\begin{verbatim}
 v =  v0, ..., vn
\end{verbatim}

The |vi|s are real or complex numbers, or, more generally, elements of
a \emph{field} (analogous to being an instance of |Fractional|).
%
Vectors can be added ``point-wise'' and multiplied with ``scalars'',
i.e., elements of the field:

\begin{verbatim}
             v0      w0        v0 + w0
              .       .           .
 v + w  =     .   +   .    =      .
              .       .           .
             vn      wn        vn + wn
\end{verbatim}

\begin{verbatim}
            s*v0
             .
 s * v  =    .
             .
            s*vn
\end{verbatim}

The scalar |s| ``scales'' all the components of |v|.

In fact, the most important feature of vectors is that they can be
\emph{uniquely} expressed as a simple sort of combination of other
vectors:

\begin{verbatim}
        v0         1               0
         .         0               .
 v  =    .  = v0 * 0 ...  + ... vn *
         .         .               0
        vn         0               1
\end{verbatim}

We denote by

\begin{verbatim}
          0
          0
 ek  =    .
          .
          0
          1     <-- position k
          0
          .
          .
          0
\end{verbatim}

the vector that is everywhere |0| except at position |k|, where it is
|1|, so that

\begin{spec}
v = v0 * e0 + ... + vn * en
\end{spec}

There is a temptation to model vectors by lists or tuples, but the
simplest (at least conceptually) way is to view them as
\emph{functions}:


\begin{spec}
type S          =   ... -- the scalars, forming a field (|REAL|, or |Complex|, or |Zn|, etc.)
type Vector G   =   G -> S
\end{spec}

Usually, |G| is finite, i.e., |Bounded| and |Enumerable|.

We know from the previous lectures that if |S| is an instance of
|Num|, |Fractional|, etc. then so is |G -> S|, with the pointwise
definitions.
%
In particular, the instance declarations for |+|, multiplication, and
embedding of constants, give us exactly the structure needed for the
vector operations.
%
For example

\begin{spec}
    s * v

=  {- |s| is promoted to a function -}

    const s * v

=  {- |Num| instance definition -}

    \ g -> (const s) g * v g

=  {- definition of |const| -}

    \ g -> s * v g
\end{spec}

The set |G| is typically |{0, 1, ..., n}|.
%
The basis vectors are then

\begin{spec}
e i  :  G -> S,    e i g = i `is` g
\end{spec}

Implementation:

\begin{code}
is a b = if a == b then 1 else 0

e g = \ (G g') -> g `is` g'

toL v = [v g | g <- [minBound .. maxBound]]   -- so we can actually see them
\end{code}

and every

\begin{spec}
v : G -> S
\end{spec}

is trivially a linear combination of |e i|s:

\begin{spec}
v =  v 0 * e 0 + ... + v n * e n
\end{spec}

\subsection{Functions on vectors}

What if we have another vector space, |Vector G' = G' -> S|?
%
We are interested in functions |f : Vector G -> Vector G'|:

\begin{spec}
f v  =  f (v 0 * e 0 + ... + v n * e n)
\end{spec}

A ``good'' function should translate the operations in |Vector G| into
operations in |Vector G'|, i.e., should be a homomorphism:

\begin{spec}
f v =  f (v 0 * e 0 + ... + v n * e n) = v 0 * f (e 0) + ... + v n * f (e n)
\end{spec}

But this means that we can determine the values of
%
|f : (G -> S) -> (G' -> S)|
%
from just the values of
%
|f . e : G -> (G' -> S)|,
%
a much ``smaller'' function.
%
Let |m = f . e|.
%
Then

\begin{spec}
f v =  v 0 * m 0 + ... + v n * m n
\end{spec}

Each of |m k| is a |Vector G'|, as is the resulting |f v|.
%
We have

\begin{spec}
  f v g'

= {- as above -}

  v 0 * m 0 + ... + v n * m n

= {- |*| and |+| for functions are def. pointwise -}

  v 0 * m 0 g' + ... + v n * m n g'

= {- using |sum| -}

  sum [v i * m i g' | i <- [minBound .. maxBound]]
\end{spec}

Implementation:

This is the almost the standard ``vector-matrix'' multiplication:

\begin{spec}
M = [m 0 | ... | m n]
\end{spec}

The columns of |M| are the images of the canonical base vectors |e i|
through |f|.
%
Every |m k| has |card G'| rows, and it has become standard to use |M i
j| to mean the |i|th element of the |j|th column, i.e., |m j i|, so
that

\begin{spec}
(M * v) i = sum [M i j * v j | j <- [0 .. n]]
\end{spec}
\begin{code}
mul m v g' = sum [m g' g * v g | g <- [minBound .. maxBound]]
\end{code}

Example:

\begin{spec}
(M * e k) i = sum [M i j * e k j | j <- [0 .. n]] = sum [M i k] = M i k
\end{spec}

i.e., |e k| extracts the |k|th column from |M| (hence the notation
``e'' for ``extract'').

Given an arbitrary matrix |M|, we can define

\begin{spec}
f v = M * v
\end{spec}

and obtain a linear transformation |(M*)|.
%
Moreover |((M*) . e) g g' = M g' g|, i.e., the matrix constructed as
above for |f| is precisely |M|.

Exercise: compute |((M*) . e ) g g'|.

Therefore, every linear transformation is of the form |(M*)| and every
|(M*)| is a linear transformation.

Matrix-matrix multiplication is defined in order to ensure that

\begin{spec}
(M' * M) * v = M' * (M * v)
\end{spec}

that is

\begin{spec}
((M' * M)*) = (M' *) . (M *)
\end{spec}

Exercise: work this out in detail.

Exercise: show that matrix-matrix multiplication is associative.

Perhaps the simplest vector space is obtained for |G = ()|, the
singleton set.
%
In this case, the vectors |s : () -> S| are functions that can take
exactly one argument, therefore have exactly one value: |s ()|, so
they are often identified with |S|.
%
But, for any |v : G -> S|, we have a function |fv : G -> (() -> S)|,
namely

\begin{spec}
fv g () = v g
\end{spec}

|fv| is similar to our |m| function above.
%
The associated matrix is

\begin{spec}
M = [m 0 | ... | m n] = [fv 0 | ... | fv n]
\end{spec}

having |n+1| columns (the dimension of |Vector G|) and one row
(dimension of |Vector ()|).
%
Let |w :: Vector G|:

\begin{spec}
M * w = w 0 * fv 0 + ... + w n * fv n
\end{spec}

|M * v| and each of the |fv k| are ``almost scalars'': functions of
type |() -> S|, thus, the only component of |M * w| is

\begin{spec}
(M * w) () = w 0 * fv 0 () + ... + w n * fv n () = w 0 * v 0 + ... + w n * v n
\end{spec}

i.e., the scalar product of |v| and |w|.


\textbf{Remark:} I have not discussed the geometrical point of view.
%
For the connection between matrices, linear transformations, and
geometry, I warmly recommend binge-watching the ``Essence of linear
algebra'' videos on youtube (start here:
https://www.youtube.com/watch?v=kjBOesZCoqc).


\subsection{Examples of matrix algebra}

\subsubsection{Derivative}


We have represented polynomials of degree |n+1| by the list of their
coefficients.
%
This is quite similar to ``standard'' geometrical vectors represented
by |n+1| coordinates.
%
This suggests that polynomials of degree |n+1| form a vector space,
and we could interpret that as |{0, ..., n} -> REAL| (or, more
generally, |Field a => {0, ..., n} -> a|.
%
The operations |+| and |*| are defined in the same way as they are for
functions.

The |derive| function takes polynomials of degree |n+1| to polynomials
of degree |n|, and since |D (f + g) = D f + D g| and |D (s * f) = s *
D f|, we expect it to be a linear transformation.
%
What is its associated matrix?

To answer that, we must first determine the canonical base vectors.
%
As for geometrical vectors, they are

\begin{spec}
e i : {0, ..., n} -> Real, e i j = i `is` j
\end{spec}

The evaluation of |e i| returns the function |\ x -> x^i|, as
expected.

The associated matrix will be

\begin{spec}
M = [ D (e 0), D (e 1), ..., D (e n) ]
\end{spec}

where each |D (e i)| has length |n|.
%
Vector |e (i+1)| represents |x^(i+1)|, therefore

\begin{spec}
D (e (i+1)) = (i+1) * x^i
\end{spec}

i.e.

\begin{spec}
D (e (i+1)) !! j = if i == j then i+1 else 0
\end{spec}

and

\begin{spec}
D (e 0) = 0
\end{spec}

Example: |n+1 = 3|:

\begin{verbatim}
          0  1  0
   M  =
          0  0  2
\end{verbatim}

Take the polynomial

\begin{spec}
3 * X^2 + 2 * X + 1
\end{spec}

as a vector

\begin{verbatim}
          1
   v  =   2
          3
\end{verbatim}

and we have

\begin{verbatim}
                0  1  0      1        2
   M  * v  =              *  2   =
                0  0  2      3        6
\end{verbatim}

representing the polynomial |6 * X^2 + 2 * X|.

Exercise: write the (infinite-dimensional) matrix representing |D| for
power series.

Exercise: write the matrix associate with integration of polynomials.

\subsubsection{Simple deterministic systems (transition systems)}

Simple deterministic systems are given by endo-functions on a finite
set |f : G -> G|.
%
They can often be conveniently represented as a graph, for example

\begin{verbatim}
                  +---+
                  | 3 |
                  +---+
                 ^      \
                /        v
            +---+         +---+
            | 1 |         | 6 |
            +---+         +---+
           ^             ^  |
          /             /   |
      +---+        +---+    |
      | 0 |        | 4 |    |
      +---+        +---+    |
                        ^   |
                         \  v
           +---+          +---+
           | 2 |--------->| 5 |
           +---+          +---+
\end{verbatim}

Here, |G = {0, ..., 6}|.
%
A node in the graph represents a state.
%
A transition |i -> j| means |f i = j|.
%
Since |f| is an endo-function, every node must be the source of
exactly one arrow.

We can take as vectors the characteristic functions of subsets of |G|,
i.e., |G -> {0, 1}|.
%
|{0, 1}| is not a field w.r.t. the standard arithmetical operations
(it is not even closed w.r.t. addition), and the standard trick to
avoid this is to extend the type of the functions to |REAL|.

The canonical basis vectors are, as usual, |e i = \j -> i `is` j|.
%
Each |e i| is the characteristic function of a singleton set, |{i}|.
%
Thus, the inputs to |f| are canonical vectors.

To write the matrix associated to |f|, we have to compute what vector
is associated to each canonical base vector vector:

\begin{spec}
M = [ f (e 0), f (e 1), ..., f (e n) ]
\end{spec}

Therefore:

\begin{verbatim}
          0    0    0    0    0    0    0
          1    0    0    0    0    0    0
          0    0    0    0    0    0    0
   M  =   0    1    0    0    0    0    0
          0    0    0    0    0    1    0
          0    0    1    0    0    0    1
          0    0    0    1    1    0    0
\end{verbatim}

Starting with a canonical base vector |e i|, we obtain |M * e i = e (f
i)|, as we would expect.

It is more interesting if we start with a non-base vector.
%
For example, |e 2 + e 4|, which represents the subset |{2, 4}|.


The more interesting thing is if we start with something different
from a basis vector, say |[0, 1, 1]|.
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
%
The problem is that |({0, 1}, max, min)| is not a field, and neither is
|(REAL, max, min)|.
%
This is not a problem if all we want is to compute the evolutions of
possible states, but we cannot apply most of the ``deeper'' results of
linear algebra.

In the example above, we have:

\begin{code}
newtype G = G Int deriving (Eq, Show)

instance Bounded G where
  minBound  =  G 0
  maxBound  =  G 6

instance Enum G where
  toEnum          =  G
  fromEnum (G n)  =  n
\end{code}

The transition function:

\begin{code}
f1 0 = 1
f1 1 = 3
f1 2 = 5
f1 3 = 6
f1 4 = 6
f1 5 = 4
f1 6 = 5
\end{code}

The associated matrix:

\begin{code}
m1 (G g') (G g)  =  g'  `is`  f1 g
\end{code}

Test:

\begin{code}
t1 = toL (mul m1 (e 3 + e 4))
\end{code}

\subsubsection{Non-deterministic systems}

Another interpretation of the application of |M| to characteristic
functions of a subset is the following: assuming that all I know is
that the system is in one of the states of the subset, where can it
end up after one step?  (this assumes the |max|-|min| algebra as
above).

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

\begin{verbatim}
                  +---+
                  | 3 |
                  +---+
                 ^      \
                /        v
            +---+         +---+
            | 1 |         | 6 |
            +---+         +---+
           ^     ^       ^
          /       \     /
      +---+        +---+
      | 0 |        | 4 |
      +---+        +---+
           \     ^      ^
            v   /        \
           +---+          +---+
           | 2 |--------->| 5 |
           +---+          +---+
\end{verbatim}

Now, starting in |0| we might and up either in |1| or |2| (but not
both!).
%
Starting in |6|, the system breaks down: there is no successor state.

The matrix associated to |R| is built in the same fashion: we need to
determine what vectors the canonical base vectors are associated with:

\begin{verbatim}
          0    0    0    0    0    0    0
          1    0    0    0    1    0    0
          1    0    0    0    0    0    0
   M  =   0    1    0    0    0    0    0
          0    0    1    0    0    1    0
          0    0    1    0    0    0    0
          0    0    0    1    1    0    0
\end{verbatim}

Exercise: start with |e 2 + e 3| and iterate a number of times, to get
a feeling for the possible evolutions.
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

\begin{code}
f2 0 g      =   g == 1 || g == 2
f2 1 g      =   g == 3
f2 2 g      =   g == 4 || g == 5
f2 3 g      =   g == 6
f2 4 g      =   g == 1 || g == 6
f2 5 g      =   g == 4
f2 6 g      =   False
\end{code}

The associated matrix:

\begin{code}
m2 (G g') (G g) = f2 g g'
\end{code}

We need a |Num| instance for |Bool| (not a field!):

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

\begin{code}
t2 = toL (mul m2 (e 3 + e 4))
\end{code}

\subsubsection{Stochastic systems}

Quite often, we have more information about the transition to possible
future states.
%
In particular, we can have \emph{probabilities} of these transitions.
%
For example

\begin{verbatim}
                  +---+
                  | 3 |
                  +---+
               1 ^      \ 1
                /        v
            +---+         +---+ 1
            | 1 |         | 6 |---.
            +---+         +---+<--´
           ^     ^       ^
       .4 /    .5 \     / .5
      +---+        +---+
      | 0 |        | 4 |
      +---+        +---+
           \   .7^      ^ 1
         .6 v   /        \
           +---+   .3     +---+
           | 2 |--------->| 5 |
           +---+          +---+
\end{verbatim}

One could say that this case is a generalisation of the previous one,
in which we can take all probabilities to be equally distributed among
the various possiblities.
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

\begin{spec}
sum [p g | g <- [0 .. 6]] = 1
\end{spec}

If we know the current probability distributions over states, then we
can compute the next one by using the \emph{total probability formula},
normally expressed as

\begin{spec}
p a = sum [p (a | b) * p b | b <- [0 .. 6]]
\end{spec}

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
In this case, the canonical base vector |e i = \ j -> i `is` j| is the
probability distribution \emph{concentrated} in |i|:

\begin{verbatim}
          0    0    0    0    0    0    0
          .4   0    0    0    .5   0    0
          .6   0    0    0    0    0    0
   M  =   0    1    0    0    0    0    0
          0    0    .7   0    0    1    0
          0    0    .3   0    0    0    0
          0    0    0    1    .5   0    1
\end{verbatim}

Exercise: starting from state 0, how many steps do you need to take
before the probability is concentrated in state 6?
%
Reverse again the arrow from 2 to 4.
%
What can you say about the long-term behaviour of the system now?

Exercise: Implement the example.
%
You will need to define:

The transition function

\begin{code}
f3 :: G -> (G -> Double)  -- but we want only |G -> (G -> [0, 1])|, the unit interval
f3 g g' = undefined       -- the probability of getting to |g'| from |g|
\end{code}

The associated matrix

\begin{code}
m3 ::  G -> (G -> Double)
m3 g' g   =  undefined
\end{code}

Test

\begin{code}
t3 = toL (mul m3 (e 2 + e 4))
\end{code}


\subsection{Monadic dynamical systems}

All the examples of dynamical systems we have seen in the previous
section have a similar structure.
%
They work by taking a state (which is one of the generators) and
return a structure of possible future states of type |G|:

\begin{itemize}
\item deterministic: there is exactly one possible future states: we
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

\begin{spec}
    M * v       -- v represents the current possible states

= {- |v| is a linear combination of the base vectors -}

    M * (v 0 * e 0 + ... + v n * e n)

= {- homomorphism -}

    v 0 * (M * e 0) + ... + v n * (M * e n)

= {- |e i| represents the perfectly known current state |i|, therefore |M * e i = f i| -}

    v 0 * f 0 + ... + v n * f n
\end{spec}

So, we apply |f| to every state, as if we were starting from precisely
that state, obtaining the possible future states starting from that
state, and then collect all these ``hypothetical'' possible future
states in some way that takes into account the initial uncertainty
(represented by |v 0|, ..., |v n|) and the nature of the uncertainty
(the specific |+| and |*|).

If you examine the types of the operations involved

\begin{spec}
e : G -> Possible G
\end{spec}

and

\begin{spec}
    Possible G -> (G -> Possible G) -> Possible G
\end{spec}

you see that they are very similar to the monadic operations

\begin{spec}
    return  :  g -> m g
    (>>=)   :  m g -> (g -> m g') -> m g'
\end{spec}

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

\begin{code}
type S            =  Double
data Vector g     =  V (g -> S)
toF (V v)         =  v

class     (Bounded a, Enum a, Eq a) => Finite a  where
instance  (Bounded a, Enum a, Eq a) => Finite a  where

class FinFunc f where
  func :: (Finite a, Finite b) =>  (a -> b) -> f a -> f b

instance FinFunc Vector where
  func = funcV

funcV :: (Finite g, Eq g') => (g -> g') -> Vector g -> Vector g'
funcV f (V v) =  V (\ g' -> sum [v g | g <- [minBound .. maxBound], g' == f g])

class FinMon f where
  embed   ::  Finite a => a -> f a
  bind    ::  (Finite a, Finite b) => f a -> (a -> f b) -> f b

instance FinMon Vector where
  embed a       =  V (\ a' -> if a == a' then 1 else 0)
  bind (V v) f  =  V (\ g' -> sum [toF (f g) g' * v g | g <- [minBound .. maxBound]])
\end{code}

A better implementation, using associated types, is in file
|Vector.lhs| in the repository.

Exercises:

\begin{enumerate}
\item Prove that the functor laws hold, i.e.

\begin{spec}
func id       =  id
func (g . f)  =  func g . func f
\end{spec}

\item Prove that the monad laws hold, i.e.

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

\subsection{Associated code}

TODO: import from suitable earlier lecture.

\begin{code}
instance Num a => Num (x -> a) where
  f + g        =  \x -> f x + g x
  f - g        =  \x -> f x - g x
  f * g        =  \x -> f x * g x
  negate f     =  negate . f
  abs f        =  abs . f
  signum f     =  signum . f
  fromInteger  =  const . fromInteger

instance Fractional a => Fractional (x -> a) where
  recip  f         =  recip . f
  fromRational     =  const . fromRational

instance Floating a => Floating (x -> a) where
  pi       =  const pi
  exp f    =  exp . f
  sin f    =  sin . f
  cos f    =  cos . f
  f ** g   =  \ x -> (f x)**(g x)
  -- and so on
\end{code}
