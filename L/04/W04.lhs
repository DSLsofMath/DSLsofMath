% Week 4-5: Poly, PowerSeries, ...

\section{Week 4}
\begin{code}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module DSLsofMath.W04 where
import Prelude hiding (Monoid)
\end{code}

% (Based on ../../2016/Lectures/Lecture06  )

\subsection{A simpler example of a non-compositional function}

Consider a very simple datatype of integer expressions:
%
\begin{code}
data E = Add E E | Mul E E | Con Integer deriving Eq
e1, e2 :: E
e1 = Add (Con 1) (Mul (Con 2) (Con 3))
e2 = Mul (Add (Con 1) (Con 2)) (Con 3)
\end{code}
%
When working with expressions it is often useful to have a
``pretty-printer'' to convert the abstract syntax trees to strings
like |"1+2*3"|.
%
\begin{code}
pretty :: E -> String
\end{code}
%
We can view |pretty| as an alternative |eval| function for a semantics
using |String| as the semantic domain instead of the more natural
|Integer|.
%
We can implement |pretty| in the usual way as a ``fold'' over the
syntax tree using one ``semantic constructor'' for each syntact
constructor:
%
\begin{code}
pretty (Add x y)  = prettyAdd (pretty x) (pretty y)
pretty (Mul x y)  = prettyMul (pretty x) (pretty y)
pretty (Con c)    = prettyCon c

prettyAdd :: String -> String -> String
prettyMul :: String -> String -> String
prettyCon :: Integer -> String
\end{code}
%
Now, if we try to implement the semantic constructors without thinking
too much we would get the following:
\begin{code}
prettyAdd sx sy  = sx ++ "+" ++ sy
prettyMul sx sy  = sx ++ "*" ++ sy
prettyCon i      = show i

p1, p2 :: String
p1 = pretty e1
p2 = pretty e2

trouble :: Bool
trouble = p1 == p2
\end{code}
%
Note that both |e1| and |e2| are different but they pretty-print to
the same string.
%
There are many ways to fix this, some more ``pretty'' than others, but
the main problem is that some information is lost in the translation.
%

TODO(perhaps): Explain using two pretty printers: for a sum of terms,
for a product of factors, ... then combine them with the tupling
transform just as with |evalD|.

Exercise: Another way to make this example go through is to refine the
semantic domain from |String| to |Precedence -> String|.
%
(This can be seen as another variant of the result after the tupling
transform: if |Precedence| is an |n|-element type then |Precedence ->
String| can be seen as an |n|-tuple.)

\subsection{Compositional semantics in general}

In general, for a syntax |Syn|, and a possible semantics (a type |Sem|
and an |eval| function of type |Syn -> Sem|), we call the semantics
\emph{compositional} if we can implement |eval| as a fold.
%
Informally a ``fold'' is a recursive function which replaces each
abstract syntax constructor |Ci| of |Syn| with a ``semantic
constructor'' |ci|.
%

TODO: Picture to illustrate
\begin{verbatim}
    Add                           add
   /   \                         /   \
Con 1   Mul         |----->   con 1   mul
       /   \                         /   \
    Con 2  Con 3                  con 2  con 3
\end{verbatim}

As an example we can define a general |foldE| for the integer
expressions:
%
\begin{code}
foldE ::  (t -> t -> t) -> (t -> t -> t) -> (Integer -> t) ->
          E -> t
foldE add mul con = rec
  where  rec (Add x y)  = add (rec x) (rec y)
         rec (Mul x y)  = mul (rec x) (rec y)
         rec (Con i)    = con i
\end{code}
%
Notice that |foldE| has three function arguments corresponding to the
three constructors of |E|.
%
The ``natural'' evaluator to integers is then easy:
\begin{code}
evalE1 :: E -> Integer
evalE1 = foldE (+) (*) id
\end{code}
%
and with a minimal modification we can also make it work for other
numeric types:
%
\begin{code}
evalE2 :: Num a => E -> a
evalE2 = foldE (+) (*) fromInteger
\end{code}

Another thing worth noting is that if we replace each abstract syntax
constructor with itself we get the identity function (a ``deep
copy''):
\begin{code}
idE :: E -> E
idE = foldE Add Mul Con
\end{code}

Finally, it is often useful to capture the semantic functions (the
parameters to the fold) in a type class:
%
\begin{code}
class IntExp t where
  add :: t -> t -> t
  mul :: t -> t -> t
  con :: Integer -> t
\end{code}
%
In this way we can make ``hide'' the arguments to the fold:
%
\begin{code}
foldIE :: IntExp t =>  E -> t
foldIE = foldE add mul con

instance IntExp E where
  add = Add
  mul = Mul
  con = Con

instance IntExp Integer where
  add = (+)
  mul = (*)
  con = id

idE' :: E -> E
idE' = foldIE

evalE' :: E -> Integer
evalE' = foldIE
\end{code}

\subsection{Back to derivatives and evaluation}

Review section \ref{sec:evalD} again with the definition of |eval'|
being non-compositional (just like |pretty|) and |evalD| a more
complex, but compositional, semantics.
%

% Compositionality:
% * simpler example of non-compositional function [See L9]
% * then back to |eval'|
% * and show |evalD|
% * general pattern: tupling
%

\section{Algebraic Structures and DSLs}

% based on ../../2016/Lectures/Lecture09.lhs

In this lecture, we continue exploring the relationship between type
classes, mathematical structures, and DSLs.

\subsection{Algebras, homomorphisms}

From Wikipedia:

\begin{quote}
  In universal algebra, an algebra (or algebraic structure) is a set |A|
  together with a collection of operations on |A|.

Example:

\begin{code}
class Monoid a where
  unit  ::  a
  op    ::  a -> a -> a
\end{code}

After the operations have been specified, the nature of the
algebra can be further limited by axioms, which in universal
algebra often take the form of identities, or \emph{equational laws}.

Example: Monoid equations

\begin{spec}
∀ x : a? (unit `op` x == x  ∧  x `op` unit == x)
∀ x, y, z : a? (x `op` (y `op` z) == (x `op` y) `op` z)
\end{spec}

A homomorphism between two algebras |A| and |B| is a function |h: A → B|
from the set |A| to the set |B| such that, for every operation |fA| of |A|
and corresponding |fB| of |B| (of arity, say, |n|),
|h(fA(x1,...,xn)) = fB(h(x1),...,h(xn))|.

\end{quote}

Example: Monoid homomorphism

\begin{spec}
h unit        =  unit
h (x `op` y)  =  h x `op` h y
\end{spec}

\begin{code}
newtype ANat      =  A Int          deriving (Show, Num, Eq)

instance Monoid ANat where
  unit            =  A 0
  op (A m) (A n)  =  A (m + n)

newtype MNat      =  M Int          deriving (Show, Num, Eq)

instance Monoid MNat where
  unit            =  M 1
  op (M m) (M n)  =  M (m * n)
\end{code}

Exercise: characterise the homomorphisms from |ANat| to |MNat|.

Solution:

Let |h : ANat -> MNat| be a homomorphism.
%
Then

\begin{spec}
h 0        = 1
h (x + y)  = h x * h y
\end{spec}

For example |h (x + x) = h x * h x = (h x) ^ 2| which for |x = 1|
means that |h 2 = (h 1) ^ 2|.

More generally, every |n| in |ANat| is equal to the sum of |n| ones:
|1 + 1 + ... + 1|.
%
Therefore
%
\begin{spec}
h n = (h 1) ^ n
\end{spec}

Every choice of |h 1| ``induces a homomorphism''.
%
This means that the value of the function |h| is fully determined by
its value for |1|.

\subsection{Homomorphism and compositional semantics}

Last time, we saw that |eval| is compositional, while |eval'| is not.
%
Another way of phrasing that is to say that |eval| is a homomorphism,
while |eval'| is not.
%
To see this, we need to make explicit the structure of |FunExp|:
%
\begin{spec}
instance Num FunExp where
  (+)          =  (:+:)
  (*)          =  (:*:)
  fromInteger  =  Const . fromInteger

instance Fractional FunExp where

instance Floating FunExp where
  exp          =  Exp

\end{spec}
%
and so on.

Exercise: complete the type instances for |FunExp|.

For instance, we have
%
\begin{spec}
eval (e1 :*: e2)  =  eval e1 * eval e2
eval (Exp e)      =  exp (eval e)
\end{spec}

These properties do not hold for |eval'|, but do hold for |evalD|.

The numerical classes in Haskell do not fully do justice to the
structure of expressions, for example, they do not contain an identity
operation, which is needed to translate |Id|, nor an embedding of
doubles, etc.
%
If they did, then we could have evaluated expressions more abstractly:
%
\begin{spec}
eval :: GoodClass a  =>  FunExp -> a
\end{spec}
%
where |GoodClass| gives exactly the structure we need for the
translation.

Exercise: define |GoodClass| and instantiate |FunExp| and |Double ->
Double| as instances of it.
%
Find another instance of |GoodClass|.

Therefore, we can always define a homomorphism from |FunExp| to
\emph{any} instance of |GoodClass|, in an essentially unique way.
%
In the language of category theory, |FunExp| is an initial algebra.

Let us explore this in the simpler context of |Monoid|.
%
The language of monoids is given by

\begin{code}
type Var      =  String

data MExpr    =  Unit  |  Op MExpr MExpr  |  V Var
\end{code}

Alternatively, we could have parametrised |MExpr| over the type of
variables.

Just as in the case of FOL terms, we can evaluate an |MExpr| in a
monoid instance if we are given a way of interpreting variables, also
called an assignment:

\begin{code}
evalM :: Monoid a => (Var -> a) -> (MExpr -> a)
\end{code}

Once given an |f :: Var -> a|, the homomorphism condition defines
|evalM|:

\begin{code}
evalM  f  Unit        =  unit
evalM  f  (Op e1 e2)  =  op (evalM f e1) (evalM f e2)
evalM  f  (V x)       =  f x
\end{code}

(Observation: In |FunExp|, the role of variables was played by
|Double|, and the role of the assignment by the identity.)

The following correspondence summarises the discussion so far:

\begin{tabular}{ll}
      Computer Science      &   Mathematics
\\\hline
\\    DSL                   &   structure (category, algebra, ...)
\\    deep embedding, abstract syntax        &   initial algebra
\\    shallow embedding     &   any other algebra
\\    semantics             &   homomorphism from the initial algebra
\end{tabular}

The underlying theory of this table is a fascinating topic but mostly
out of scope for the DSLsofMath course.
%
See
\href{http://wiki.portal.chalmers.se/cse/pmwiki.php/CTFP14/CoursePlan}{Category
  Theory and Functional Programming} for a whole course around this.

\subsection{Other homomorphisms}

Last time, we defined a |Num| instance for functions with a |Num|
codomain.
%
If we have an element of the domain of such a function, we can use it
to obtain a homomorphism from functions to their codomains:

\begin{spec}
Num a => x ->  (x -> a) -> a
\end{spec}

As suggested by the type, the homomorphism is just function
application:

\begin{spec}
apply :: a -> (a -> b) -> b
apply a = \f -> f a
\end{spec}

Indeed, writing |h = apply c| for some fixed |c|, we have

\begin{spec}
     h (f + g)

  =  {- def. |apply| -}

     (f + g) c

  =  {- def. |+| for functions -}

     f c + g c

  =  {- def. |apply| -}

     h f + h g
\end{spec}
%
etc.

Can we do something similar for |FD|?

The elements of |FD| a are pairs of functions, so we can take

\begin{spec}
apply ::  a ->  FD a     ->  (a, a)
apply     c     (f, f')  =   (f c, f' c)
\end{spec}

We now have the domain of the homomorphism |(FD a)| and the
homomorphism itself |(apply c)|, but we are missing the structure on
the codomain, which now consists of pairs |(a, a)|.
%
In fact, we can \emph{compute} this structure from the homomorphism
condition.
%
For example:

\begin{spec}
     h ((f, f') * (g, g'))

  =  {- def. |*| for |FD a| -}

     h (f * g, f' * g + f * g')

  =  {- def. |h = apply c| -}

     ((f * g) c, (f' * g + f * g') c)

  =  {- def. |*| and |+| for functions -}

     (f c * g c, f' c * g c + f c * g' c)

  =  {- homomorphism condition from step 1 -}

     h (f, f') * h (g, g')

  =  {- def. |h = apply c| -}

     (f c, f' c) * (g c, g' c)
\end{spec}

The identity will hold if we take

\begin{spec}
     (x, x') * (y, y') = (x * y, x' * y + x * y')
\end{spec}

Exercise: complete the instance declarations for |(Double, Double)|.

Note: As this computation goes through also for the other cases we can
actually work with just pairs of values (at an implicit point |c ::
a|) instead of pairs of functions.
%
Thus we can redefine |FD| to be
%
\begin{spec}
type FD a = (a, a)
\end{spec}

Hint: Something very similar can be used for Assignment 2.

\section{Some helper functions}

\begin{code}
instance Num E where -- Some abuse of notation
  (+)  = Add
  (*)  = Mul
  fromInteger = Con
\end{code}
