\chapter{Compositional Semantics and Algebraic Structures}
% Based on ../../2016/Lectures/Lecture06  and
% based on ../../2016/Lectures/Lecture09.lhs
\label{sec:CompSem}

\begin{code}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
module DSLsofMath.W04 where
import Prelude hiding (Monoid, even, Num(..))
import DSLsofMath.FunExp
import DSLsofMath.Algebra hiding (fromInteger)
\end{code}
%

Algebraic structures are fundamental to
the structuralist point of view in mathematics, which emphasises relations
between objects rather than the objects themselves, and their
representations.
Furthermore, each mathematical domain has its own fundamental
structures.
%
Once these have been identified, one tries to push their study as far
as possible \emph{on their own terms}, without picking any particular
representation (which may have richer structure than the one we want
to study).
%
For example, in group theory, one starts by exploring the consequences
of just the group structure, rather than introducing any particular
group (like integers) which have an order structure and monotonicity.

Furthermore, mappings or (translations) between such structures
becomes an important topic of study.

When such mappings preserve the structure, they are called
\emph{homomorphisms}.
%
As two examples, we have the homomorphisms |exp| and |log|, specified
as follows:
%
\begin{spec}
  exp  :  REAL  ->  RPos
  exp  0        =   1                 --  \(e^0 = 1\)
  exp  (a + b)  =   exp a  *  exp b   --  \(e^{a+b} = e^a e^b\)

  log  :  RPos  ->  REAL
  log  1        =   0                 -- \(\log 1 = 0\)
  log  (a * b)  =   log a  +  log b   -- \(\log(ab) = \log a + \log b \)
\end{spec}
%
What we recognize as the familiar laws of exponentiation and
logarithms are actually examples of homomorphism conditions, which
relate the additive and multiplicative structures of reals and
positive reals.

Additionally, homomorphisms play a crucial role when relating an
abstract syntax (a datatype), and a semantic domain (another type) via
an evaluation function between them (the semantics).
%
In this chapter we will explain the notions of algebraic struture and
homomorphism in detail and show applications both in mathematics and
DSLs in general.


\section{Algebraic Structures}

What is an algebraic structure?
%
Let's turn to Wikipedia as a starting point:
%
\begin{quote}
  In universal algebra, an algebra (or algebraic structure) is a set
  |A| together with a collection of operations on |A| (of finite
  arity) and a collection of axioms which those operation must
  satisfy.
\end{quote}

The fact that a type |a| is equipped with operations is conveniently
captured in Haskell using a type class (\cref{sec:typeclasses}).

\begin{example}
  A particularly pervasive structure is that of monoids.
  %
  A monoid is an algebra which has an associative operation |op| and a
  |unit|:
\begin{code}
class Monoid a where
    unit  ::  a
    op    ::  a -> a -> a
\end{code}
% 
The laws cannot be obviously captured in the class, but can be
formulated as the following equations:
% 
\begin{spec}
  ∀ x : a? (unit `op` x == x  ∧  x `op` unit == x)
  ∀ x, y, z : a? (x `op` (y `op` z) == (x `op` y) `op` z)
  \end{spec}
  The first law ensures that |unit| is indeed the unit of |op| and the
  second law is the familiar associativity law for |op|.
\end{example}

\begin{example}
  Examples of monoids include numbers with additions, |(REAL, 0, (+))|,
  positive numbers with multiplication |(RPos, 1, (*))|, and even endofunctions
  with composition |(a->a,id, (.))| .
  %
  (An ``endofunction'', also known as ``endomorphism'' is a function of type |X->X| for some set
  |X|.)
  \label{ex:endofunction}
\end{example}

\begin{exercise}
  Define the above monoids and check that the laws are satisfied.
\end{exercise}

To make this a bit more concrete, here are two examples of monoids in
Haskell: the additive monoid |ANat| and the multiplicative monoid
|MNat|.
%
\begin{code}
newtype ANat      =  A Natural          deriving (Show, Eq)

instance Monoid ANat where
  unit            =  A 0
  op (A m) (A n)  =  A (m + n)

newtype MNat      =  M Natural          deriving (Show, Eq)

instance Monoid MNat where
  unit            =  M 1
  op (M m) (M n)  =  M (m * n)
\end{code}
Indeed, in Haskell there can ever be at most one instance of a given
class for a given type, so we cannot define two |instance Monoid
Natural|: we must make a |newtype| whose role is to indicate which of
the two possible monoids applies in a given context.
%
But, in mathematical texts the constructors |M| and |A| are usually
omitted, and instead the names of the operations suggest which of the
monoids one is referring to.  To be able to conform to that tradition
we can define two separate classes, one for the additive and one for
the multiplicative monoids, as follows.
\label{sec:ring-like-classes}
\begin{spec}
class Additive a where
  zero :: a
  (+) :: a -> a -> a

class Multiplicative a where
  one :: a
  (*) :: a -> a -> a
\end{spec}
This is what we have done in \cref{sec:numeric-classes}.

\begin{example}{Groups and rings}
  Another important structure are groups, which are monoids augmented with an
  inverse. To complete our mathematically-grounded |Num| replacement,
  we define the additive group as follows.

\begin{spec}
class Additive a => AddGroup a where
  negate :: a -> a
\end{spec}

Groups demand that the inverse (called |negate| for the additive
group) act like an inverse. Namely, applying the operation to an
element and its inverse should yield the unit of the group.  Thus, for
the additive group, the laws look like this:

\begin{spec}
negate a + a = zero
a + negate a = zero
\end{spec}

And thus we can define subtraction as
\begin{spec}
a - b = a + negate b
\end{spec}

Finally, when the additive monoid is abelian (commutative) and
addition distributes over multiplication, we have a |Ring|. As always
we cannot conveniently specify laws in Haskell typeclasses and thus
define |Ring| simply as the conjunction of |AddGroup| and
|Multiplicative|:
\begin{spec}
type Ring a = (AddGroup a, Multiplicative a)
\end{spec}

With that, we have completed the structural motivation of our
replacement for the |Num| class!

\section{Homomorphisms}
The Wikipedia definition of homomorphism states ``A homomorphism is a
structure-preserving map between two algebraic structures of the same
type''. 

\subsection{(Homo)morphism on one operation}

To capture this
idea, in a first instance, we can define a
ternary predicate |H2|. The first argument |h|, is the map. The second
(|Op|) and third (|op|) arguments represent the algebraic structures.
%
\begin{spec}
  H2(h,Op,op)  =  Forall x (Forall y (h(Op x y) == op (h x) (h y)))
\end{spec}
%
If this holds, we say that |h : A -> B| is a homomorphism from |Op :
A->A->A| to |op : B->B->B|.
%
Or that |h| is a homomorphism from |Op| to |op|.
%
Or even that |h| is a homomorphism from |A| to |B| if the
operators are clear from the context.
We have seen several examples in earlier chapters:
%
\begin{enumerate}
\item in \refSec{sec:complexcase} we saw that |evalE : ComplexE ->
  ComplexD| is a homomorphism from the syntactic operator |Plus| to
  the corresponding semantic operator |plusD|.
\item in \refSec{sec:logic} we saw de Morgan's laws which can be
  stated as |H2(not,(&&),(||||))| and |H2(not,(||||),(&&))|.
\item in \refSec{sec:FunExp} we saw that |eval : FunExp -> Func| is a
  homomorphism from syntactic |(:*:)| to semantic |(*)| for functions
\item
  \label{distributivity-as-homomorphism}
  If |(*)| distributes
  over |(+)| for some type |A| then |(*c) : A -> A| is a homomorphism
  from |(+)| to |(+)|: |H2((*c),(+),(+))|.
\end{enumerate}

To see how this last item plays out, it
can be helpful to study the syntax trees of the left and right hand
sides of the distributive law: |((a+b)*c = (a*c)+(b*c))|.
%
We observe that |(*c)| is ``pushed down'' to both |a| and |b|:

\tikzset{
  AbsSyn/.style={%
    baseline,
    text height=1.5ex,text depth=.25ex,
    level 1/.style={sibling distance=1.5cm, level distance=1cm},level 2/.style={sibling distance=1cm}
  },
  emph/.style={edge from parent/.style={thick,draw},font=\boldmath},
  bold/.style={font=\boldmath}
}

\begin{tikzpicture}[AbsSyn]
\node [bold] {|*|}
child {node {|+|} child {node {|a|}} child {node {|b|}}}
child[emph] {node {|c|}};
\end{tikzpicture}
\begin{tikzpicture}[AbsSyn]
\node{|+|}
child {node [bold] {|*|} child {node {|a|}} child[emph] {node {|c|}}}
child {node [bold] {|*|} child {node {|b|}} child[emph] {node {|c|}}};
\end{tikzpicture}
%

\begin{exercise}
  Expand the definition of |H2| in each case and check that the
  obtained conditions hold.
\end{exercise}

\subsection{Homomorphism on structures}
\label{sec:AlgHomo}
But so far our definition of homomorphism takes the rather
limited view that a single operation is transformed. Usually,
homomorphisms map a whole \emph{structure}.

Back to Wikipedia:
%
\begin{quote}
  More formally, a homomorphism between two algebras |A| and |B| is a
function |h : A → B| from the set |A| to the set |B| such that, for
every operation |fA| of |A| and corresponding |fB| of |B| (of arity,
say, |n|), |h(fA(x1,...,xn)) = fB(h(x1),...,h(xn))|.
\end{quote}

\subsubsection{Example: Log-space}

The general monoid homomorphism conditions for |h : A -> B| are:
%
\begin{spec}
h unit        =  unit             -- |h| takes units to units
h (x `op` y)  =  h x `op` h y     -- and distributes over |op| (for all |x| and |y|)
\end{spec}
%
Note that both |unit| and |op| have different types on the left and right hand sides.
%
On the left they belong to the monoid |(A, unitA, opA)| and on the
right the belong to |(B, unitB, opB)|.

\jp{the log/exp example from the start}

\jp{remind ANat and MNat here }

\begin{exercise}
Characterise the homomorphisms from |ANat| to |MNat|.
\end{exercise}
%
\begin{solution}
Let |h : ANat -> MNat| be a homomorphism.
%
Then it must satisfy the following conditions:
%
\begin{spec}
h 0        = 1
h (x + y)  = h x * h y  -- for all |x| and |y|
\end{spec}
%
For example |h (x + x) = h x * h x = (h x) ^ 2| which for |x = 1|
means that |h 2 = h (1 + 1) = (h 1) ^ 2|.

More generally, every |n| in |ANat| is equal to the sum of |n| ones:
|1 + 1 + ... + 1|.\jp{Here you have to talk about associativity and unit laws, otherwise it's too much to assume.}
%
Therefore
%
\begin{spec}
h n = (h 1) ^ n
\end{spec}
%
Every choice of |h 1| ``induces a homomorphism''.
%
This means that the value of the function |h|, for any natural number,
is fully determined by its value for |1|.
\end{solution}



Our examples |exp| and |log| are homomorphisms between monoids (either the additive monoid or the mutiplicative monoid). 
\jp{come back to the definition of homomorphism here and connect to the example.}

\jp{And, ... Summing up:
  
  H(h,C) = for every operation op in the class C, H2(h,op,op)
      Morphism:
      H(h,C1,C2)
    }

\subsubsection{Other homomorphisms}



\begin{exercise}
  Show that |const| is a homomorphism\jp{What structure are we talking
    about here? Also, if the additive one (minus zero), is it
    reasonable to expect that we remember that instance for functions
    here?}
\end{exercise}
%
The distribution law\jp{This is the first time that this name is used. Also, isn't it simply the homomorphism law?}
can be shown as follows:
%
\begin{spec}
  h a + h b                     =  {- |h = const| in this case -}
  const a  +  const b           =  {- By def. of |(+)| on functions -}
  (\x-> const a x + const b x)  =  {- By def. of |const|, twice -}
  (\x->  a + b )                =  {- By def. of |const| -}
  const (a + b)                 =  {- |h = const| -}
  h (a + b)
\end{spec}
%
We now have a homomorphism from values to functions, and you may
wonder if there is a homomorphism in the other direction.
%
The answer is ``Yes, many''.
%
\begin{exercise}
Show that |apply c| is a homomorphism for all |c|, where
|apply x f = f x|.
\end{exercise}


As we saw that every |n| in |ANat| is equal to the sum of |n| ones, every |Integer| is the sum of |n| ones or the negation of such a sum. Thus we can map every |Integer| to an element of a |Ring| (the multiplicative structure is used to provide |one|):
\end{example}

\begin{exercise}
  \label{ex:fromInteger}
  Assume an arbitrary Ring-homomorphism |f| from |Integer| to an
  arbitrary type |a|. Prove |f == fromInteger|, provided the
  definition in \cref{sec:overloaded-integer-literals}.
\end{exercise}

\begin{exercise}
  Continue extend the exponential-logarithm morphism to map AddGroup and MulGroup.
\end{exercise}

\section{Compositional semantics}

\subsection{Compositional functions are homomorphisms}
Consider a datatype of very simple integer expressions:
%
\begin{code}
data E = Add E E | Mul E E | Con Integer deriving Eq
e1, e2 :: E                             -- | 1 + 2 * 3 |
e1 = Add (Con 1) (Mul (Con 2) (Con 3))  -- | 1 +(2 * 3)|
e2 = Mul (Add (Con 1) (Con 2)) (Con 3)  -- |(1 + 2)* 3 |
\end{code}
%
|e1 = |
\begin{tikzpicture}[AbsSyn]
\begin{scope}[yshift=2ex]
\node(root) {|Add|}
child {node {|Con|} child {node(leftleaf) {|1|}}}
child {node {|Mul|}
  child {node {|Con|} child {node {|2|}}}
  child {node {|Con|} child {node(rightleaf) {|3|}}}};
\end{scope}
\node [draw=blue, ellipse, thick, fit = (root) (leftleaf) (rightleaf)] {};
\end{tikzpicture}
\hspace{2em}
|e2 = |
\begin{tikzpicture}[AbsSyn]
\begin{scope}[yshift=2ex]
\node(e2) {|Mul|}
child {node {|Add|}
  child {node {|Con|} child {node(leftleaf) {|1|}}}
  child {node {|Con|} child {node {|2|}}}}
child {node {|Con|} child {node(rightleaf) {|3|}}};
\end{scope}
\node [draw=blue, ellipse, thick, inner sep=-4pt, fit = (root) (leftleaf) (rightleaf)] {};
\end{tikzpicture}

As the reader may have guessed, the natural evaluator |eval : E -> Integer|
(defined later) is a homomorphism from |Add| to |(+)| and from |Mul| to
|(*)|.
%
But to practice the definition of homomorphism we will here check if
|even| or |isPrime| is a homomorphism from |E| to |Bool|.

Let's try to define |even : E ->
Bool| with the usual induction pattern (``wishful thinking''):
%
\begin{code}
even (Add x y)  =  evenAdd (even x) (even y)
even (Mul x y)  =  evenMul (even x) (even y)
even (Con c)    =  evenCon c

evenAdd :: Bool -> Bool -> Bool
evenMul :: Bool -> Bool -> Bool
evenCon :: Integer -> Bool
\end{code}
%
Note that |even| throws away lots of information: the domain is
infinite and the range is a two-element set.
%
This information loss could make it difficult to define the helper functions |evenAdd|,
etc. because they only get to work on the small range.
%
Still, in this case we are lucky: we can use the ``parity rules''
taught in elementary school: even plus even is even, etc.
%
In code we simply get:
%
\begin{code}
evenAdd = (==)
evenMul = (||)
evenCon = (0==).(`mod` 2)
\end{code}
\footnote{A perhaps more natural alternative would be to taken |odd| instead of |even| as the homomorphism. You can try it out as an exercise.}
%
\begin{exercise}
Exercise: prove |H2(even,Add,evenAdd)| and |H2(even,Mul,evenMul)|.
\end{exercise}

\subsection{An example of a non-compositional function}


Let's now try to define
|isPrime : E -> Bool| in the same way to see a simple example of a
non-compositional function.
%
In this case it is enough to just focus on one of the cases to already
see the problem:
%
\begin{code}
isPrime (Add x y)  =  isPrimeAdd (isPrime x) (isPrime y)
isPrimeAdd :: Bool -> Bool -> Bool
isPrimeAdd = error "Can this be done?"
\end{code}
%
As before, if we can define |isPrimeAdd|, we will get
|H2(isPrime,Add,isPrimeAdd)| ``by construction''
%
But it is not possible for |isPrime| to both satisfy its specification
and |H2(isPrime,Add,isPrimeAdd)|.
%
(To shorten the calculation we write just |n| for |Con n|.)
%
\begin{spec}
  False
= {- By spec. of |isPrime| (four is prime). -}
  isPrime (Add 2 2)
= {- by |H2| -}
  isPrimeAdd (isPrime 2) (isPrime 2)
= {- By spec. of |isPrime| (two is prime). -}
  isPrimeAdd (isPrime 2) True
= {- By spec. of |isPrime| (three is also prime). -}
  isPrimeAdd (isPrime 2) (isPrime 3)
= {- by |H2| -}
  isPrime (Add 2 3)
= {- By spec. of |isPrime| (five is prime). -}
  True
\end{spec}
%
But because we also know that |False /= True|, we have a contradiction.
%
Thus we conclude that |isPrime| is \emph{not} a homomorphism from |E|
to |Bool|, regardless of the choice of the operator corresponding to addition.


\section{Characterisation of compositionality as Folds}

\label{sec:compositionality-and-homomorphisms}
In general, for a syntax |Syn|, and a possible semantics (a type |Sem|
and an |eval| function of type |Syn -> Sem|), we call the semantics
\emph{compositional} if we can implement |eval| as a fold
%
Informally a ``fold'' is a recursive function which replaces each
abstract syntax constructor |Ci| of |Syn| with its semantic interpretation |ci| --- but without doing any other change in the structure. In particular,
moving around constructors is forbidden. 
%
For example, in our datatype |E|, a compositional semantics means that |Add|
maps to |add|, |Mul {-"\mapsto"-} mul|, and |Con {-"\mapsto"-} con|
for some ``semantic functions'' |add|, |mul|, and |con|.
%
\begin{center}
\begin{tikzpicture}[AbsSyn]
\node (lhs) {|Add|}
child {node {|Con 1|}}
child {node {|Mul|}
  child {node {|Con 2|}}
  child {node {|Con 3|}}};
%
\node (rhs) at (5,0) {|add|}
child {node {|con 1|}}
child {node {|mul|}
  child {node {|con 2|}}
  child {node {|con 3|}}};
%
\path (2,-1) edge[||->] (3,-1);
%
\end{tikzpicture}
\end{center}
As an example we can define a general |foldE| for the integer
expressions:
%
\begin{code}
foldE ::  (s -> s -> s) -> (s -> s -> s) -> (Integer -> s) -> (E -> s)
foldE add mul con = rec
  where  rec (Add x y)  = add (rec x) (rec y)
         rec (Mul x y)  = mul (rec x) (rec y)
         rec (Con i)    = con i
\end{code}
%
Notice that |foldE| has three function arguments corresponding to the
three constructors of |E|.
%
The ``natural'' evaluator to integers is then easy to define:
%
\begin{code}
evalE1 :: E -> Integer
evalE1 = foldE (+) (*) id
\end{code}
%
and with a minimal modification we can also make it work for other
numeric types:
%
\begin{code}
evalE2 :: Ring a => E -> a
evalE2 = foldE (+) (*) fromInteger
\end{code}

Another thing worth noting is that if we replace each abstract syntax
constructor with itself we get the identity function (a ``deep
copy''):
%
\begin{code}
idE :: E -> E
idE = foldE Add Mul Con
\end{code}


\jp{
  State that every fold is a (homo-?) morphism:
  Consider:

  fold phi (Op x1 ... xn) = phi(Op) (fold phi x1) ... (fold phi xn)

  Is every homomorphism a fold?

  Yes if the input structure is a deep-embedding.
}.

Finally, it is useful to capture the semantic functions (the
parameters to the fold) in a type class:
%
\begin{code}
class IntExp t where
  add  ::  t -> t -> t
  mul  ::  t -> t -> t
  con  ::  Integer -> t
\end{code}
%
In this way we can turn the arguments to the fold into a constraint on the return type:
%
\begin{code}
foldIE :: IntExp t => E -> t
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

To get a more concrete feeling for this, we define some concrete
values, not just functions:
%
\begin{code}
seven :: IntExp a => a
seven = add (con 3) (con 4)

testI :: Integer
testI = seven

testE :: E
testE = seven

check :: Bool
check = and  [  testI  ==  7
             ,  testE  ==  Add (Con 3) (Con 4)
             ,  testP  ==  "3+4"
             ]
\end{code}
%
We can also see |String| and |pretty| as an instance:
%
\begin{code}
instance IntExp String where
  add = prettyAdd
  mul = prettyMul
  con = prettyCon

pretty' :: E -> String
pretty' = foldIE

testP :: String
testP = seven
\end{code}

To sum up, by defining a class |IntExp| (and some instances) we can
use the metods (|add|, |mul|, |con|) of the class as ``smart
constructors'' which adapt to the context.
%
An overloaded expression, like |seven :: IntExp a => a|, which only uses
these smart constructors can be instantiated to different types,
ranging from the syntax tree type |E| to different semantic
interpretations (like |Integer|, and |String|).

      - What is a fold?
      - Restrictive version of algebra to Fa -> a (or a bunch thereof ...)
      - Relation between folds and homomorphisms

\subsection{Even folds can be wrong!}
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
We can implement |pretty| in the usual way as a ``fold''\jp{Fold is defined only informally below} over the
syntax tree using one ``semantic constructor'' for each syntactic
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
With this definition, note that |pretty : E -> String| is a
homomorphism (from |Add| to |prettyAdd| and from |Mul| to |prettyMul|)
regardless of what their definitions are.

Now, if we try to implement the semantic constructors without thinking
too much we would get the following:
%
\begin{code}
prettyAdd xs ys  = xs ++ "+" ++ ys
prettyMul xs ys  = xs ++ "*" ++ ys
prettyCon c      = show c

p1, p2 :: String
p1 = pretty e1
p2 = pretty e2

trouble :: Bool
trouble = p1 == p2
\end{code}
%
Note that |e1| and |e2| are not equal, but they still pretty-print to
the same string.
%
This means that |pretty| is doing something wrong: the inverse,
|parse|, is ambiguous.
%
There are many ways to fix this, some more ``pretty'' than others, but
the main problem is that some information is lost in the translation:
|pretty| is not invertible.

Thus, we can see that a function can be a homomorphism and still be
``wrong''.

\paragraph{For the curious}
%
One solution to the problem with parentheses is to create three
(slightly) different functions intended for printing in different
contexts.
%
The first of them is for the top level, the second for use inside
|Add|, and the third for use inside |Mul|.
%
These three functions all have type |E -> String| and can thus be
combined with the tupling transform into one function returning a
triple: |prVersions :: E -> (String, String, String)|.
%
The result is the following:
%
\begin{code}
prTop :: E -> String
prTop e =  let (pTop, _, _) = prVersions e
           in pTop

type ThreeVersions = (String, String, String)
prVersions :: E -> ThreeVersions
prVersions = foldE prVerAdd prVerMul prVerCon

prVerAdd :: ThreeVersions -> ThreeVersions -> ThreeVersions
prVerAdd (xTop, xInA, xInM) (yTop, yInA, yInM) =
  let s = xInA ++ "+" ++ yInA  -- use |InA| because we are ``in |Add|''
  in (s, paren s, paren s)     -- parens needed except at top level

prVerMul :: ThreeVersions -> ThreeVersions -> ThreeVersions
prVerMul (xTop, xInA, xInM) (yTop, yInA, yInM) =
  let s = xInM ++ "*" ++ yInM  -- use |InM| because we are ``in |Mul|''
  in (s, s, paren s)           -- parens only needed inside |Mul|

prVerCon :: Integer -> ThreeVersions
prVerCon i =
  let s = show i
  in (s, s, s)                 -- parens never needed

paren :: String -> String
paren s = "(" ++ s ++ ")"
\end{code}

Exercise: Another way to make this example go through is to refine the
semantic domain from |String| to |Precedence -> String|.
%
This can be seen as another variant of the result after the tupling
transform: if |Precedence| is an |n|-element type then |Precedence ->
String| can be seen as an |n|-tuple.
%
In our case a three-element |Precedence| would be enough.


\section{Initial and Free structures}

\subsection{Initial Ring}


\subsection{Free Ring}
\jp{Rewrite this whole subsection to make it comprehensible}
Earlier, we saw that |eval| is compositional, while |eval'| (from \cref{sec:evalD}) is not.
%
Another way of phrasing that is to say that |eval| is a homomorphism,
while |eval'| is not.
%
To see this, we need to make explicit the structure of |FunExp|:
%
\begin{spec}
instance Additive FunExp where
  (+) = (:+:)
  -- ...

instance Multiplicative FunExp where
  (*) = (:*:)
  -- ...

instance Fractional FunExp where
  -- Exercise: fill in

instance Floating FunExp where
  exp          =  Exp
  -- Exercise: fill in
\end{spec}
%
and so on.
%

\begin{exercise}
Complete the type instances for |FunExp|.
\end{exercise}
For instance, we have
%
\begin{spec}
eval (e1 :*: e2)  =  eval e1 * eval e2
eval (Exp e)      =  exp (eval e)
\end{spec}
%
These properties do not hold for |eval'|, but do hold for |evalD|.

The numerical classes in Haskell do not fully do justice to the
structure of expressions.\jp{Here you are talking about the language of functions of one variable, but this is very hard to infer from the context.} For example, they do not contain an identity
operation, which is needed to translate |Id|, nor an embedding of
doubles, etc.
%
If they did, then we could have evaluated expressions more abstractly:
%
\jp{Probably this is saying that FunExp is the free field, or something like that?}
\begin{spec}
eval :: GoodClass a  =>  FunExp -> a
\end{spec}
%
where |GoodClass| gives exactly the structure needed for the
translation.
%
With this class in place we can define generic expressions using smart
constructors just like in the case of |IntExp| above.
%
For example, we could define
%
\begin{code}
twoexp :: GoodClass a => a
twoexp = mulF (constF 2) (expF idF)
\end{code}
%
and instantiate it to either syntax or semantics:
%
\begin{code}
testFE :: FunExp
testFE = twoexp

testFu :: Func
testFu = twoexp
\end{code}

\begin{exercise}
Define the class |GoodClass| and instances for |FunExp| and
|Func = REAL -> REAL| to make the example work.
\end{exercise}
%
Find another instance of |GoodClass|.
\jp{I am lost here. (Perhaps we can use  |Ring t =>| here?) The only new operations appears to be |idF|. (Check if |expF| is useful.)}
%
\begin{code}
class GoodClass t where
  constF :: REAL -> t
  addF :: t -> t -> t
  mulF :: t -> t -> t
  expF :: t -> t
  idF  :: t
  -- ... Exercise: continue to mimic the |FunExp| datatype as a class

newtype FD a = FD (a -> a, a -> a)

instance Ring a => GoodClass (FD a) where
  addF = evalDApp
  mulF = evalDMul
  expF = evalDExp
  -- ... Exercise: fill in the rest

evalDApp = error "Exercise"
evalDMul = error "Exercise"
evalDExp = error "Exercise"

instance GoodClass FunExp where
  addF = (:+:)
  -- ...

instance GoodClass (REAL->REAL) where
  addF = (+)
  -- ...
\end{code}
%
We can always define a homomorphism from |FunExp| to \emph{any}
instance of |GoodClass|, in an essentially unique way.
%
In the language of category theory, the datatype |FunExp| is an
initial algebra\jp{Probably should be a footnote. Or should we define an explain this term properly and use it below?}.

Let us explore this in the simpler context of |Monoid|.
%

\subsection{Free Monoid}
Ignoring its laws, the language of monoid expressions is given by
%
\begin{code}
type Var      =  String

data MExpr    =  Unit  |  Op MExpr MExpr  |  V Var
\end{code}
%
Alternatively, we could have parametrised |MExpr| over the type of
variables.

Just as in the case of FOL terms, we can evaluate an |MExpr| in a
monoid instance if we are given a way of interpreting variables, also
called an assignment:
%
\begin{code}
evalM :: Monoid a => (Var -> a) -> (MExpr -> a)
\end{code}
%
Once given an |f :: Var -> a|, the homomorphism condition defines
|evalM|:
%
\begin{code}
evalM  f  Unit        =  unit
evalM  f  (Op e1 e2)  =  op (evalM f e1) (evalM f e2)
evalM  f  (V x)       =  f x
\end{code}
%
(Observation: In |FunExp|, the role of variables was played by |REAL|,
and the role of the assignment by the identity.)

\subsection{\extraMaterial A generic Free construction}
%include FreeMonoid.lhs

\subsection{Summary}
\jp{
 - Deep embeddings (FunExp is a deep free Ring-like structure; modulo laws. )
 - Shallow embeddings (-> is a shallow free ...; in this case we don't have to worry about laws)}

\section{Application: Derivatives}

% TODO: perhaps not include this here. The background is that this material did not quite fit in the previous lecture. Also some repition was needed.
% Alternatively, move all computation of derivatives here.

Review \refSec{sec:evalD} again with the definition of |eval'|
being non-compositional (just like |isPrime|) and |evalD| a more
complex, but compositional, semantics.
%

We want to implement |eval' = eval . derive| in the following diagram:

\tikzcdset{diagrams={column sep = 2cm, row sep = 2cm}}
\quad%
\begin{tikzcd}
  |FunExp| \arrow[r, "|eval|"] \arrow[d, "|derive|"]
                               \arrow[dr, "|eval'|"]  & |(REAL -> REAL)| \arrow[d, "D"] \\
  |FunExp| \arrow[r, "|eval|"]                        & |(REAL -> REAL)|
\end{tikzcd}

As we saw in \refSec{sec:evalD} this does not work in the sense
that |eval'| cannot directly be implemented compositionally.
%
The problem is that some of the rules of computing the derivative
depends not only on the derivative of the subexpressions, but also on
the subexpressions before taking the derivative.
%
A typical example of the problem is |derive (f :*: g)| where the
result involves not only |derive f| and |derive g|, but also |f| and
|g|.
%

The solution is to extend the return type of |eval'| from one
semantic value |f| of type |Func = REAL -> REAL| to two such values
|(f, f') :: (Func, Func)| where |f' = D f|.
%
One way of expressing this is to say that in order to implement |eval'
:: FunExp -> Func| we need to also compute |eval :: FunExp -> Func|.
%
Thus we need to implement a pair of |eval|-functions |(eval, eval')|
together.
%
Using the ``tupling transform'' we can express this as computing just
one function |evalD :: FunExp -> (Func, Func)| returning a pair of |f|
and |D f| at once.
%

This combination \emph{is} compositional, and we can then get |eval'|
back as the second component of |evalD e|:
%
\begin{spec}
eval' :: FunExp -> Func
eval' = snd . evalD
\end{spec}

% \tikzcdset{diagrams={column sep = 2cm, row sep = 2cm}}
% \quad%
% \begin{tikzcd}
%   |FunExp| \arrow[r, "|evalD|"] \arrow[d, "|derive|"]
%                                 \arrow[dr, "|eval'|"]  & |(Func, Func)| \arrow[d, "D"] \\
%   |FunExp| \arrow[r, "|evalD|"]                        & |(Func, Func)|
% \end{tikzcd}


\section{Summing up}

The following correspondence summarises the discussion so far:
\jp{Is it really true that initial algebra are deep embeddings? }
\begin{tabular}{ll}
      Computer Science      &   Mathematics
\\\hline
\\    DSL                   &   structure (category, algebra, ...)
\\    deep embedding, abstract syntax        &   initial algebra
\\    shallow embedding     &   any other algebra
\\    semantics             &   homomorphism from the initial algebra
\end{tabular}

The underlying theory of this table is a fascinating topic but mostly
out of scope for these lecture notes (and the DSLsofMath course).
%
See
\href{http://wiki.portal.chalmers.se/cse/pmwiki.php/CTFP14/CoursePlan}{Category
  Theory and Functional Programming} for a whole course around this
(lecture notes are available on
\href{https://github.com/DSLsofMath/ctfp2014}{github}).

\subsection{Definitions and representations}
\jp{I think that this is meant to say that the initial structure is not the only
representent of the structure? Unclear/pedantic?}
We defined a |Ring| structure on pairs |(REAL, REAL)| by requiring
the operations to be compatible with the interpretation |(f a, f' a)|.
%
For example
%
\begin{spec}
(x, x') *? (y, y') = (x * y, x' * y + x * y')
\end{spec}
%
There is nothing in the ``nature'' of pairs of |REAL| that forces
this definition upon us.
%
We chose it, because of the intended interpretation.

This multiplication is obviously not the one we need for \emph{complex
  numbers}. It would be instead:
%
\begin{spec}
(x, x') *. (y, y') = (x * y - x' * y', x * y' + x' * y)
\end{spec}
%
Again, there is nothing in the nature of pairs that foists this
operation on us.
%
In particular, it is, strictly speaking, incorrect to say that a
complex number \emph{is} a pair of real numbers.
%
The correct interpretation is that a complex number can be
\emph{represented} by a pair of real numbers, provided we define the
operations on these pairs in a suitable way.

The distinction between definition and representation is similar to
the one between specification and implementation, and, in a certain
sense, to the one between syntax and semantics.
%
All these distinctions are frequently obscured, for example, because
of prototyping (working with representations / implementations /
concrete objects in order to find out what definition / specification
/ syntax is most adequate).
%
They can also be context-dependent (one man's specification is another
man's implementation).
%
Insisting on the difference between definition and representation can
also appear quite pedantic (as in the discussion of complex numbers
above).
%
In general though, it is a good idea to be aware of these
distinctions, even if they are suppressed for reasons of brevity or
style.
%
We will see this distinction again in
\refSec{sec:polynotpolyfun}.

\section{Beyond Algebras: Co-algebra and the Stream calculus}

In the coming chapters there will be quite a bit of material on
infinite structures.
%
These are often captured not by algebras, but by co-algebras.\jp{unfortunately the definition that we gave for algebras seem to fit co-algebras just as well?}
%
We will not build up a general theory of co-algebras in this notes,
but because we will be using infinite streams in the upcoming chapters
we will expose right here their co-algebraic structure.

%include AbstractStream.lhs


=============================================================================================


\section{Spillover to sort}











\subsection{Other homomorphisms}

In \refSec{sec:FunNumInst}, we defined a |Ring| instance for
functions with a |Ring| codomain.
%
If we have an element of the domain of such a function, we can use it
to obtain a homomorphism from functions to their codomains:\jp{I have no idea what this tries to say.}
%
\begin{spec}
Ring a => x ->  (x -> a) -> a
\end{spec}
%
As suggested by the type, the homomorphism is just function
application:
%
\begin{spec}
apply :: a -> (a -> b) -> b
apply a = \f -> f a
\end{spec}
\label{sec:apply}

Indeed, writing |h = apply c| for some fixed |c|, we have
%
\begin{spec}
     h (f + g)         =  {- def. |apply| -}

     (f + g) c         =  {- def. |+| for functions -}

     f c + g c         =  {- def. |apply| -}

     h f + h g
\end{spec}
%
etc.

Can we do something similar for |FD|?
The elements of |FD a| are pairs of functions, so we can take
%
\label{sec:applyFD}
\begin{code}
type Dup a = (a, a)

applyFD ::  a ->  FD a          ->  Dup a
applyFD     c     (FD (f, f'))  =   (f c, f' c)
\end{code}

We now have the domain of the homomorphism |(FD a)| and the
homomorphism itself |(applyFD c)|, but we are missing the structure on
the codomain, which now consists of pairs |Dup a = (a, a)|.
%
In fact, we can \emph{compute} this structure from the homomorphism
condition.
%
For example (we skip the constructor |FD| for brevity):
%
\begin{spec}
     h ((f, f') * (g, g'))                       =  {- def. |*| for |FD a| -}

     h (f * g, f' * g + f * g')                  =  {- def. |h = applyFD c| -}

     ((f * g) c, (f' * g + f * g') c)            =  {- def. |*| and |+| for functions -}

     (f c * g c, f' c * g c + f c * g' c)        =  {- |let x=f c; y=g c; x'=f' c; y'=g' c| -}

     (  x * y  ,   x' * y   +   x * y'  )        =  {- \textbf{introduce |*?| to make the ends meet} -}

     (  x, x'  ) *? (y  , y'  )                  =  {- expand shorter names again -}

     (f c, f' c) *? (g c, g' c)                  =  {- def. |h = applyFD c| -}

     h (f, f') *? h (g, g')
\end{spec}
%
The identity will hold if we take
%
\begin{code}
(*?) :: Ring a =>  Dup a -> Dup a -> Dup a
(x, x') *? (y, y')  =  (x * y, x' * y + x * y')
\end{code}
%
Thus, if we define a ``multiplication'' on pairs of values using
|(*?)|, we get that |(applyFD c)| is a |Multiplicative|-homomorphism for all |c|.
%
We can now define an instance
%
\begin{code}
instance Ring a => Multiplicative (Dup a) where
  (*) = (*?)
  -- ... exercise
\end{code}
%
\begin{exercise}
Complete the instance declarations for |Dup REAL|.
\end{exercise}


Note: because this computation goes through also for the other cases we can
actually work with just pairs of values (at an implicit point |c ::
a|) instead of pairs of functions.
%
Thus we can define a variant of |FD a| to be |type Dup a = (a, a)|

Hint: Something very similar can be used for Assignment 2.\jp{What's that?}



\subsection{Some helper functions}

\begin{code}
instance Additive E where -- Some abuse of notation (no proper |negate|, etc.)
  (+)  = Add
  zero = Con zero

instance Multiplicative E where
  (*)  = Mul
  one  = Con one

instance AddGroup E where -- TODO: probably this is there to allow for fromInteger to work (with Num). But the funny implementation is no longer needed.
  negate = negateE

negateE (Con c) = Con (negate c)
negateE _ = error "negate: not supported"
\end{code}
%
%TODO: Perhaps include the comparison of the |Ring t => Ring (Bool -> t)| instance (as a special case of functions as |Ring|) and the |Ring r => Ring (r,r)| instance from the complex numbers. But it probably takes us too far off course. blackboard/W5/20170213_104559.jpg



%include E4.lhs
