\chapter{Compositionality and Algebras}
% Based on ../../2016/Lectures/Lecture06  and
% based on ../../2016/Lectures/Lecture09.lhs
\label{sec:CompSem}

%if False
\begin{code}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds, RebindableSyntax #-}
module DSLsofMath.W04 where
import Prelude hiding (Monoid, even, Num(..), recip, sin, cos, exp, (^))
import DSLsofMath.FunExp hiding (eval)
import DSLsofMath.Algebra
import DSLsofMath.Derive (derive)
-- import DSLsofMath.W03 (derive)
type ℝ = REAL
\end{code}
%endif
%

Algebraic structures are fundamental to the structuralist point of
view in mathematics, which emphasises relations between objects rather
than the objects themselves and their representations.
%
Furthermore, each mathematical domain has its own fundamental
structures.
%
Once these structures have been identified, one tries to push their
study as far as possible \emph{on their own terms}, without picking
any particular representation (which may have richer structure than
the one we want to study).
%
For example, in group theory, one starts by exploring the consequences
of just the group structure, rather than introducing any particular
group (like integers) which have (among others) an order structure and
monotonicity.

Furthermore, mappings or (translations) between such structures
becomes an important topic of study.
%
When such mappings preserve the structure, they are called
\emph{homomorphisms}.
%
As two examples, we have the homomorphisms |exp| and |log|, specified
as follows:
%
\begin{spec}
  exp :  REAL     ->  RPos
  exp    0        =   1                 --  \(e^0 = 1\)
  exp    (a + b)  =   exp a  *  exp b   --  \(e^{a+b} = e^a e^b\)

  log :  RPos     ->  REAL
  log    1        =   0                 -- \(\log 1 = 0\)
  log    (a * b)  =   log a  +  log b   -- \(\log(ab) = \log a + \log b \)
\end{spec}
%
What we recognize as the familiar laws of exponentiation and
logarithms arise from homomorphism conditions, which
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
The laws cannot easily be captured in the class, but can be formulated
as the following propositions:
%
\begin{spec}
  ∀ x : a? (unit `op` x == x  ∧  x `op` unit == x)
  ∀ x, y, z : a? (x `op` (y `op` z) == (x `op` y) `op` z)
\end{spec}
  The first law ensures that |unit| is indeed the unit of |op| and the
  second law is the familiar associativity law for |op|.
\end{example}

\begin{example}
  Examples of monoids include numbers with addition, |(REAL, 0, (+))|,
  positive numbers with multiplication |(RPos, 1, (*))|, and even endofunctions
  with composition |(a->a,id, (.))|.
  %
  (An ``endofunction'', also known as ``endomorphism'' is a function
  of type |X->X| for some set |X|.)
  \label{ex:endofunction}
\end{example}

\begin{exercise}
  Define the above monoids as type class instances and check that the
  laws are satisfied.
\end{exercise}

\begin{example}
  To make this a bit more concrete, here are two examples of monoids
  in Haskell: the additive monoid |ANat| and the multiplicative monoid
  |MNat|.
%
%if False
Included just for type-checking (it is really inefficient in practice).
\begin{code}
data Natural = Zero | Succ Natural deriving (Show, Eq)
addNatural :: Natural -> Natural -> Natural
addNatural Zero      m = m
addNatural (Succ n)  m = Succ (addNatural n m)

mulNatural :: Natural -> Natural -> Natural
mulNatural Zero      m     = zero
mulNatural n         Zero  = zero
mulNatural (Succ n)  m     = addNatural m (mulNatural n m)

instance Additive Natural        where zero = Zero;      (+) = addNatural
instance Multiplicative Natural  where one = Succ Zero;  (*) = mulNatural
\end{code}
%endif
\label{sec:anat-mnat}
\begin{code}
newtype ANat      =  A Natural          deriving (Show, Eq)

instance Monoid ANat where
  unit            =  A zero
  op (A m) (A n)  =  A (m + n)

newtype MNat      =  M Natural          deriving (Show, Eq)

instance Monoid MNat where
  unit            =  M one
  op (M m) (M n)  =  M (m * n)
\end{code}
\end{example}

In Haskell there can ever be at most one instance of a given class for
a given type, so we cannot define two |instance Monoid Natural|: we
must make a |newtype| whose role is to indicate which of the two
possible monoids (additive or applicative) applies in a given context.
%
But, in mathematical texts the constructors |M| and |A| are usually
omitted, and instead the names of the operations suggest which of the
monoids one is referring to.
%
To be able to conform to that tradition we define two separate
classes, one for the additive and one for the multiplicative monoids,
as follows.
\label{sec:ring-like-classes}
\pj{Perhaps use two one-liners to see the matching structure?}
% class Additive a        where  zero  :: a;   (+)  :: a -> a -> a
% class Multiplicative a  where  one   :: a;   (*)  :: a -> a -> a
\begin{spec}
class Additive a where
  zero :: a
  (+) :: a -> a -> a

class Multiplicative a where
  one :: a
  (*) :: a -> a -> a
\end{spec}
This is what we have done in \cref{sec:numeric-classes}.

\begin{example}\textbf{Groups and rings}
  Another important structure are groups, which are monoids augmented with an
  inverse.
  %
  To continue our mathematically-grounded |Num| replacement, we have
  also defined the additive group as follows:

\begin{spec}
class Additive a => AddGroup a where
  negate :: a -> a
\end{spec}

Groups demand that the inverse (called |negate| for the additive
group) act like an inverse.
%
Namely, applying the operation to an element and its inverse should
yield the unit of the group.
%
Thus, for the additive group, the laws look like this:

\begin{spec}
negate a + a = zero
a + negate a = zero
\end{spec}

And thus we can define subtraction as
\begin{spec}
(-) :: AddGroup a => a -> a -> a
a - b = a + negate b
\end{spec}

When the additive monoid is abelian (commutative) and addition
distributes over multiplication, we have a |Ring|.
%
As always we cannot conveniently specify laws in Haskell typeclasses
and thus define |Ring| simply as the conjunction of |AddGroup| and
|Multiplicative|:
\begin{spec}
type Ring a = (AddGroup a, Multiplicative a)
\end{spec}
\end{example}

With that, we have completed the structural motivation of our
replacement for the |Num| class! \jp{Show some instances?}
\pj{Provide a comparison table / figure relating the operations of
  |Num|, |Fractional|, etc. with those of |Additive|, etc.}

We note right away that one can have a multiplicative group structure
as well, whose inverse is called the reciprocal (traditionally
abbreviated as |recip| in Haskell).
%
With that in place, division can be defined is terms of multiplication
and reciprocal.
\begin{joincode}%class Multiplicative is defined earlier
\begin{spec}
class Multiplicative a => MulGroup a where
  recip :: a -> a -- reciprocal
\end{spec}
\begin{code}
(/) :: MulGroup a => a -> a -> a
a / b = a * recip b
\end{code}
\end{joincode}
Often the multiplicative group is added to a |Ring|, and one has a |Field|:
\label{sec:fields-defintion}
\begin{code}
type Field a = (Ring a, MulGroup a)
\end{code}
For fields, the reciprocal is not defined at |0|.
%
We will not capture this precondition in types: it would cause too
much notational awkwardness.

\section{Homomorphisms}
The Wikipedia definition of homomorphism states that ``A homomorphism
is a structure-preserving map between two algebraic structures of the
same type''.

\subsection{(Homo)morphism on one operation}

As a stepping stone to capture this idea, we can define a
ternary predicate |H2|. The first argument |h|, is the map. The second
(|Op|) and third (|op|) arguments correspond to the algebraic structures.
%
\begin{spec}
  H2(h,Op,op)  =  Forall x (Forall y (h(Op x y) == op (h x) (h y)))
\end{spec}
%
If the predicate |H2(h,Op,op)| holds, we say that |h : A -> B| is a
homomorphism from |Op : A->A->A| to |op : B->B->B|.
%
Or that |h| is a homomorphism from |Op| to |op|.
%
Or even that |h| is a homomorphism from |A| to |B| if the operators
are clear from the context.
%
We have seen several examples in earlier chapters:
%
\begin{enumerate}
\item in \refSec{sec:complexcase} we saw that |evalE : ComplexE ->
  ComplexD| is a homomorphism from the syntactic operator |Plus| to
  the corresponding semantic operator |plusD|.
\item in \refSec{sec:logic} we saw de Morgan's laws, which says that
  ``not'' (|not|) is a homomorphism in two ways: |H2(not,(&&),(||||))|
  and |H2(not,(||||),(&&))|.
\item in \refSec{sec:FunExp} we saw that |eval : FunExp -> Func| is a
  homomorphism from syntactic |(:*:)| to semantic |(*)| for functions
\item
  \label{distributivity-as-homomorphism}
  If |(*)| distributes
  over |(+)| for some type |A| then |(*c) : A -> A| is a homomorphism
  from |(+)| to |(+)|: |H2((*c),(+),(+))|.
\end{enumerate}

To see how this last item plays out, it can be helpful to study the
syntax trees of the left and right hand sides of the distributive law:
|((a+b)*c = (a*c)+(b*c))|.
%
We observe that the function |(*c)| is ``pushed down'' to both |a| and |b|:

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
So far our definition of homomorphism takes the rather
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

In our Haskell interpretation, the above would mean that we have
|H2(h,fA,fB)| for every binary method |f| in a given class |C| and
more generally |Hn(h,opA,opB)| for each operation |op| of arity |n|.
%
We can also use type class overloading to write |Hn(h,op,op)| where
the first occurence of |op| comes from the |C A| instance and the
second one from |C B|.


\begin{example}
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
\end{example}

\begin{example}
  Hence, the function |exp| is a monoid homomorphism from (|Real|,0,+)
  to (|RPos|,1,*).
  \begin{spec}
  exp  :  REAL  ->  RPos
  exp  0        =   1                 --  \(e^0 = 1\)
  exp  (a + b)  =   exp a  *  exp b   --  \(e^{a+b} = e^a e^b\)
  \end{spec}
\end{example}
%
In the above example, we have simply checked the homomorphism
conditions for the exponential function.
%
But we can try to go the other way around: knowing that a function |h|
is homomorphism, what kind of function can |h| be?

\begin{example}
  \label{ex:exponential-as-homomorphism}
As an example, let us can characterise the homomorphisms from |ANat| to
|MNat| (from \cref{sec:anat-mnat}).

Let |h : ANat -> MNat| be a monoid homomorphism.
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

More generally, every natural number can is equal to the sum of |n| ones:
|1 + 1 + ... + 1|.
%
Therefore
%
\begin{spec}
h n   =  h (1  + ...   + 1)
      =  h 1   *  ...  * h 1
      =  (h 1) ^ n
\end{spec}
%
That is, every choice of |h 1| induces a homomorphism from |ANat| to |MNat|.
%
This means that the value of the function |h|, for any natural number,
is fully determined by its value for |1|.

In other words, we know that every |h| (homomorphism from |ANat| to
|MNat|) is of the form
\begin{spec}
h n = a ^ n
\end{spec}
for a given natual number |a = h 1|.
%
So, the set of homomorphisms between the additive monoid and the
multiplicative monoid is the set of exponential functions, one for
every base |a|.
\end{example}

\begin{exercise}
  \label{ex:fromInteger}
  Assume an arbitrary Ring-homomorphism |f| from |Integer| to an
  arbitrary type |a|. Prove |f == fromInteger|, provided the
  definition in \cref{sec:overloaded-integer-literals}.
\end{exercise}

\begin{solution}
The homomorphism conditions include:

\begin{spec}
f zero = zero
f (one + x) = one + (f x)
f (negate x) = negate (f x)
\end{spec}

By substitution we get the following equations:

\begin{spec}
f zero = zero
f x = one + (f (x - one))
f x = negate (f (negate x))
\end{spec}

These are compatible with the behaviour of |fromInteger|, but they
also completely fix the behaviour of |f| if |x| is an integer, because
it can either be zero, positive or negative.
\end{solution}

\subsubsection{Other homomorphisms}


\begin{exercise}
  Show that |const| is a homomorphism.
\end{exercise}
%
\begin{solution}
  This exercise is underspecified (what structure? from and to which
  types?) so we need to explore a bit to find a reasonable
  interpretation.
  %
  We can start simple and use addition |(+)| as the structure, thus we
  want to show |H2(h,(+),(+))| where |h = const| is of type |A->B|.
  %
  Next we need to identify the types |A| and |B| where addition is
  used in the predicate.
  %
  We have |const :: a -> (x->a)| for any |a| and |x| and we can take
  |A=a=REAL| and |B=x->REAL|.
  %
  As |B| is a function type the |(+)| on that side is addition of
  functions, which we defined in \refSec{sec:FunNumInst}.
  %
  The homomorphism law (that |h| distributes over |(+)|) can be shown
  as follows:
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
\end{solution}
%
The answer is ``Yes, many''. Such homomorphisms take the form |apply
c|, for any |c|.
%
\begin{exercise}
  \label{ex:apply}
Show that |apply c| is an |Additive| homomorphism for all |c|, where
|apply x f = f x|.
\end{exercise}
\begin{solution}
Indeed, writing |h = apply c| for some fixed |c|, we have
%
\begin{spec}
     h (f + g)         =  {- def. |apply| -}

     (f + g) c         =  {- def. |+| for functions -}

     f c + g c         =  {- def. |apply| -}

     h f + h g
   \end{spec}
   and
\begin{spec}
     h zero            =  {- def. |apply| -}

     zero c            =  {- def. |zero| for functions -}

     zero
   \end{spec}
\end{solution}

\begin{exercise}
  Extend the exponential-logarithm morphism to relate |AddGroup| and |MulGroup|.
\end{exercise}

\section{Compositional semantics}

\subsection{Compositional functions are homomorphisms}
\label{sec:compositionality-and-homomorphisms}
Consider a datatype of very simple integer expressions:
%
\begin{code}
data E = Add E E | Mul E E | Con Integer deriving Eq
e1, e2 :: E                             -- | 1 + 2 * 3 |
e1 = Add (Con 1) (Mul (Con 2) (Con 3))  -- | 1 +(2 * 3)|
e2 = Mul (Add (Con 1) (Con 2)) (Con 3)  -- |(1 + 2)* 3 |
\end{code}
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
Bool| with the usual induction pattern
%if lectureNotes
(``wishful thinking'')
%endif
:
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
In code we simply get:\footnote{A perhaps more natural alternative would be to taken |odd| instead of |even| as the homomorphism. You can try it out as an exercise.}
%
\begin{code}
evenAdd = (==)
evenMul = (||)
evenCon = (0==).(`mod` 2)
\end{code}

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
|H2(isPrime,Add,isPrimeAdd)| ``by construction''.
%
But it is not possible for |isPrime| to both satisfy its specification
and |H2(isPrime,Add,isPrimeAdd)|.
%
(To shorten the calculation we write just |n| for |Con n|.)
%
\begin{spec}
  False
= {- By spec. of |isPrime| (four is not prime). -}
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


\section{Folds}

\label{sec:folds}
In general, for a syntax |Syn|, and a possible semantics (a type |Sem|
and an |eval| function of type |Syn -> Sem|), we call the semantics
\emph{compositional} if we can implement |eval| as a fold.
%
Informally a ``fold'' is a recursive function which replaces each
abstract syntax constructor |Ci| of |Syn| with its semantic
interpretation |ci| --- but without doing any other change in the
structure. In particular, moving around constructors is forbidden.
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
constructor with itself we get an identity function, sometimes known
as a ``deep copy'':
%
\begin{code}
idE :: E -> E
idE = foldE Add Mul Con
\end{code}


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
In this way we can turn the arguments to the fold into a constraint on
the return type:
%
\begin{code}
foldIE :: IntExp t => E -> t
foldIE = foldE add mul con

instance IntExp E where
  add  = Add
  mul  = Mul
  con  = Con

instance IntExp Integer where
  add  = (+)
  mul  = (*)
  con  = id

idE' :: E -> E
idE' = foldIE

evalE' :: E -> Integer
evalE' = foldIE
\end{code}


Additionally |IntExp| is the underlying algebraic structure of the
fold.
%
The function |foldIE| is a homomorphism which maps the |IntExp E|
instance to another (arbitrary) instance |IntExp e|.
%
This is what a fold is in general.
%
Given a structure |C|, a fold is a homomorphism from a realisation of
|C| as a data-type.
%
We can note at this point that a class |C a| can be realised as a
datatype only if all the functions of |class C a| return
|a|.
%
(Otherwise the constructors could create another type; and so they are
not constructors any more.)
%
This condition was satisfied in the case of our |class IntExp t|: all
function signatures end with |... -> t|.
%
When this condition is satisfied, we say that the class is an
\emph{algebra} --- not just any algebraic structure.\footnote{Indeed,
  this terminology can be confusing.}

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
We can implement |pretty| in the usual way as a fold over the
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

We can also see |String| and |pretty| as an instance of the |IntExp| class:
%
\begin{code}
instance IntExp String where
  add = prettyAdd
  mul = prettyMul
  con = prettyCon

pretty' :: E -> String
pretty' = foldIE

\end{code}
%
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
There are many ways to fix this, some more ``pretty'' than others.
%
One way to characterise the issue is that some information is lost in
the translation: |pretty| is not invertible.

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


\section{Initial and Free Structures}

In \cref{sec:folds} we started with a data-type, and derived an
algebraic structure (more precisely an algebra) from it. But we can go
in the other direction: start with an algebra and derive a datatype
which capture the structure of the algebra, but nothing more. This is
representation is called the initial algebra.

\subsubsection{The Initial Monoid}

As a first example, consider an initial algebra for monoids (an initial monoid for short).

We know that we have at least one object: the |unit|.
But we can also construct more objects using |op|:
|unit `op` unit|, |unit `op` (unit `op` unit)|, etc.
So a draft for the initial monoid could be:

\begin{spec}
data M where
  Unit :: M
  Op :: M -> M -> M
\end{spec}
or:
\begin{spec}
data M = Unit | Op M M
\end{spec}

But we also have the unit laws, which in particular tell us that |unit
`op` unit == unit|. So, in fact, we are left with a single element:
the |unit|.  A representation of the initial monoid is then simply:

\begin{spec}
data M = Unit
\end{spec}

As one might guess, there are not many interesting applications of the
initial monoid, so let us consider another structure.

\subsubsection{The Initial Ring}

Gathering all function in various type classes, we find that a |Ring|
corresponds to the following algebra --- again we start by ignoring laws:

\begin{spec}
zero :: a
(+) :: a -> a -> a
negate :: a -> a
one :: a
(*) :: a -> a -> a
\end{spec}

In this case, we can start with |zero| and |one|. As before, using
addition on |zero| or multiplication on |one| would yield no more
elements. But we can use addition on |one|, and get |one + one|, |one
+ one + one|, etc. Because of associativity, we don't have to --- and
ought not to --- write parentheses. Let's write an addition of |n|
ones as |n|. What about multiplying? Are we going to get more kinds of
numbers from that? No, because of distributivity. For example:

\begin{spec}
(one + one) * (one + one) == one + one + one + one
\end{spec}

By following this line of reasoning to its conclusion, we will find that
the initial |Ring| is the set of integers.


\subsection{A general initial structure}

In Haskell, the type |C a => a| is a generic way to represent the
initial algebra for a class |C|.  To get a more concrete feeling for
this, let us return to |IntExp|, and consider a few values of type
|IntExp a => a|.
%
\begin{code}
seven :: IntExp a => a
seven = add (con 3) (con 4)

testI :: Integer
testI = seven

testE :: E
testE = seven

testP :: String
testP = seven

check :: Bool
check = and  [  testI  ==  7
             ,  testE  ==  Add (Con 3) (Con 4)
             ,  testP  ==  "3+4"
             ]
\end{code}
%

By defining a class |IntExp| (and some instances) we can
use the methods (|add|, |mul|, |con|) of the class as ``smart
constructors''\jp{what is that?} which adapt to the context.
%
An overloaded expression, like |seven :: IntExp a => a|, which only
uses these smart constructors can be instantiated to different types,
ranging from the syntax tree type |E| to any possible semantic
interpretations (like |Integer|, |String|, etc.).  In general, for any
given value |x| of type |IntExp a => a|, all the variants of |x|
instanciated at different types are guaranteed to be related by
homomorphisms, because one simply replaces |add|, |mul|, |con| by
valid instances.

The same kind of reasoning justifies the overloading of Haskell
integer literals. They can be given the type |Ring a => a|, and doing
in a mathematically meaningful way, because |Ring a => a| is the
initial algebra for |Ring|.

\subsection{Free Structures}

Another useful kind of are free structures. They are similar to
initial structures, but they also allow to embed an arbitrary set of
\emph{generators} |G|. That is, it is as if we would throw an
additional |generate| function in the algebra:

\begin{code}
class Generate a where
  generate :: G -> a
\end{code}

(We could parameterize the class  over an abstract generator set |g|,
but will refain from it to avoid needless complications.)

\subsubsection{Free Monoid}

As an example, consider the free monoid. Our algebra has the following
signature:
\begin{spec}
generate :: G -> a
op :: a -> a -> a
unit :: a -> a -> a
\end{spec}

As a first version, we can convert each function to a constructor and
obtain the type:
\begin{code}
data FreeMonoid g   =  Unit  |  Op (FreeMonoid g) (FreeMonoid g)  |  Generator g deriving Show
instance Monoid (FreeMonoid g) where
  unit = Unit
  op = Op
\end{code}

Let us consider a fold for the above |FreeMonoid|. We can write its type as follows:
\begin{spec}
evalM :: (Monoid a, Generate a) => (FreeMonoid G -> a)
\end{spec}
but we can also drop the |Generate| constraint and take the |generate|
method as an explicit argument:
\begin{code}
evalM :: Monoid a => (G -> a) -> (FreeMonoid G -> a)
\end{code}
This form is similar to the evaluators of expressions with variables
of type |G|, which we have seen for example in \cref{sec:ArithExp}.
%
Once given a function |f :: G -> a|, the homomorphism condition forces
|evalM| to be a fold:
%
\begin{code}
evalM  f  Unit           =  unit
evalM  f  (Op e1 e2)     =  op (evalM f e1) (evalM f e2)
evalM  f  (Generator x)  =  f x
\end{code}

However, before being completely satisfied, we must note that the
|FreeMonoid| representation is ignoring monoid laws.  By following the
same kind of reasoning as before, we find that we only have in fact
only two distinct forms for the elements of the free monoid:

\begin{itemize}
\item |unit|
\item |generate x1 `op` generate x2 `op` ... `op` generate xn|
\end{itemize}
Because of associativity we have no parentheses in the second form;
and because of the unit laws we need not have |unit| composed with |op|
either.

Thus, the free monoid over a generator set |G| is a list of |G|.

We seemingly also ignored the laws when defining |evalM|. Is this a problem?
For example, is it possible that |e1 `Op` (e2 `Op` e3)| and |(e1 `Op`
e2) `Op` e3| which are by monoid laws equal, map to different values?  By
definition of |evalM|, the condition reduces to checking |evalM f e1
`op` (evalM f e2 `op` evalM f e3) == (evalM f e1 `op` evalM f e2) `op`
evalM f e3|. But then, this turns out to be satisfied if |op| is
associative. In sum, |evalM| will be correct if the target |Monoid|
instance satisfies the laws. This is true in general: folds are always
homomorphisms even if the datatype representation that they work on
ignore laws.

\subsubsection{Functions of one variable as algebras}
\label{sec:OneVarExp-class}
Earlier we have used (many variants of) data types for arithmetic
expressions. Using the free construction, we can easily conceive a
suitable type for any such expression language. For example, the type
for arithmetic expressions with |+,-,*| and variables is the free
|Ring| with the set of variables as generator set.

Let us consider again our deep-embedding for expressions of one
variable \cref{sec:FunExp}. According to our analysis, it should be a
free structure, and because we have only one variable, we can take the
generator set (|G|) to be the unit type.

\begin{code}
type G = ()
instance Generate FunExp where
  generate () = X

instance Additive FunExp where
  (+) = (:+:)
  zero = Const 0

instance Multiplicative FunExp where
  (*) = (:*:)
  one = Const 1
\end{code}
%
and so on for the other numeric classes.

%if False
\begin{code}
instance AddGroup FunExp where negate = Negate
instance MulGroup FunExp where (/) = (:/:)
instance Transcendental FunExp where pi = Const (Prelude.pi); exp = Exp; sin = Sin; cos = Cos
\end{code}
%endif


\begin{exercise}
  Implement |FunExp| instances for |AddGroup|, |MulGroup|, and
  |Transcendental| (possibly extending the datatype).

  Remark: to translate the |Const :: REAL -> FunExp| constructor we
  need a way to map any |REAL| to the above structures.
%
  We know how to do that for integers, (|fromInteger|).
  %
  For this exercise you can restrict yourself to floating point
  representation of constants, and use |recip| (from |MulGroup|) to
  map them to fractions.
\end{exercise}

We can then check that the evaluator is compositional.
For instance, we have
%
\begin{code}
eval (e1 :*: e2)  =  eval e1 * eval e2
eval (Exp e)      =  exp (eval e)
\end{code}
etc.

We can now also generalise the type of evaluator as follows:
\begin{code}
type OneVarExp a = (Generate a, Transcendental a)
eval :: OneVarExp a  =>  FunExp -> a
\end{code}

With this class in place we can define generic expressions using smart
constructors just like in the case of |IntExp| above.
%
For example, we can define
%
\begin{code}
varX :: OneVarExp a => a
varX = generate ()
twoexp :: OneVarExp a => a
twoexp = 2 * exp varX -- recall the implicit |fromInteger|
\end{code}
%
and instantiate |twoexp| to either syntax or semantics:
%
\begin{code}
testFE :: FunExp
testFE = twoexp

testFu :: Func
testFu = twoexp
\end{code}
provided a suitable instance for |Generate Func|:
\begin{code}
instance Generate Func where
  generate () = id
\end{code}

\begin{exercise}
Find another instance of |OneVarExp|.
\jp{What is the intent of the exercise?}
\end{exercise}
As before, we can always define a homomorphism from |FunExp| to \emph{any}
instance of |OneVarExp|, in a unique way, using the fold pattern.
%
This is because the datatype |FunExp| is an initial |OneVarExp|.
Working with |OneVarExp a => a| can be more economical than using |FunExp|:
one does not need any |eval|.

The DSL of expressions, whose syntax is given by the type |FunExp|,
turns out to be almost identical to the DSL defined via type classes
in \cref{sec:typeclasses}\jp{Have we said that we defined a DSL?}.
%
The correspondence between them is given by the |eval| function.
%
The difference between the two implementations is that the first one
separates more cleanly from the semantical one.
%
For example, |:+:| \emph{stands for} a function, while |+| \emph{is}
that function.

\subsection{\extraMaterial A generic Free construction}

We can use the same trick as for initial algebras to construct free
algebras: |(C a, Generate a) => a| is the free |C|-structure. However,
it is often more convenient to pass the embedding function explicitly
rather than via the |Generate| class. In this case, we obtain the type:
|C a => (g -> a) -> a| if |g| is the set of generators.
In modern versions of Haskell, we can even parameterize over the |C| class, and write:
\begin{code}
newtype Free c g = Free (forall a. c a => (g -> a) -> a)
\end{code}
Embedding a generator is then done like so:
\begin{code}
embed :: g -> Free c g
embed g = Free (\generate -> generate g)
\end{code}

Unfortunately the |Free c| type is not automatically an instance of
|c|: we have to implement those manually. Let us see how this plays out for monoid:

\begin{code}
instance Monoid (Free Monoid g)  where
  unit = Free (\_ -> unit)
  Free f `op` Free g = Free (\x -> f x `op` g x)
\end{code}

We can also check the monoid laws for the free monoid. For
example, here is the proof that the right identity law holds:
\begin{spec}
    Free f `op` unit
==  {- def. -}
    Free f `op` Free (\_ -> unit)
==  {- def. -}
    Free (\x -> f x `op` unit)
==  {- law of the underlying monoid -}
    Free (\x -> f x)
==  {- eta-reduction -}
    Free f
  \end{spec}

\begin{exercise}
Prove group laws for |Free AdditiveGroup|.
\end{exercise}


But we can also recover the whole structure which was used to build an
element of this type, for example we could use lists (recall that they are isomorphic to free monoids):
\begin{code}
extract :: Free Monoid g -> [g]
extract (Free f) = f (\g -> [g])
\end{code}

%if False
\begin{code}
instance Monoid [a] where
  unit = []
  op = (++)
\end{code}
%endif
As an example, we can extract the value of the following:
\begin{code}
example :: Free Monoid Int
example = embed 1 `op` embed 10 `op` unit `op` embed 11

-- |>>> extract example|
-- |[1,10,11]|
\end{code}

\begin{exercise}
  Show that |Free Ring ()| is in bijection with |FunExp|.
\end{exercise}


\section{Computing Derivatives, reprise.}

\label{sec:evalD}
%
As discussed in \cref{sec:OneVarExp-class}, it can sometimes be
economical to use the |OneVarExp a => a| representation rather than
the |FunExp| data type. However, in \cref{sec:computingDerivatives} we
argued that the rules for derivatives were naturally operating on a
syntactic representation.

The question is: can we implement |derive| in the shallow embedding?
As a reminder, the reason that the shallow embedding (|ℝ -> ℝ|) works is
that the |eval| function is a \emph{fold}: first evaluate the
sub-expressions of |e|, then put the evaluations together without
reference to the sub-expressions.

Let us now check whether the semantics of derivatives is
compositional.
%
This evaluation function for derivatives is given by composition as below:
%
%{
%format DummyFunc = Func
\begin{code}
type DummyFunc = ℝ -> ℝ
eval'  ::  FunExp -> Func
eval'  =   eval . derive
\end{code}
%}
In a diagram:
\tikzcdset{diagrams={column sep = 2cm, row sep = 2cm}}
\quad%
\begin{tikzcd}
  |FunExp| \arrow[r, "|eval|"] \arrow[d, "|derive|"]
                               \arrow[dr, "|eval'|"]  & |(REAL -> REAL)| \arrow[d, "D"] \\
  |FunExp| \arrow[r, "|eval|"]                        & |(REAL -> REAL)|
\end{tikzcd}

%
Let us consider the |Exp| case:
%
\begin{spec}
     eval' (Exp e)                      =  {- def. |eval'|, function composition -}

     eval (derive (Exp e))              =  {- def. |derive| for |Exp| -}

     eval (Exp e :*: derive e)          =  {- def. |eval| for |:*:| -}

     eval (Exp e) * eval (derive e)     =  {- def. |eval| for |Exp| -}

     exp (eval e) * eval (derive e)     =  {- def. |eval'| -}

     exp (eval e) * eval' e             =  {- let |f = eval e|, |f' = eval' e| -}

     exp f * f'
\end{spec}
Thus, given \emph{only} the derivative |f' = eval' e|, it is
impossible to compute |eval' (Exp e)|.
Another example of the problem is |derive (f :*: g)| where the
result involves not only |derive f| and |derive g|, but also |f| and
|g|.
%
In general, the problem is that some of the rules for computing the derivative
depend not only on the derivative of the subexpressions, but also on
the subexpressions before taking the derivative.

Consequently, |eval'| is in
fact non-compositional (just like |isPrime|).  There is no way to
implement |eval' :: FunExp -> Func| as a fold \emph{ if |Func| is the
  target type}.

One way of expressing this is to say that in order to implement |eval'
:: FunExp -> Func| we need to also compute |eval :: FunExp -> Func|.
%

%
Thus we need to implement a pair of |eval|-functions |(eval, eval')|
together.

In practice, the solution is to extend the return type of |eval'| from one
semantic value |f| of type |Func = REAL -> REAL| to two such values
|(f, f') :: (Func, Func)| where |f' = D f|.
%
That is, we are using the ``tupling transform'': we are computing just
one function |evalD :: FunExp -> (Func, Func)| returning a pair of |f|
and |D f| at once. (At this point, you are advised to look up and solve
Exercise~\ref{exc:tuplingE1} in case you
have not done so already.)
\begin{code}
type FD a = (a -> a, a -> a)

evalD ::  FunExp  ->  FD Double
evalD     e       =   (eval e, eval' e)
\end{code}
%

Is |evalD| compositional?
%
We compute, for example:
%
\begin{spec}
     evalD (Exp e)                           =  {- specification of |evalD| -}

     (eval (Exp e), eval' (Exp e))           =  {- def. |eval| for |Exp| and reusing the computation above -}

     (exp (eval e), exp (eval e) * eval' e)  =  {- introduce names for subexpressions -}

     let  f   = eval e
          f'  = eval' e
     in (exp f, exp f * f')                  =  {- def. |evalD| -}

     let (f, f') = evalD e
     in (exp f, exp f * f')
\end{spec}
%
This semantics \emph{is} compositional and the |Exp| case is as follows:
%
\begin{code}
evalDExp ::  FD Double  ->  FD Double
evalDExp     (f, f')  =   (exp f, exp f * f')
\end{code}

In general, while |eval'| is non-compositional, |evalD| is a more
complex, but compositional, semantics.
%
We can then get |eval'|
back as the second component of |evalD e|:
%
\begin{spec}
eval' :: FunExp -> Func
eval' = snd . evalD
\end{spec}

Because all compositional functions can be expressed as a fold for a
given algebra, we can now define a shallow embedding for the combined
computation of functions and derivatives, using the numerical type
classes.
%
\begin{code}
instance Additive a => Additive (a -> a, a -> a) where  -- same as |Num a => Num (FD a)|
  zero = (const zero, const zero)
  (f, f')  +  (g, g')  =  (f  +  g,  f'      +  g'      )

instance (Additive a, Multiplicative a) => Multiplicative (a -> a, a -> a) where  -- same as |Num a => Num (FD a)|
  one = (const one, const zero)
  (f, f')  *  (g, g')  =  (f  *  g,  f' * g  +  f * g'  )
\end{code}
%
\begin{exercise}
Implement the rest of the |Num| instance for |FD a|.
\end{exercise}

\subsection{Automatic differentiation}
\label{sec:automatic-differentiation}
The simultaneous computation of values and derivatives is an important
technique called ``automatic differentiation''. Automatic
differentiation has grown in importance with the rise of machine
learning, which often uses derivatives (or gradients) to find a values
of parameter which minimizes a user-defined objective
function. However, in such systems, one is often not interested in
computing whole functions and their derivatives (as we have done so
far), but rather a function at a point (say |f x0|) and the derivative
at the same point (say |D f x0|).

The question then arises: is it enough to \emph{only} compute the pair
|(f x0, D f x0)|? In other words, is automatic differentiation
compositional? To answer this question, we must find yet again if
there is a homomorphism between whole functions and their value at a
point.

Fortunately, we have already seen part of the answer in
\cref{ex:apply}.
Namely, the homomorphism is |apply c|, with the definition:
\begin{spec}
apply :: a -> (a -> b) -> b
apply a = \f -> f a
\end{spec}
Because |apply c| is so simple, it is an homomorphism not only for
|Additive|, but also |Ring| (and any numeric class we have seen so
far). We already took advantage of this simple structure to define
homomorphism in the other direction in \cref{sec:FunNumInst}, where we
defined a |Ring| instance for functions with a |Ring| codomain.
\label{sec:apply}

Can we do something similar for |FD|?
The elements of |FD a| are pairs of functions, so we can take
%
\label{sec:applyFD}
%{
%format DummyFD = FD
\begin{code}
type Dup a = (a, a)
type DummyFD a = (a -> a, a -> a)

applyFD ::  a ->  FD a          ->  Dup a
applyFD     c     ((f, f'))  =   (f c, f' c)
\end{code}
%}

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
  one = (one, one)
\end{code}
%
\begin{exercise}
Complete the instance declarations for |Dup REAL|: |Additive|, |AddGroup|, etc.
\end{exercise}


In sum, because this computation goes through also for the other cases we can
actually work with just pairs of values (at an implicit point |c ::
a|) instead of pairs of functions.
%
Thus we can define a variant of |FD a| to be |type Dup a = (a, a)|

%if lectureNotes
Hint: Something very similar can be used for Assignment 2.
%endif


%TODO: Perhaps include the comparison of the |Ring t => Ring (Bool -> t)| instance (as a special case of functions as |Ring|) and the |Ring r => Ring (r,r)| instance from the complex numbers. But it probably takes us too far off course. blackboard/W5/20170213_104559.jpg



\section{Summary}

The following correspondence table summarises the discussion so far:
\begin{center}
\begin{tabular}{ll}
      Computer Science      &   Mathematics
\\\hline
\\    DSL                   &   structure (category, algebra, ...)
\\    deep embedding, abstract syntax        &   initial algebra
\\    shallow embedding     &   any other algebra
\\    semantics             &   homomorphism from the initial algebra
\end{tabular}
\end{center}
\jp{I disagree with this table.

  1. Initial algebras can be constructed without reference to data types (|Class a => a|)

  2. Shallow embeddings can be initial algebras (|Class a => x -> a| is an initial algebra of |Class| + unit generator.)

  3. We saw that homomorphisms are not necessarily judicious semantics.

  4. And isn't a DSL akin to a mathematical domain?
}
The underlying theory of this table is a fascinating topic but mostly
out of scope for this \course{}.
%
%if lectureNotes
See
\href{http://wiki.portal.chalmers.se/cse/pmwiki.php/CTFP14/CoursePlan}{Category
  Theory and Functional Programming} for a whole course around this
(lecture notes are available on
\href{https://github.com/DSLsofMath/ctfp2014}{github}).
%endif

\subsection{Homomorphism as roadmaps}
\label{sec:homomophism-roadmap}
Homomorphisms are key to describe mathematical structure, specify programs, and derive of correct programs.
The relation |h : S1 -> S2| (standing for for ``h is an isomorphism from |S1| to |S2|''), can be used in many ways,
depending on what is known and what is unknown.


\begin{itemize}
\item |? : S1 -> S2|. Given two structures |S1| and |S2|, can we
  derive some function which is a homomorphism between those two
  structures? We asked such a question in \cref{ex:apply} (|apply c :
  Num (x -> a) -> Num a|) and \cref{ex:exponential-as-homomorphism}
  (exponentials).

\item |h : S1 -> S2??|.  What is a structure |S2| compatible with a
  given structure |S1| and given homomorphism |h|?  (eg. we derived
  |applyFD c : FD a -> (a, a)| as a composition of a syntactic
  derivative and an evaluation function.)

\item |? : S1 -> S2??|.  Can we find a good structure on |S2| so that
  it becomes homomorphic with |S1|? This is how we found the structure
  |FD| in |evalD : FunExp -> FD a|.

\item |h : S1?? -> S2|. Given |h| and |S2|, can we find a structure
  |S1| compatible with a given homomorphism |h|?  We will encounter an
  example in \cref{sec:poly} (evaluation function for polynomials.)
\end{itemize}



\subsection{Structures and representations}
One take home message of this chapter is that one should, as a rule,
start with structural definitions first, and consider representation
second.
For example, in \cref{sec:evalD} we defined a |Ring| structure on pairs |(REAL, REAL)| by requiring
the operations to be compatible with the interpretation |(f a, f' a)|.
%
This requirement yields the following definition for multiplication
for pairs:
%
\begin{spec}
(x, x') *? (y, y') = (x * y, x' * y + x * y')
\end{spec}
%
But there is nothing in the ``nature'' of pairs of |REAL| that forces
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
\emph{represented} by a pair of real numbers, provided that we define the
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
in \cref{sec:complexcase}).
%
In general though, it is a good idea to be aware of these
distinctions, even if they are suppressed for reasons of brevity or
style.
%
We will encounter this distinction again in
\cref{sec:polynotpolyfun}.

\section{Beyond Algebras: Co-algebra and the Stream calculus}

In the coming chapters there will be quite a bit of material on
infinite structures.
%
These are often captured not by algebras, but by co-algebras.
%
We will not build up a general theory of co-algebras in this \course{},
but because we will be using infinite streams in the upcoming chapters
we will expose right here their co-algebraic structure.

%include AbstractStream.lhs




%include E4.lhs
