\section{Compositional Semantics and Algebraic Structures}
\label{sec:CompSem}
\begin{code}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module DSLsofMath.W04 where
import Prelude hiding (Monoid)
\end{code}

\subsection{Compositional semantics}
% (Based on ../../2016/Lectures/Lecture06  )

\subsubsection{A simpler example of a non-compositional function}

Consider a very simple datatype of integer expressions:
%
\begin{code}
data E = Add E E | Mul E E | Con Integer deriving Eq
e1, e2 :: E                             -- | 1 + 2 * 3 |
e1 = Add (Con 1) (Mul (Con 2) (Con 3))  -- | 1 +(2 * 3)|
e2 = Mul (Add (Con 1) (Con 2)) (Con 3)  -- |(1 + 2)* 3 |
\end{code}
%
\begin{tikzpicture}[AbsSyn]
\node{|Add|}
child {node {|Con|} child {node {|1|}}}
child {node {|Mul|}
  child {node {|Con|} child {node {|2|}}}
  child {node {|Con|} child {node {|3|}}}};
\end{tikzpicture}
\begin{tikzpicture}[AbsSyn]
\node{|Mul|}
child {node {|Add|}
  child {node {|Con|} child {node {|1|}}}
  child {node {|Con|} child {node {|2|}}}}
child {node {|Con|} child {node {|3|}}};
\end{tikzpicture}


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
Now, if we try to implement the semantic constructors without thinking
too much we would get the following:
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
Note that both |e1| and |e2| are different but they pretty-print to
the same string.
%
There are many ways to fix this, some more ``pretty'' than others, but
the main problem is that some information is lost in the translation.
%

TODO(perhaps): Explain using three pretty printers for the three
``contexts'': at top level, inside |Add|, inside |Mul|, ... then
combine them with the tupling transform just as with |evalD|.
%
The result is the following:
%
\begin{code}
prTop :: E -> String
prTop e =  let (pTop, _, _) = prVersions e
           in pTop

prVersions = foldE prVerAdd prVerMul prVerCon

prVerAdd (xTop, xInA, xInM) (yTop, yInA, yInM) =
  let s = xInA ++ "+" ++ yInA  -- use |InA| because we are ``in |Add|''
  in (s, paren s, paren s)     -- parens needed except at top level

prVerMul (xTop, xInA, xInM) (yTop, yInA, yInM) =
  let s = xInM ++ "*" ++ yInM  -- use |InM| because we are ``in |Mul|''
  in (s, s, paren s)           -- parens only needed inside |Mul|

prVerCon i =
  let s = show i
  in (s, s, s)                 -- parens never needed

paren :: String -> String
paren s = "(" ++ s ++ ")"
\end{code}

Exercise: Another way to make this example go through is to refine the
semantic domain from |String| to |Precedence -> String|.
%
%TODO (by DaHe): The term "tupling transform" has not been defined! Perhaps should explain what
% it means, when we introduce FD in the prev chapter. (If I'm correct in
% assuming that is also a tupling transform)
%
This can be seen as another variant of the result after the tupling
transform: if |Precedence| is an |n|-element type then |Precedence ->
String| can be seen as an |n|-tuple.
%
In our case a three-element |Precedence| would be enough.

\subsubsection{Compositional semantics in general}

In general, for a syntax |Syn|, and a possible semantics (a type |Sem|
and an |eval| function of type |Syn -> Sem|), we call the semantics
\emph{compositional} if we can implement |eval| as a fold.
%
Informally a ``fold'' is a recursive function which replaces each
abstract syntax constructor |Ci| of |Syn| with a ``semantic
constructor'' |ci|.
%
Thus, in our datatype |E|, a compositional semantics means that |Add|
maps to |add|, |Mul {-"\mapsto"-} mul|, and |Con {-"\mapsto"-} con|
for some ``semantic functions'' |add|, |mul|, and |con|.
%

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
  add  ::  t -> t -> t
  mul  ::  t -> t -> t
  con  ::  Integer -> t
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

And we can also see |pretty| as instance:

\begin{code}
instance IntExp String where
  add = prettyAdd
  mul = prettyMul
  con = prettyCon

pretty' :: E -> String
pretty' = foldIE
\end{code}

%TODO (by DaHe): Maybe we should show how to do this with FunExp (here or in
% 4.2.2.), and how we can build a FunExp out of the semantic constructors if we
% have instantiated the type classes: then evaluate the expression to either the
% syntactic or semantic type by typecasting (exp :: FunExp = F, exp :: (t ->
% Double) = f) or applying the types to id or Id respectively.
%
% Alternatively, we could just show this for E, something like
% add (con 3) (con 4) :: Integer = 7
% add (con 3) (con 4) :: E = Add (Con 3) (Con 4)
%
% And leave it as an exercise to play around with the same thing using FunExp
%
\subsubsection{Back to derivatives and evaluation}

TODO: perhaps not include this here. The background is that this material did not quite fit in the previous lecture. Also some repition was needed.

Review section \ref{sec:evalD} again with the definition of |eval'|
being non-compositional (just like |pretty|) and |evalD| a more
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

As we saw in section \ref{sec:evalD} this does not work.
%
Instead we need to use the ``tupling transform'' to compute a pair of
|f| and |D f| at once.

%TODO: Perhaps make a diagram for this case as well.

% Compositionality:
% * simpler example of non-compositional function [See L9]
% * then back to |eval'|
% * and show |evalD|
% * general pattern: tupling
%

\subsection{Algebraic Structures and DSLs}

% based on ../../2016/Lectures/Lecture09.lhs

In this lecture, we continue exploring the relationship between type
classes, mathematical structures, and DSLs.

\subsubsection{Algebras, homomorphisms}
\label{sec:AlgHomo}

TODO: rewrite to make it smoother

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

A monoid is an algebra which has an associative operation 'op' and a
unit.
%
The laws can be formulated as the following equations:
%
\begin{spec}
∀ x : a? (unit `op` x == x  ∧  x `op` unit == x)
∀ x, y, z : a? (x `op` (y `op` z) == (x `op` y) `op` z)
\end{spec}

\end{quote}

Examples of monoids include numbers with additions, |(REAL, 0, (+))|,
numbers with multiplication |(RPos, 1, (*))|, and even endofunctions
with composition |(a->a,id, (.))|.
%
It is a good exercise to check that the laws are satisfied.

In mathematics, as soon as there are several examples of a structure,
the question of what ``translation'' between them comes up.
%
An important class of such ``translations'' are ``structure preserving
maps'' called \emph{homomorphisms}.
%
As two examples, we have the homomorphisms |log| and |exp|:

\begin{spec}
  exp  :  REAL  ->  RPos
  exp  0        =   1
  exp  (a + b)  =   exp a  *  exp b

  log  :  RPos  ->  REAL
  log  1        =   0
  log  (a * b)  =   log a  +  log b
\end{spec}

\begin{quote}

  More formally, a homomorphism between two algebras |A| and |B| is a
function |h : A → B| from the set |A| to the set |B| such that, for
every operation |fA| of |A| and corresponding |fB| of |B| (of arity,
say, |n|), |h(fA(x1,...,xn)) = fB(h(x1),...,h(xn))|.

\end{quote}

Example: Monoid homomorphism

\begin{spec}
h unit        =  unit             -- |h| takes units to units
h (x `op` y)  =  h x `op` h y     -- and distributes over |op|
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

%TODO (by DaHe): Where happens to the constructors A and M go in the solution
% below? Perhaps provide both the "math" and "Haskell" versions to compare.
%
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

Exercise: show that |const| is a homomorphism.
%
The distribution law can be shown as follows:
%
\begin{spec}
  h a + h b                     =  {- |h = const| in this case -}
  const a  +  const b           =  {- By def. of |(+)| on functions -}
  (\x-> const a x + const b x)  =  {- By def. of |const|, twice -}
  (\x->  a + b )                =  {- By def. of |const| -}
  const (a + b)                 =  {- |h = const| -}
  h (a + b)
\end{spec}

We now have a homomorphism from values to functions, and you may
wonder if there is a homomorphism in the other direction.
%
The answer is ``Yes, many''.
%
Exercise: Show that |apply c| is a homomorphism for all |c|, where
|apply x f = f x|.

\subsubsection{Homomorphism and compositional semantics}

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
%
\begin{code}
class GoodClass t where
  addF :: t -> t -> t
  mulF :: t -> t -> t
  expF :: t -> t
  -- ... Exercise: continue to mimic the |FunExp| datatype as a class

newtype FD a = FD (a -> a, a -> a)

instance Num a => GoodClass (FD a) where
  addF = evalDApp
  mulF = evalDMul
  expF = evalDExp
  -- ... Exercise: fill in the rest

evalDApp = error "Exercise"
evalDMul = error "Exercise"
evalDExp = error "Exercise"
\end{code}
%


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
  Theory and Functional Programming} for a whole course around this
(lecture notes are available on
\href{https://github.com/DSLsofMath/ctfp2014}{github}).

\subsubsection{Other homomorphisms}

Last time, we defined a |Num| instance for functions with a |Num|
codomain.
%
If we have an element of the domain of such a function, we can use it
to obtain a homomorphism from functions to their codomains:

\begin{spec}
Num a => x ->  (x -> a) -> a
\end{spec}
%
As suggested by the type, the homomorphism is just function
application:

\begin{spec}
apply :: a -> (a -> b) -> b
apply a = \f -> f a
\end{spec}
\label{sec:apply}

Indeed, writing |h = apply c| for some fixed |c|, we have

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

\begin{spec}
applyFD ::  a ->  FD a     ->  (a, a)
applyFD     c     (f, f')  =   (f c, f' c)
\end{spec}

We now have the domain of the homomorphism |(FD a)| and the
homomorphism itself |(applyFD c)|, but we are missing the structure on
the codomain, which now consists of pairs |(a, a)|.
%
In fact, we can \emph{compute} this structure from the homomorphism
condition.
%
For example:

\begin{spec}
     h ((f, f') * (g, g'))                       =  {- def. |*| for |FD a| -}

     h (f * g, f' * g + f * g')                  =  {- def. |h = applyFD c| -}

     ((f * g) c, (f' * g + f * g') c)            =  {- def. |*| and |+| for functions -}

     (f c * g c, f' c * g c + f c * g' c)        =  {- homomorphism condition from step 1 -}

     h (f, f') *? h (g, g')                      =  {- def. |h = applyFD c| -}

     (f c, f' c) *? (g c, g' c)
\end{spec}

The identity will hold if we take

\begin{spec}
     (x, x') *? (y, y') = (x * y, x' * y + x * y')
\end{spec}
%TODO: state explicitly that |(applyFD c)| is a |Num|-homomorphism for all |c|
%
%TODO (by DaHe): It should probably be mentioned that we have now defined
% instance Num a => FD a where (x, x') (*) (y, y') = ..., and that the exercise
% below asks to define the rest of the operations on FD (I assume that's what
% "the instance declarations" refers to below, it is not entirely clear to me)
%%

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

\subsection{Summing up: definitions and representation}

We defined a |Num| structure on pairs |(Double, Double)| by requiring
the operations to be compatible with the interpretation |(f a, f' a)|.
%
For example

\begin{spec}
(x, x') *? (y, y') = (x * y, x' * y + x * y')
\end{spec}

There is nothing in the ``nature'' of pairs of |Double| that forces
this definition upon us.
%
We chose it, because of the intended interpretation.

This multiplication is obviously not the one we need for \emph{complex
  numbers}:

\begin{spec}
(x, x') *. (y, y') = (x * y - x' * y', x * y' + x' * y)
\end{spec}

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
We will see this distinction again in section
\ref{sec:polynotpolyfun}.


\subsubsection{Some helper functions}

\begin{code}
instance Num E where -- Some abuse of notation (no proper |negate|, etc.)
  (+)  = Add
  (*)  = Mul
  fromInteger = Con
  negate = negateE

negateE (Con c) = Con (negate c)
negateE _ = error "negate: not supported"
\end{code}

%TODO: Perhaps include the comparison of the |Num t => Num (Bool -> t)| instance (as a special case of functions as |Num|) and the |Num r => Num (r,r)| instance from the complex numbers. But it probably takes us too far off course. blackboard/W5/20170213_104559.jpg
\subsection{Exercises}

%include E4.lhs
