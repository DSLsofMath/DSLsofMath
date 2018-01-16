\section{A DSL for arithmetic expressions and complex numbers}
\label{sec:DSLComplex}

This chapter is partly based on the paper
\citep{TFPIE15_DSLsofMath_IonescuJansson} from the International
Workshop on Trends in Functional Programming in Education 2015.
%
We will implement certain concepts in the functional programming
language Haskell and
%
the code for this lecture is placed in a module called
|DSLsofMath.W01| that starts here:

\begin{code}
module DSLsofMath.W01 where
import qualified DSLsofMath.CSem as CSem
import DSLsofMath.CSem (ComplexSem(CS))
import Numeric.Natural (Natural)
import Data.Ratio (Rational, Ratio, (%))
import Data.List(find)
\end{code}

\subsection{Intro: Pitfalls with traditional mathematical notation}

\paragraph{A function or the value at a point?}

Mathematical texts often talk about ``the function $f(x)$'' when ``the
function $f$'' would be more clear.
%
Otherwise there is a risk of confusion between $f(x)$ as a
function and $f(x)$ as the value you get from applying the function
$f$ to the value bound to the name $x$.

Examples: let $f(x) = x + 1$ and let $t = 5*f(2)$.
%
Then it is clear that the value of $t$ is the constant $15$.
%
But if we let $s = 5*f(x)$ it is not clear if $s$ should be seen as a
constant or as a function of $x$.

Paying attention to types and variable scope often helps to sort out
these ambiguities.

\paragraph{Scoping}

The syntax and scoping rules for the integral sign are rarely
explicitly mentioned, but looking at it from a software perspective
can help.
%
If we start from a simple example, like \(\int_{1}^{2} x^2 dx\), it is
relatively clear: the integral sign takes two real numbers as limits
and then a certain notation for a function, or expression, to be
integrated.
%
Comparing the part after the integral sign to the syntax of a function
definition \(f(x) = x^2\) reveals a rather odd rule: instead of
\emph{starting} with declaring the variable \(x\), the integral syntax
\emph{ends} with the variable name, and also uses the letter ``d''.
%
(There are historical explanations for this notation, and it is
motivated by computation rules in the differential calculus, but we
will not go there now.)
%
It seems like the scope of the variable ``bound'' by |d| is from the
integral sign to the final |dx|, but does it also extend to the
limits?
%
The answer is no, as we can see from a slightly extended example:
%
\begin{align*}
   f(x) &= x^2
\\ g(x) &= \int_{x}^{2x} f(x) dx &= \int_{x}^{2x} f(y) dy
\end{align*}
%
The variable |x| bound on the left is independent of the variable |x|
``bound under the integral sign''.
%
Mathematics text books usually avoid the risk of confusion by
(silently) renaming variables when needed, but we believe this
renaming is a sufficiently important operation to be more explicitly
mentioned.

\paragraph{Variable names as type hints}

In mathematical texts there are often conventions about the names used
for variables of certain types.
%
Typical examples include |i, j, k| for natural numbers or integers,
|x, y| for real numbers and |z, w| for complex numbers.


The absence of explicit types in mathematical texts can sometimes lead
to confusing formulations.
%
For example, a standard text on differential equations by
\citet*{edwards2008elementary} contains at page 266 the following
remark:

\newcommand{\Lap}[1]{\ensuremath{|Lap|\{#1\}}}
\begin{quote}
  The differentiation operator $D$ can be viewed as a transformation
  which, when applied to the function $f(t)$, yields the new function
  $D\{f(t)\} = f'(t)$.
  %
  The Laplace transformation |Lap| involves the operation of
  integration and yields the new function $\Lap{f(t)} = F(s)$ of a new
  independent variable $s$.
\end{quote}

This is meant to introduce a distinction between ``operators'', such
as differentiation, which take functions to functions of the same
type, and ``transforms'', such as the Laplace transform, which take
functions to functions of a new type.
%
To the logician or the computer scientist, the way of phrasing this
difference in the quoted text sounds strange:
%
surely the \emph{name} of the independent variable does not matter:
%
the Laplace transformation could very well return a function of the
``old'' variable |t|.
%
We can understand that the name of the variable is used to carry
semantic meaning about its type (this is also common in functional
programming, for example with the conventional use of a plural "s"
suffix, as in the name |xs|, to denote a list of values.).
%
Moreover, by using this (implicit!)\ convention, it is easier to deal
with cases such as that of the Hartley transform (a close relative of
the Fourier transform), which does not change the type of the input
function, but rather the \emph{interpretation} of that type.
%
We prefer to always give explicit typings rather than relying on
syntactical conventions, and to use type synonyms for the case in
which we have different interpretations of the same type.
%
In the example of the Laplace transformation, this leads to

\begin{spec}
type T  =  Real
type S  =  CC
Lap : (T -> CC) -> (S -> CC)
\end{spec}

\subsection{Types of |data|}

Dividing up the world (or problem domain) into values of different
types is one of the guiding principles of this course.
%
We will see that keeping track of types can guide the development of
theories, languages, programs and proofs.
%
To start out we introduce some of the ways types are defined in
Haskell, the language we use for implementation (and often also
specification) of mathematical concepts.

%TODO: perhaps make the following few paragraphs flow better

%TODO: perhaps use more from Expr.lhs

\subsubsection{type / newtype / data}

There are three keywords in Haskell involved in naming types: |type|,
|newtype|, and |data|.

\paragraph{type -- abbreviating type expressions}

The |type| keyword is used to create a type synonym - just another name
for a type expression.

\begin{code}
type Heltal = Integer
type Foo = (Maybe [String], [[Heltal]])
type BinOp = Heltal -> Heltal -> Heltal
type Env v s = [(v,s)]
\end{code}

The new name for the type on the RHS does not add type safety, just
readability (if used wisely).
%
The |Env| example shows that a type synonym can have type parameters.

\paragraph{newtype -- more protection}

A simple example of the use of |newtype| in Haskell is to distinguish
values which should be kept apart. A simple example is

\begin{code}
newtype Age   = Ag Int  -- Age in years
newtype Shoe  = Sh Int  -- Shoe size (EU)
\end{code}

Which introduces two new types, |Age| and |Shoe|, which both are
internally represented by an |Int| but which are good to keep apart.

The constructor functions |Ag :: Int -> Age| and |Sh :: Int -> Shoe| are
used to translate from plain integers to ages and shoe sizes.

In the lecture notes we used a newtype for the semantics of complex
numbers as a pair of numbers in the cartesian representation but may
also be useful to have another newtype for complex as a pair of numbers
in the polar representation.

\paragraph{The keyword |data| -- for syntax trees}

The simplest form of a recursive datatype is the unary notation for
natural numbers:

\begin{code}
data N = Z | S N
\end{code}

This declaration introduces
\begin{itemize}
\item a new type |N| for unary natural numbers,
\item a constructor |Z :: N| to represent zero, and
\item a constructor |S :: N -> N| to represent the successor.
\end{itemize}
Examples values: |zero = Z|, |one = S Z|, |three = S (S one)|

The |data| keyword will be used throughout the course to define
datatypes of syntax trees for different kinds of expressions: simple
arithmetic expresssions, complex number expresssions, etc.
%
But it can also be used for non-recursive datatypes, like |data Bool =
False || True|, or |data Person = P String Age Shoe|.
%
The |Bool| type is the simplest example of a \emph{sum type}, where
each value uses either of the two variants |False| and |True| as the
constructor.
%
The |Person| type is an example of a \emph{product type}, where each
value uses the same constructor |P| and records values for the name,
age, and shoe size of the person modelled.
%
(See exercise \ref{exc:counting} for the intuition behind the terms
``sum'' and ``product'' used here.)

\paragraph{|Maybe| and parameterised types.}
%
It is very often possible describe a family of types of the same ``shape''.
%
One simple example is the type constructor |Maybe|:
%
\begin{spec}
data Maybe a = Nothing | Just a
\end{spec}
%
This declaration introduces
\begin{itemize}
\item a new type |Maybe a| for every type |a|,
\item a constructor |Nothing :: Maybe a| to represent ``no value'', and
\item a constructor |Just :: a -> Maybe a| to represent ``just a value''.
\end{itemize}
%
A maybe type is often used when a function may, or may not, return a
value.

Two other examples of, often used, parameterised types are |(a,b)| for
the type of pairs (a product type) and |Either a b| for either an |a|
or a |b| (a sum type).
%
\begin{spec}
data Either p q = Left p | Right q
\end{spec}


\subsubsection{|Env| and variable |lookup|.}

The type synonym
%
\begin{spec}
type Env v s = [(v,s)]
\end{spec}
%
is one way of expressing a partial function from |v| to |s|.
%
As an example value of this type we can take:
%
\begin{code}
env1 :: Env String Int
env1 = [("hej", 17), ("du", 38)]
\end{code}

We can see the type |Env v s| as a syntactic representation of a
partial function from |v| to |s|.
%
We can convert to a total function |Maybe| returning an |s| using
|evalEnv|:
%
\begin{code}
evalEnv :: Eq v =>  Env v s -> (v -> Maybe s)
evalEnv vss var  =  findFst vss
  where  findFst ((v,s):vss)
           | var == v         =  Just s
           | otherwise        =  findFst vss
         findFst []           =  Nothing
\end{code}
%
Or we can use the Haskell prelude function |lookup = flip evalEnv|:
%
\begin{spec}
lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
\end{spec}
%
We will use |Env| and |lookup| below (in section~\ref{sec:ArithExp})
when we introduce abstract syntax trees containing variables.

\subsection{A syntax for simple arithmetical expressions}
\label{sec:ArithExp}

%TODO: (by DaHe) It would also be a good opportunity to explain deep
% vs shallow embedding, (which I remember some of my classmates had
% trouble grasping when I took the course), as these words are used
% throughout the chapter.

%TODO: The data type could then be expanded to include variables,
% [introduce the type without variables before the currrent type?]
% making possible expressions like 5*x + 7, and how we need to be able
% look up the value of the variable (from an env) in order to eval the
% expression.
%
% This way, the idea of a math DSL would be presented in the most
% basic way possible, before we make it one step more complicated by
% trying to construct a DSL from a book's definition of complex
% numbers.

\begin{code}
data AE = V String | P AE AE | T AE AE
\end{code}

This declaration introduces

\begin{itemize}
\item a new type |AE| for simple arithmetic expressions,
\item a constructor |V :: String -> AE| to represent variables,
\item a constructor |P :: AE -> AE -> AE| to represent plus, and
\item a constructor |T :: AE -> AE -> AE| to represent times.
\end{itemize}

Example values: |x = V "x"|, |e1 = P x x|, |e2 = T e1 e1|

If you want a contructor to be used as an infix operator you need to use
symbol characters and start with a colon:

\begin{spec}
data AE' = V' String | AE' :+ AE' | AE' :* AE'
\end{spec}

Example values: |y = V "y"|, |e1 = y :+ y|, |e2 = x :* e1|

Finally, you can add one or more type parameters to make a whole family
of datatypes in one go:

\begin{code}
data AE' v = V' v | AE' v :+ AE' v | AE' v :* AE' v
\end{code}
%
The purpose of the parameter |v| here is to enable a free choice of
type for the variables (be it |String| or |Int| or something else).

The careful reader will note that the same Haskell module cannot
contain both these definitions of |AE'|.
%
This is because the name of the type and the names of the constructors
are clashing.
%
The typical ways around this are: define the types in different
modules, or rename one of them (often by adding primes as in |AE'|).
%
In this book we often take the liberty of presenting more than one
version of a datatype without changing the names, to avoid multiple
modules or too many primes.


Together with a datatype for the syntax of arithmetic expressions we
also want to define an evaluator of the expressions.
%
The concept of ``an evaluator'', a function from the syntax to the
semantics, is something we will return to many times in this book.
%
We have already seen one example: the function |evalEnv| which
translates from a list of key-value-pairs (the abstract syntax of the
environment) to a function (the semantics).

In the evaluator for |AE' v| we take this one step further: given an
environment |env| and the syntax of an arithmetic expression |e| we
compute the semantics of that expression.
%
\begin{code}
evalAE :: Env String Integer -> (AE -> Maybe Integer)
evalAE env (V x)      =  evalEnv env x
evalAE env (P e1 e2)  =  mayP  (evalAE env e1)  (evalAE env e2)
evalAE env (T e1 e2)  =  mayT  (evalAE env e1)  (evalAE env e2)

mayP :: Maybe Integer -> Maybe Integer -> Maybe Integer
mayP (Just a) (Just b)  =  Just (a+b)
mayP _        _         =  Nothing

mayT :: Maybe Integer -> Maybe Integer -> Maybe Integer
mayT (Just a) (Just b)  =  Just (a*b)
mayT _        _         =  Nothing
\end{code}

\begin{code}
evalAE' :: (Eq v, Num sem) =>  (Env v sem) -> (AE' v -> Maybe sem)
evalAE' env (V' x)      =  evalEnv env x
evalAE' env (e1 :+ e2)  =  liftM (+)   (evalAE' env e1)  (evalAE' env e2)
evalAE' env (e1 :* e2)  =  liftM (*)   (evalAE' env e1)  (evalAE' env e2)

liftM :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
liftM op   (Just a)  (Just b)  =  Just (op a b)
liftM _op  _         _         =  Nothing
\end{code}

\subsection{A case study: complex numbers}

We will start by an analytic reading of the introduction of complex
numbers in \cite{adams2010calculus}.
%
We choose a simple domain to allow the reader to concentrate on the
essential elements of our approach without the distraction of
potentially unfamiliar mathematical concepts.
%
For this section, we bracket our previous knowledge and approach the
text as we would a completely new domain, even if that leads to a
somewhat exaggerated attention to detail.

Adams and Essex introduce complex numbers in Appendix 1.
%
The section \emph{Definition of Complex Numbers} begins with:

\begin{quote}
  We begin by defining the symbol |i|, called \textbf{the imaginary
    unit}, to have the property

<      square i = -1

  Thus, we could also call |i| the square root of |-1| and denote it
  |sqrt (-1)|.
%
  Of course, |i| is not a real number; no real number has a negative
  square.
\end{quote}

At this stage, it is not clear what the type of |i| is meant to be, we
only know that |i| is not a real number.
%
Moreover, we do not know what operations are possible on |i|, only
that |square i| is another name for |-1| (but it is not obvious that,
say |i * i| is related in any way with |square i|, since the
operations of multiplication and squaring have only been introduced so
far for numerical types such as |Nat| or |REAL|, and not for symbols).

For the moment, we introduce a type for the value |i|, and, since we
know nothing about other values, we make |i| the only member of this
type:

\begin{code}
data ImagUnits = I

i :: ImagUnits
i = I
\end{code}

We use a capital |I| in the |data| declaration because a lowercase
constructor name would cause a syntax error in Haskell.

Next, we have the following definition:

\begin{quote}
  \textbf{Definition:} A \textbf{complex number} is an expression of
  the form

<  a + bi {-"\qquad \mathrm{or} \qquad"-} a + ib,

  where |a| and |b| are real numbers, and |i| is the imaginary unit.
\end{quote}

This definition clearly points to the introduction of a syntax (notice
the keyword ``form'').
%
This is underlined by the presentation of \emph{two} forms, which can
suggest that the operation of juxtaposing |i| (multiplication?) is not
commutative.

A profitable way of dealing with such concrete syntax in functional
programming is to introduce an abstract representation of it in the
form of a datatype:

\begin{code}
data ComplexA  =  CPlus1 REAL REAL ImagUnits
               |  CPlus2 REAL ImagUnits REAL
\end{code}

We can give the translation from the abstract syntax to the concrete
syntax as a function |showCA|:

\begin{code}
showCA ::  ComplexA       ->  String
showCA     (CPlus1 x y i)  =  show x ++ " + " ++ show y ++ "i"
showCA     (CPlus2 x i y)  =  show x ++ " + " ++ "i" ++ show y
\end{code}

Notice that the type |REAL| is not implemented yet and it is not
really even exactly implementable but we want to focus on complex
numbers so we will approximate |REAL| by double precision floating
point numbers for now.

\begin{code}
type REAL = Double
\end{code}

The text continues with examples:

\begin{quote}
  For example, $3 + 2i$, $\frac{7}{2} - \frac{2}{3}i$,
  $i\pi = 0 + i\pi$ and $-3 = -3 + 0i$ are all complex numbers.
  %
  The last of these examples shows that every real number can be
  regarded as a complex number.
\end{quote}

The second example is somewhat problematic: it does not seem to be of
the form |a + bi|.
%
Given that the last two examples seem to introduce shorthand for
various complex numbers, let us assume that this one does as well, and
that |a - bi| can be understood as an abbreviation of |a + (-b)i|.
%
With this provision, in our notation the examples are written as in
Table~\ref{tab:CompleSyntaxExamplesMathHaskell}.
%
\begin{table}[tbph]
  \centering
\begin{tabular}{lll}
    \multicolumn{2}{@@{}l@@{}}{Mathematics} & Haskell
\\\hline
    $3 +2i$                        &  & |CPlus1 3 2 I|
\\ $\frac{7}{2} - \frac{2}{3} i$ &=
   $\frac{7}{2} + \frac{-2}{3} i$     & |CPlus1 (7/2) (-2/3) I|
\\ $i \pi$ &= $0 + i \pi$             & |CPlus2 0 I pi|
\\ $-3$ &= $-3 + 0 i$                 & |CPlus1 (-3) 0 I|
\end{tabular}
  \caption{Examples of notation and abstract syntax for some complex numbers.}
  \label{tab:CompleSyntaxExamplesMathHaskell}
\end{table}

%if False
% This is just for testing.
\begin{code}
testC1 :: [ComplexA]
testC1 =  [  CPlus1 3 2 I  ,    CPlus1 (7/2) (-2/3) I
          ,  CPlus2 0 I pi ,    CPlus1 (-3) 0 I
          ]
testS1 = map showCA testC1
\end{code}
%endif

We interpret the sentence ``The last of these examples \ldots'' to
mean that there is an embedding of the real numbers in |ComplexA|,
which we introduce explicitly:

\begin{code}
toComplex :: REAL -> ComplexA
toComplex x = CPlus1 x 0 I
\end{code}

Again, at this stage there are many open questions.
%
For example, we can assume that |i 1| stands for the complex number
|CPlus2 0 I 1|, but what about |i| by itself?
%
If juxtaposition is meant to denote some sort of multiplication, then
perhaps |1| can be considered as a unit, in which case we would have
that |i| abbreviates |i 1| and therefore |CPlus2 0 I 1|.
%
But what about, say, |2i|?
%
Abbreviations with |i| have only been introduced for the |ib| form,
and not for the |bi| one!

The text then continues with a parenthetical remark which helps us
dispel these doubts:

\begin{quote}
  (We will normally use |a + bi| unless |b| is a complicated
  expression, in which case we will write |a + ib| instead.
%
  Either form is acceptable.)
\end{quote}

This remark suggests strongly that the two syntactic forms are meant
to denote the same elements, since otherwise it would be strange to
say ``either form is acceptable''.
%
After all, they are acceptable by definition.

Given that |a + ib| is only ``syntactic sugar'' for |a + bi|, we can
simplify our representation for the abstract syntax, eliminating one
of the constructors:

\begin{code}
data ComplexB = CPlusB REAL REAL ImagUnits
\end{code}

In fact, since it doesn't look as though the type |ImagUnits| will
receive more elements, we can dispense with it altogether:

\begin{code}
data ComplexC = CPlusC REAL REAL
\end{code}

\noindent
(The renaming of the constructor to |CPlusC| serves as a guard against
the case we have suppressed potentially semantically relevant syntax.)

We read further:

\begin{quote}
  It is often convenient to represent a complex number by a single
  letter;
%
  |w| and |z| are frequently used for this purpose.
%
  If |a|, |b|, |x|, and |y| are real numbers, and |w = a + bi| and |z
  = x + yi|, then we can refer to the complex numbers |w| and |z|.
%
  Note that |w = z| if and only if |a = x| and |b = y|.
\end{quote}


First, let us notice that we are given an important semantic
information:
%
to check equality for complex numbers, it is enough to check equality
of the components (the arguments to the constructor |CPlusC|).
%
(Another way of saying this is that |CPlusC| is injective.)
%
The equality on complex numbers is what we would obtain in Haskell by
using |deriving Eq|.

This shows that the set of complex numbers is, in fact, isomorphic
with the set of pairs of real numbers, a point which we can make
explicit by re-formulating the definition in terms of a |newtype|:
%
\begin{code}
newtype ComplexD = CD (REAL, REAL)   deriving Eq
\end{code}

The point of the somewhat confusing discussion of using ``letters'' to
stand for complex numbers is to introduce a substitute for
\emph{pattern matching}, as in the following definition:

\begin{quote}
  \textbf{Definition:} If |z = x + yi| is a complex number (where |x|
  and |y| are real), we call |x| the \textbf{real part} of |z| and
  denote it |Re (z)|.
%
  We call |y| the \textbf{imaginary part} of |z| and denote it |Im
  (z)|:

< Re(z)  =  Re (x + yi)  =  x
< Im(z)  =  Im (x + yi)  =  y

\end{quote}

%
This is rather similar to Haskell's \emph{as-patterns}:

\begin{code}
re :: ComplexD        ->  REAL
re z @ (CD (x , y))   =   x

im :: ComplexD        ->  REAL
im z @ (CD (x , y))   =   y
\end{code}

\noindent
a potential source of confusion being that the symbol |z| introduced
by the as-pattern is not actually used on the right-hand side of the
equations.

The use of as-patterns such as ``|z = x + yi|'' is repeated throughout
the text, for example in the definition of the algebraic operations on
complex numbers:

\begin{quote}
  \textbf{The sum and difference of complex numbers}

  If |w = a + bi| and |z = x + yi|, where |a|, |b|, |x|, and |y| are real numbers,
  then

< w  +  z  =  (a + x)  +  (b + y)i
<
< w  -  z  =  (a - x)  +  (b - y)i

\end{quote}

With the introduction of algebraic operations, the language of complex
numbers becomes much richer.
%
We can describe these operations in a \emph{shallow embedding} in
terms of the concrete datatype |ComplexD|, for example:

\begin{code}
(+.) :: ComplexD -> ComplexD -> ComplexD
(CD (a , b)) +. (CD (x , y))  =  CD ((a + x) , (b + y))
\end{code}

\noindent
or we can build a datatype of ``syntactic'' complex numbers from the
algebraic operations to arrive at a \emph{deep embedding} as seen in
the next section.

Exercises:
\begin{itemize}
\item implement |(*.)| for |ComplexD|
\end{itemize}


At this point we can sum up the ``evolution'' of the datatypes introduced so far.
%
Starting from |ComplexA|, the type has evolved by successive
refinements through |ComplexB|, |ComplexC|, ending up in |ComplexD|
(see Fig.~\ref{fig:ComplexTypeSummary}).
%
(We can also make a parameterised version of |ComplexD|, by noting
that the definitions for complex number operations work fine for a
range of underlying numeric types.
%
The operations for |ComplexSem| are defined in module |CSem|,
available in appendix~\ref{app:CSem}.)

%
\begin{figure}[tbph]
\begin{spec}
data     ImagUnits     =  I
data     ComplexA      =  CPlus1  REAL   REAL ImagUnits
                       |  CPlus2  REAL   ImagUnits REAL
data     ComplexB      =  CPlusB  REAL   REAL ImagUnits
data     ComplexC      =  CPlusC  REAL   REAL
newtype  ComplexD      =  CD  (REAL, REAL)   deriving Eq
newtype  ComplexSem r  =  CS  (r , r)        deriving Eq
\end{spec}
  \caption{Complex number datatype refinement (semantics).}
  \label{fig:ComplexTypeSummary}
\end{figure}

% ----------------------------------------------------------------

\subsection{A syntax for (complex) arithmetical expressions}

So far we have tried to find a datatype to represent the intended
\emph{semantics} of complex numbers.
%
That approach is called ``shallow embedding''.
%
Now we turn to the \emph{syntax} instead (``deep embedding'').

We want a datatype |ComplexE| for the abstract syntax tree of
expressions.
%
The syntactic expressions can later be evaluated to semantic values:

\begin{code}
evalE :: ComplexE -> ComplexD
\end{code}

The datatype |ComplexE| should collect ways of building syntactic
expression representing complex numbers and we have so far seen
%
the symbol |i|, an embedding from |REAL|, plus and times.
%
We make these four \emph{constructors} in one recursive datatype as
follows:

\begin{code}
data ComplexE  =  ImagUnit
               |  ToComplex REAL
               |  Plus   ComplexE  ComplexE
               |  Times  ComplexE  ComplexE
 deriving (Eq, Show)
\end{code}
%
Note that, in |ComplexA| above, we also had a constructor for
``plus'', but it was another ``plus''.
%
They are distinguished by type: |CPlus1| took (basically) two real
numbers, while |Plus| here takes two complex numbers as arguments.

We can implement the evaluator |evalE| by pattern matching on the
syntax tree and recursion.
%
To write a recursive function requires a small leap of faith.
%
It can be difficult to get started implementing a function (like
|eval|) that should handle all the cases and all the levels of a
recursive datatype (like |ComplexE|).
%
One way to overcome this difficulty is through ``wishful thinking'':
assume that all but one case has been implemented already.
%
All you need to focus on is that one remaining case, and you can
freely call the function (that you are implementing) recursively, as
long as you do it for subtrees.
%

For example, when implementing the |evalE (Plus c1 c2)| case, you can
assume that you already know the values |s1, s2 :: ComplexD|
corresponding to the subtrees |c1| and |c2|.
%
The only thing left is to add them up componentwise and we can assume
there is a function |(+.) :: ComplexD -> ComplexD -> ComplexD| taking
care of this step.
%
Continuing in this direction (by ``wishful thinking'') we arrive at
the following implementation.
%
\begin{code}
evalE ImagUnit         = CD (0 , 1)
evalE (ToComplex r)    = CD (r , 0)
evalE (Plus  c1 c2)    = evalE c1   +.  evalE c2
evalE (Times c1 c2)    = evalE c1   *.  evalE c2
\end{code}

We also define a function to embed a semantic complex number in the
syntax:

\begin{code}
fromCD :: ComplexD -> ComplexE
fromCD (CD (x , y)) = Plus (ToComplex x) (Times (ToComplex y) ImagUnit)

testE1 = Plus (ToComplex 3) (Times (ToComplex 2) ImagUnit)
testE2 = Times ImagUnit ImagUnit
\end{code}

This is |testE1| as an abstract syntax tree:
\begin{tikzpicture}[level 1/.style={sibling distance=3cm}]
\node{|Plus|}
child {node {|ToComplex|} child {node {|3|}}}
child {node {|Times|}
  child {node {|ToComplex|} child {node {|2|}}}
  child {node {|ImagUnit|}}};
\end{tikzpicture}

\subsection{Laws, properties and testing}
There are certain laws we would like to hold for operations on complex
numbers.
%
To specify these laws, in a way which can be easily testable in
Haskell, we use functions to |Bool| (also called \emph{predicates} or
\emph{properties}).
%
The intended meaning of such a boolean function is ``forall inputs,
this should return |True|''.
%
This idea is at the core of \emph{property based testing} (pioneered
by \citet{claessen_quickcheck_2000}) and conveniently available in the
library QuickCheck.
%

%
The simplest law is perhaps |square i = -1| from the start of the lecture,
%
\begin{code}
propImagUnit :: Bool
propImagUnit = Times ImagUnit ImagUnit === ToComplex (-1)

(===) :: ComplexE -> ComplexE -> Bool
z === w  =  evalE z == evalE w
\end{code}

and that |fromCD| is an embedding:

\begin{code}
propFromCD :: ComplexD -> Bool
propFromCD c =  evalE (fromCD c) == c
\end{code}

but we also have that |Plus| and |Times| should be associative and
commutative and |Times| should distribute over |Plus|:

\begin{code}
propAssocPlus  x y z     =  Plus (Plus x y) z    ===  Plus x (Plus y z)
propAssocTimes x y z     =  Times (Times x y) z  ===  Times x (Times y z)
propDistTimesPlus x y z  =  Times x (Plus y z)   ===  Plus (Times x y) (Times x z)
\end{code}

These three laws actually fail, but not because of the implementation
of |evalE|.
%
We will get back to that later but let us first generalise the
properties a bit by making the operator a parameter:

\begin{code}
propAssocA :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
propAssocA (+?) x y z =  (x +? y) +? z == x +? (y +? z)
\end{code}

Note that |propAssocA| is a higher order function: it takes a function
(a binary operator) as its first parameter.
%
It is also polymorphic: it works for many different types |a| (all
types which have an |==| operator).

Thus we can specialise it to |Plus|, |Times| and other binary
operators.
%
In Haskell there is a type class |Num| for different types of
``numbers'' (with operations |(+)|, |(*)|, etc.).
%
We can try out |propAssocA| for a few of them.
%

\begin{code}
propAssocAInt    = propAssocA (+) :: Int -> Int -> Int -> Bool
propAssocADouble = propAssocA (+) :: Double -> Double -> Double -> Bool
\end{code}

The first is fine, but the second fails due to rounding errors.
%
QuickCheck can be used to find small examples - I like this one best:

\begin{code}
notAssocEvidence :: (Double , Double , Double , Bool)
notAssocEvidence = (lhs , rhs , lhs-rhs , lhs==rhs)
  where  lhs = (1+1)+1/3
         rhs =  1+(1+1/3)
\end{code}

For completeness: this is the answer:

\begin{spec}
  (  2.3333333333333335     -- Notice the five at the end
  ,  2.333333333333333,     -- which is not present here.
  ,  4.440892098500626e-16  -- The difference
  ,  False)
\end{spec}

This is actually the underlying reason why some of the laws failed for
complex numbers: the approximative nature of |Double|.
%
But to be sure there is no other bug hiding we need to make one more
version of the complex number type: parameterise on the underlying
type for |REAL|.
%
At the same time we generalise |ToComplex| to |FromCartesian|:

%TODO: Add as an exercise the version with I | ToComplex | Plus ... | Times ...
% See data blackboard/W1/20170116_114608.jpg, eval blackboard/W1/20170116_114613.jpg
\label{sec:toComplexSyn}
\label{sec:firstFromInteger}
\begin{code}
data ComplexSyn r  =  FromCartesian r r
                   |  ComplexSyn r  :+:  ComplexSyn r
                   |  ComplexSyn r  :*:  ComplexSyn r

toComplexSyn :: Num a => a -> ComplexSyn a
toComplexSyn x = FromCartesian x 0

-- From |CSem| in appendix~\ref{app:CSem}: |newtype ComplexSem r = CS (r , r)    deriving Eq|

evalCSyn :: Num r => ComplexSyn r -> CSem.ComplexSem r
evalCSyn (FromCartesian x y) = CS (x , y)
evalCSyn (l :+: r)  = evalCSyn l  CSem.+.  evalCSyn r
evalCSyn (l :*: r)  = evalCSyn l  CSem.*.  evalCSyn r

instance Num a => Num (ComplexSyn a) where
   (+)  = (:+:)
   (*)  = (:*:)
   fromInteger = fromIntegerCS
   -- Exercise: add a few more operations (hint: extend |ComplexSyn| as well)
   -- Exercise: also extend |eval|

fromIntegerCS :: Num r =>  Integer -> ComplexSyn r
fromIntegerCS = toComplexSyn . fromInteger
\end{code}

\paragraph{From syntax to semantics and back}

We have seen evaluation functions from abstract syntax to semantics
(|eval :: Syn -> Sem|).
%
Often an inverse is also available: |embed :: Sem -> Syn|.
%
For our complex numbers we have
%
\begin{code}
embed :: CSem.ComplexSem r -> ComplexSyn r
embed (CS (x, y)) = FromCartesian x y
\end{code}

The embedding should satisfy a round-trip property:
%
|eval (embed s) == s| for all semantic complex numbers |s|.
%
Here is a diagram showing how the types and the functions fit together

\begin{tikzcd}
  |ComplexSyn r| \arrow[d, bend left, "|eval|"]  \arrow[loop right, "|embed . eval|"] \\
  |ComplexSem r| \arrow[u, bend left, "|embed|"] \arrow[loop right, "|eval . embed|"]
\end{tikzcd}

%TODO: Perhaps add typed quantification
%
Exercise \ref{exc:embedeval}: What about the opposite direction?
%
When is |embed (eval e) == e|?

%TODO: perhaps include
% We can also state and check properties relating the semantic and the syntactic operations:

% |a + b = eval (Plus (embed a) (embed b))| for all |a| and |b|.

\paragraph{More about laws}

Some laws appear over and over again in different mathematical contexts.
%
Binary operators are often as associative or commutative, and
sometimes one operator distributes over another.
%
We will work more formally with logic in chapter~\ref{sec:logic} but
we introduce a few definitions already here:

|Associative (+) = Forall (a, b, c) ((a+b)+c = a+(b+c))|

|Commutative (+) = Forall (a, b) (a+b = b+a)|

Non-examples: division is not commutative, average is commutative but
not associative.

|Distributive (*) (+) = Forall (a, b, c) ((a+b)*c = (a*c)+(b*c))|

We saw implementations of some of these laws as |propAssocA| and
|propDistTimesPlus| earlier, and learnt that the underlying set
matters: |(+)| for |REAL| has some properties, but |(+)| for |Double|
has other.
%
When implementing, approximation is often necessary, but makes many
laws false.
%
Thus, we should attempt to do it late, and if possible, leave a
parameter to make the degree of approximation tunable (|Int|,
|Integer|, |Float|, |Double|, |QQ|, syntax trees, etc.).

To get a feeling for the distribution law, it can be helpful to study
the syntax trees of the left and right hand sides.
%
Note that |(*c)| is pushed down (distributed) to both |a| and |b|:

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

(In the language of section \ref{sec:AlgHomo}, distributivity means
that |(*c)| is a |(+)|-homomorphism.)

Exercise: Find some operator |(#)| which satisfies |Distributive (+) (#)|
% Answer: |max|

Exercise: Find other pairs of operators satisfying a distributive law.

\subsection{More about functions}

\paragraph{Function composition.}

The infix operator \verb+.+ (period) in Haskell is an implementation
of the mathematical operation of function composition.
%
The period is an ASCII approximation of the composition symbol $\circ{}$ typically
used in mathematics.
%
(The symbol $\circ{}$ is encoded as \verb"U+2218" and called \textsc{ring
operator} in Unicode, \verb+&#8728+ in HTML, \verb+\circ+ in \TeX,
etc.)
%
Its implementation is:
%
\begin{spec}
f . g = \x -> f (g x)
\end{spec}

The type is perhaps best illustrated by a diagram with types as nodes
and functions (arrows) as directed edges:
%


\begin{figure}[htbp]
\hfill
\begin{tikzcd}
  |a| \arrow[d, "|g|"] \arrow[rd, "|f.g|", dashed] &  \\
  |b| \arrow[r, "|f|"]            & |c|
\end{tikzcd}
\hfill
\begin{tikzcd}
  |[Int]| \arrow[d, "|sort|"] \arrow[rd, "|head.sort|", dashed] &  \\
  |[Int]| \arrow[r, "|head|"]            & |Int|
\end{tikzcd}
\hfill
\begin{tikzcd}
  |Env v s| \arrow[d, "|head|"] \arrow[rd, "|fst.head|", dashed] &  \\
  |(v, s)| \arrow[r, "|fst|"]            & |v|
\end{tikzcd}
\hfill{}
%\includegraphics[width=0.4\textwidth]{../E/FunComp.jpg}
\caption{Function composition diagrams: in general, and two examples}
\end{figure}

In Haskell we get the following type:

\begin{spec}
(.) :: (b->c) -> (a->b) -> (a->c)
\end{spec}

which may take a while to get used to.


\subsubsection{Notation and abstract syntax for (infinite) sequences}
\label{sec:infseq}
%TODO: perhaps add as possible reading: http://www.mathcentre.ac.uk/resources/uploaded/mc-ty-convergence-2009-1.pdf
%TODO: perhaps link to https://en.wikipedia.org/wiki/Squeeze_theorem for nice examples
As a bit of preparation for the language of sequences and limits in
later lectures we here spend a few lines on the notation and abstract
syntax of sequences.

Common math book notation: $\left\{ a_i \right\}_{i=0}^{\infty}$ or
just $\left\{ a_i \right\}$ and (not always) an indication of the type
$X$ of the $a_i$.
%
Note that the |a| at the center of this notation actually carries all
of the information: an infinite family of values $a_i : X$.
%
If we interpret ``subscript'' as function application we can see that
|a : Nat -> X| is a useful typing of a sequence.
%
Some examples:
%
\begin{code}
type Nat    =  Natural     -- imported from |Numeric.Natural|
type QQ     =  Ratio Nat   -- imported from |Data.Ratio|
type Seq a  =  Nat -> a

idSeq :: Seq Nat
idSeq i = i                -- |{0, 1, 2, 3, ...}|

invSeq :: Seq QQ
invSeq i = 1%(1 + i)       -- |{frac 1 1, frac 1 2, frac 1 3, frac 1 4, ...}|

pow2 :: Num r =>  Seq r
pow2 = (2^{-"{}"-})        -- |{1, 2, 4, 8, ...}|

conSeq :: a -> Seq a
conSeq c i = c             -- |{c, c, c, c, ...}|
\end{code}

What operations can be performed on sequences?
%
We have seen the first one: given a value |c| we can generate a
constant sequence with |conSeq c|.
%
We can also add sequences componentwise (also called ``pointwise''):
%
\begin{code}
addSeq :: Num a => Seq a -> Seq a -> Seq a
addSeq f g i = f i + g i
\end{code}
and in general lift any binary operation |op :: a -> b -> c| to the
corresponding, pointwise, operation of sequences:
\begin{code}
liftSeq2 :: (a->b->c) -> Seq a -> Seq b -> Seq c
liftSeq2 op f g i = op (f i) (g i)    -- |{op (f 0) (g 0), op (f 1) (g 1), ...}|
\end{code}
Similarly we can lift unary operations, and ``nullary'' operations:
\begin{code}
liftSeq1 :: (a->b) -> Seq a -> Seq b
liftSeq1 h f i = h (f i)              -- |{h (f 0), h (f 1), h (f 2), ...}|

liftSeq0 :: a -> Seq a
liftSeq0 c i = c
\end{code}

Exercice \ref{exc:fmap}: what does function composition do to a sequence?

Another common mathematical operator on sequences is the limit.
%
We will get back to limits in later chapters (\ref{sec:LimPoint}, \ref{sec:FunLimit}), but here we just
analyse the notation and typing.
%
This definition is slightly adapted from Wikipedia (2017-11-08):
\begin{quote}
  We call \(L\) the limit of the sequence |{xn}| if the following
  condition holds: For each real number |ε>0|, there exists
  a natural number |N| such that, for every natural number
  |n >= N|, we have |absBar (xn - L) < ε|.

  If so, we say that the sequence converges to |L| and write
  \[L = \lim_{i\to\infty} x_i\]
\end{quote}
%
There are (at least) two things to note here.
%
First, with this syntax, the $\lim_{i\to\infty} x_i$ expression form
binds |i| in the expression |xi|.
%
We could just as well say that |lim| takes a function |x :: Nat -> X|
as its only argument.
%
Second, an arbitrary |x|, may or may not have a limit.
%
Thus the customary use of |L =| is a bit of abuse of notation, because
the right hand side may not be well defined.
%
One way to capture that is to give |lim| the type |(Nat -> X) -> Maybe
X|.
%
Then \(L = \lim_{i\to\infty} x_i\) would mean |Just L = lim x|
%
We will return to limits and their proofs in
\refSec{par:LimitOfSequence} after we have reviewed some logic.
%
Here we just define one more common operation: the sum of a sequence
(like \(\sigma = \sum_{i=0}^{\infty} 1/i!\)).
%
Just as not all sequences have a limit, not all have a sum either.
%
But for every sequence we can define a new sequence of partial sums:
%
\begin{code}
sums :: Num a => Seq a -> Seq a
sums = scan 0 (+)
scan :: a -> (a->a->a) -> Seq a -> Seq a
scan z (+) f = s
  where  s 0 = z
         s i = s (i-1)  +  f i
\end{code}
%
And by combining this with limits we can state formally that the sum
of a sequence |a| exists and is |S| iff the limit of |sums a| exists
and is |S|.
%
As a formula we get |Just S = lim (sums a)|, and for our example it
turns out that it converges and that
\(\sigma = \sum_{i=0}^{\infty} 1/i! = e\) but we will not get to that
until \refSec{sec:exp}.

We will also return to limits in \refSec{sec:typePartialDerivative}
about derivatives where we explore variants of the classical
definition
%
\[f'(x) = \lim_{h\to0} \frac{f(x+h)-f(x)}{h}\]

To sum up this subsection, we have defined a small Domain Specific
Language (DSL) for infinite sequences by defining a type (|Seq a|),
some operations (|conSeq|, |addSeq|, |fmap|, |sums|, \ldots) and some
``run functions'' or predicates (like |lim| and |sum|).

% ----------------------------------------------------------------

%if False
\subsection{Some helper functions (can be skipped)}

\begin{code}
propAssocAdd :: (Eq a, Num a) => a -> a -> a -> Bool
propAssocAdd = propAssocA (+)

(*.) :: ComplexD -> ComplexD -> ComplexD
CD (ar, ai) *. CD (br, bi) = CD (ar*br - ai*bi, ar*bi + ai*br)

instance Show ComplexD where
  show = showCD

showCD :: ComplexD -> String
showCD (CD (x, y)) = show x ++ " + " ++ show y ++ "i"
\end{code}
%endif

%include E1.lhs
