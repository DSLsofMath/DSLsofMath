\section{Week 1}

This lecture is partly based on the paper
\citep{TFPIE15_DSLsofMath_IonescuJansson} from the International
Workshop on Trends in Functional Programming in Education 2015.
%
We will implement certain concepts in the functional programming
language Haskell and
%
the code for this lecture is placed in a module called
|DSLsofMath.L01| that starts here:

\begin{code}
module DSLsofMath.L01 where
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
showCA                ::  ComplexA -> String
showCA (CPlus1 x y i)  =  show x ++ " + " ++ show y ++ "i"
showCA (CPlus2 x i y)  =  show x ++ " + " ++ "i" ++ show y
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
  For example, |3 + 2i|, |div 7 2 - (div 2 3)i|, |i(pi) = 0 + i(pi)|,
  and |-3 = -3 + 0i| are all complex numbers.
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

With this provision, in our notation the examples are written as:

\begin{code}
testC1 :: [ComplexA]
testC1 =  [  CPlus1 3 2 I  ,    CPlus1 (7/2) (-2/3) I
          ,  CPlus2 0 I pi ,    CPlus1 (-3) 0 I
          ]
testS1 = map showCA testC1
\end{code}

We interpret the sentence ``The last of these examples \ldots'' to
mean that there is an embedding of the real numbers in |ComplexA|,
which we introduce explicitly:

\begin{code}
toComplex :: REAL -> ComplexA
toComplex x = CPlus1 x 0 i
\end{code}

Again, at this stage there are many open questions.
%
For example, we can assume that |i1| stands for the complex number
|CPlus2 0 i 1|, but what about |i| by itself?
%
If juxtaposition is meant to denote some sort of multiplication, then
perhaps |1| can be considered as a unit, in which case we would have
that |i| abbreviates |i1| and therefore |CPlus2 0 i 1|.
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
|CPlusC| is not just syntactically injective (as all constructors
are), but also semantically.
%
The equality on complex numbers is what we would obtain in Haskell by
using |deriving Eq|.

This shows that complex numbers are, in fact, isomorphic with pairs of
real numbers, a point which we can make explicit by re-formulating the
definition in terms of a |newtype|:

\begin{code}
type ComplexD = ComplexSem REAL
newtype ComplexSem r = CS (r , r)    deriving Eq
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
re :: ComplexSem r      ->  r
re z @ (CS (x , y))   =   x

im :: ComplexSem r      ->  r
im z @ (CS (x , y))   =   y
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
terms of the concrete datatype |ComplexSem|, for example:

\begin{code}
(+.) :: Num r =>  ComplexSem r -> ComplexSem r -> ComplexSem r
(CS (a , b)) +. (CS (x , y))  =  CS ((a + x) , (b + y))
\end{code}

\noindent
or we can build a datatype of ``syntactic'' complex numbers from the
algebraic operations to arrive at a \emph{deep embedding} as seen in
the next section.

Exercises:
\begin{itemize}
\item implement |(*.)| for |ComplexSem|
\end{itemize}


% ----------------------------------------------------------------

\subsection{A syntax for arithmetical expressions}

So far we have tried to find a datatype to represent the intended
\emph{semantics} of complex numbers.
%
That approach is called ``shallow embedding''.
%
Now we turn to the \emph{syntax} instead (``deep
embedding'').

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

And we can write the evaluator by induction over the syntax tree:

\begin{code}
evalE ImagUnit         = CS (0 , 1)
evalE (ToComplex r)    = CS (r , 0)
evalE (Plus  c1 c2)    = evalE c1   +.  evalE c2
evalE (Times c1 c2)    = evalE c1   *.  evalE c2
\end{code}

We also define a function to embed a semantic complex number in the
syntax:

\begin{code}
fromCS :: ComplexD -> ComplexE
fromCS (CS (x , y)) = Plus (ToComplex x) (Times (ToComplex y) ImagUnit)

testE1 = Plus (ToComplex 3) (Times (ToComplex 2) ImagUnit)
testE2 = Times ImagUnit ImagUnit
\end{code}

There are certain laws we would like to hold for operations on complex
numbers.
%
The simplest is perhaps |square i = -1| from the start of the lecture,

\begin{code}
propImagUnit :: Bool
propImagUnit = Times ImagUnit ImagUnit === ToComplex (-1)

(===) :: ComplexE -> ComplexE -> Bool
z === w  =  evalE z == evalE w
\end{code}

and that |fromCS| is an embedding:

\begin{code}
propFromCS :: ComplexD -> Bool
propFromCS c =  evalE (fromCS c) == c
\end{code}

but we also have that |Plus| and |Times| should be associative and
commutative and |Times| should distribute over |Plus|:

\begin{code}
propAssocPlus  x y z  =  Plus (Plus x y) z === Plus x (Plus y z)
propAssocTimes x y z  =  Times (Times x y) z === Times x (Times y z)
propDistTimesPlus x y z = Times x (Plus y z) === Plus (Times x y) (Times x z)
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

\begin{code}
data ComplexSyn r  =  FromCartesian r r
                   |  ComplexSyn r  :+:  ComplexSyn r
                   |  ComplexSyn r  :*:  ComplexSyn r

toComplexSyn :: Num a => a -> ComplexSyn a
toComplexSyn x = FromCartesian x (fromInteger 0)

evalCSyn :: Num r => ComplexSyn r -> ComplexSem r
evalCSyn (FromCartesian x y) = CS (x , y)
evalCSyn (l :+: r) = evalCSyn l +. evalCSyn r
evalCSyn (l :*: r) = evalCSyn l *. evalCSyn r

instance Num a => Num (ComplexSyn a) where
   (+) = (:+:)
   (*) = (:*:)
   fromInteger = fromIntegerCS
   -- TODO: add a few more operations (hint: extend ComplexSyn as well)
   -- TODO: also extend eval

fromIntegerCS :: Num r =>  Integer -> ComplexSyn r
fromIntegerCS = toComplexSyn . fromInteger
\end{code}

\subsection{TODO[PaJa]: Textify}

Here are some notes about things scribbled on the blackboard during
the first two lectures. At some point this should be made into text
for the lecture notes.

\subsubsection{Pitfalls with traditional mathematical notation}

\paragraph{A function or the value at a point?}

Mathematical texts often talk about ``the function $f(x)$'' when ``the
function $f$'' would be more clear. Otherwise there is a clear risk of
confusion between $f(x)$ as a function and $f(x)$ as the value you get
from applying the function $f$ to the value bound to the name $x$.

\paragraph{Scoping}

Scoping rules for the integral sign:
\begin{align*}
   f(x) &= x^2
\\ g(x) &= \int_{x}^{2x} f(x) dx &= \int_{x}^{2x} f(y) dy
\end{align*}
The variable |x| bound on the left is independent of the variable |x|
``bound under the integral sign''.

\paragraph{From syntax to semantics and back}

We have seen evaluation functions from abstract syntax to semantics
(|eval :: Syn -> Sem|). Often a partial inverse is also available:
|embed :: Sem -> Syn|. For our complex numbers we have TODO: fill in a
function from |ComplexSem r -> ComplexSyn r|.

The embedding should satisfy a round-trip property: |eval (embed s) ==
s| for all |s|.
%
Exercise: What about the opposite direction? When is |embed (eval e)
== e|?

We can also state and check properties relating the semantic and the
syntactic operations:

|a + b = eval (Plus (embed a) (embed b))| for all |a| and |b|.


\paragraph{Variable names as type hints}

In mathematical texts there are often conventions about the names used
for variables of certain types. Typical examples include |i, j, k| for
natural numbers or integers, |x, y| for real numbers and |z, w| for
complex numbers.


The absence of explicit types in mathematical texts can sometimes lead
to confusing formulations.  For example, a standard text on
differential equations by Edwards, Penney and Calvis
\cite{edwards2008elementary} contains at page 266 the following
remark:

\newcommand{\Lap}[1]{\ensuremath{|Lap|\{#1\}}}
\begin{quote}
  The differentiation operator $D$ can be viewed as a transformation
  which, when applied to the function $f(t)$, yields the new function
  $D\{f(t)\} = f'(t)$. The Laplace transformation |Lap| involves the
  operation of integration and yields the new function $\Lap{f(t)} =
  F(s)$ of a new independent variable $s$.
\end{quote}

This is meant to introduce a distinction between ``operators'', such
as differentiation, which take functions to functions of the same
type, and ``transforms'', such as the Laplace transform, which take
functions to functions of a new type.  To the logician or the computer
scientist, the way of phrasing this difference in the quoted text
sounds strange: surely the \emph{name} of the independent variable
does not matter: the Laplace transformation could very well return a
function of the ``old'' variable |t|.  We can understand that the name
of the variable is used to carry semantic meaning about its type (this
is also common in functional programming, for example with the
conventional use of |as| to denote a list of |a|s).  Moreover, by
using this (implicit!) convention, it is easier to deal with cases
such as that of the Hartley transform (a close relative of the Fourier
transform), which does not change the type of the input function, but
rather the \emph{interpretation} of that type.  We prefer to always
give explicit typings rather than relying on syntactical conventions,
and to use type synonyms for the case in which we have different
interpretations of the same type.  In the example of the Laplace
transformation, this leads to


\begin{spec}
type T  =  Real
type S  =  CC
Lap : (T -> CC) -> (S -> CC)
\end{spec}

\subsubsection{Other}

\paragraph{Lifting operations to a parameterised type}
When we define addition on complex numbers (represented as pairs of
real and imaginary components) we can do that for any underlying type
|r| which supports addition.

\begin{code}
type CS = ComplexSem -- for shorter type expressions below
liftPlus ::  (r     -> r     -> r     ) ->
             (CS r  -> CS r  -> CS r  )
liftPlus (+) (CS (x, y)) (CS (x', y')) = CS (x+x', y+y')
\end{code}
Note that |liftPlus| takes |(+)| as its first parameter and uses it
twice on the RHS.

\paragraph{Laws}

TODO: Associative, Commutative, Distributive, ...




\paragraph{TODO[PaJa]: move earlier}

Table of examples of notation and abstract syntax for some complex numbers:

\begin{tabular}{l||l}
    Mathematics & Haskell
\\\hline
    3 + 2i                       & |CPlus1 3 2 i|
\\ 7/2 - 2/3 i = 7/2 + (-2/3) i  & |CPlus1 (7/2) (-2/3) i|
\\ i pi = 0 + i pi               & |CPlus2 0 i pi|
\\ -3 = -3 + 0 i                 & |CPlus1 (-3) 0 i|
\end{tabular}




\subsection{Questions and answers from the exercise sessions week 1}

\subsubsection{Function composition}

The infix operator \verb+.+ in Haskell is an implementation of the
mathematical operation of function composition.

\begin{spec}
f . g = \x -> f (g x)
\end{spec}

The period is an ASCII approximation of the composition symbol $\circ{}$ typically
used in mathematics. (The symbol $\circ{}$ is encoded as \verb"U+2218" and called RING
OPERATOR in Unicode, \verb+&#8728+ in HTML, \verb+\circ+ in \TeX, etc.)

The type is perhaps best illustrated by a diagram with types as nodes
and functions (arrows) as directed edges:

\begin{figure}[htbp]
\centering
\includegraphics[width=0.4\textwidth]{../E/FunComp.jpg}
\caption{Function composition diagram}
\end{figure}

In Haskell we get the following type:

\begin{spec}
(.) :: (b->c) -> (a->b) -> (a->c)
\end{spec}

which may take a while to get used to.

\subsubsection{fromInteger (looks recursive)}

Near the end of the lecture notes there was an instance declaration
including the following lines:

\begin{spec}
instance Num r => Num (ComplexSyn r) where
  -- ... several other methods and then
  fromInteger = toComplexSyn . fromInteger
\end{spec}

This definition looks recursive, but it is not. To see why we need to
expand the type and to do this I will introduce a name for the right
hand side (RHS): |fromIntC|.

\begin{verbatim}
--          ComplexSyn r <---------- r <---------- Integer
fromIntC =              toComplexSyn . fromInteger
\end{verbatim}

I have placed the types in the comment, with ``backwards-pointing''
arrows indicating that |fromInteger :: Integer -> r| and |toComplexSyn
:: r -> ComplexSyn r| while the resulting function is |fromIntC ::
Integer -> ComplexSyn r|. The use of |fromInteger| at type |r| means
that the full type of |fromIntC| must refer to the |Num| class. Thus
we arrive at the full type:

\begin{spec}
fromIntC :: Num r =>   Integer -> ComplexSyn r
\end{spec}

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
readability (if used wisely). The |Env| example shows that a type
synonym can have type parameters.

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

\paragraph{data -- for syntax trees}

Some examples:

\begin{code}
data N = Z | S N
\end{code}

This declaration introduces

\begin{itemize}
\item
  a new type |N| for unary natural numbers,
\item
  a constructor |Z :: N| to represent zero, and
\item
  a constructor |S :: N -> N| to represent the successor.
\end{itemize}

Examples values: |zero = Z|, |one = S Z|, |three = S (S one)|

\begin{code}
data E = V String | P E E | T E E
\end{code}

This declaration introduces

\begin{itemize}
\item
  a new type |E| for simple arithmetic expressions,
\item
  a constructor |V :: String -> E| to represent variables,
\item
  a constructor |P :: E -> E -> E| to represent plus, and
\item
  a constructor |T :: E -> E -> E| to represent times.
\end{itemize}

Example values: |x = V "x"|, |e1 = P x x|, |e2 = T e1 e1|

If you want a contructor to be used as an infix operator you need to use
symbol characters and start with a colon:

\begin{code}
data E' = V' String | E' :+ E' | E' :* E'
\end{code}

Example values: |y = V "y"|, |e1 = y :+ y|, |e2 = x :* e1|

Finally, you can add one or more type parameters to make a whole family
of datatypes in one go:

\begin{code}
data ComplexSy v r  =  Var v
                    |  FromCart r r
                    |  ComplexSy v r  :++  ComplexSy v r
                    |  ComplexSy v r  :**  ComplexSy v r
\end{code}

The purpose of the first parameter |v| here is to enable a free choice
of type for the variables (be it |String| or |Int| or something else)
and the second parameter |r| makes is possible to express ``complex
numbers over'' different base types (like |Double|, |Float|, |Integer|,
etc.).

\subsubsection{|Env|, |Var|, and variable lookup}

The type synonym

\begin{spec}
type Env v s = [(v,s)]
\end{spec}

is one way of expressing a partial function from |v| to |s|.

Example value:

\begin{code}
env1 :: Env String Int
env1 = [("hej", 17), ("du", 38)]
\end{code}

The |Env|type is commonly used in evaluator functions for syntax trees
containing variables:

\begin{code}
evalCP :: Eq v => Env v (ComplexSem r) -> (ComplexSy v r -> ComplexSem r)
evalCP env (Var x) = case lookup x env of
                       Just c -> undefined -- ...
-- ...
\end{code}

Notice that |env| maps ``syntax'' (variable names) to ``semantics'',
just like the evaluator does.

\subsection{Some helper functions}

\begin{code}
propAssocAdd :: (Eq a, Num a) => a -> a -> a -> Bool
propAssocAdd = propAssocA (+)

(*.) :: Num r =>  ComplexSem r -> ComplexSem r -> ComplexSem r
CS (ar, ai) *. CS (br, bi) = CS (ar*br - ai*bi, ar*bi + ai*br)

instance Show r => Show (ComplexSem r) where
  show = showCS

showCS :: Show r => ComplexSem r -> String
showCS (CS (x, y)) = show x ++ " + " ++ show y ++ "i"
\end{code}
