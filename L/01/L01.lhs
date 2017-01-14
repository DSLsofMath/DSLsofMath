\section{Lecture 01}

This lecture is partly based on the paper
\citep{TFPIE15_DSLsofMath_IonescuJansson} from the International
Workshop on Trends in Functional Programming in Education 2015.
%
We will implement certain concepts in the functional programming
language Haskell and
%
the code for this lecture is placed in a module called
|DSLsofMath.L01| that starts here:

> module DSLsofMath.L01 where

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

> data ImagUnits = I

> i :: ImagUnits
> i = I

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

> data ComplexA  =  CPlus1 REAL REAL ImagUnits
>                |  CPlus2 REAL ImagUnits REAL

We can give the translation from the abstract syntax to the concrete
syntax as a function |showCA|:

> showCA               ::  ComplexA -> String
> showCA (CPlus1 x y i)  =  show x ++ " + " ++ show y ++ "i"
> showCA (CPlus2 x i y)  =  show x ++ " + " ++ "i" ++ show y

Notice that the type |REAL| is not implemented yet and it is not
really even exactly implementable but we want to focus on complex
numbers so we will approximate |REAL| by double precision floating
point numbers for now.

> type REAL = Double

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

> testC1 :: [ComplexA]
> testC1 =  [  CPlus1 3 2 I  ,    CPlus1 (7/2) (-2/3) I
>           ,  CPlus2 0 I pi ,    CPlus1 (-3) 0 I
>           ]
> testS1 = map showCA testC1

We interpret the sentence ``The last of these examples \ldots'' to
mean that there is an embedding of the real numbers in |ComplexA|,
which we introduce explicitly:

> toComplex :: REAL -> ComplexA
> toComplex x = CPlus1 x 0 i

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

> data ComplexB = CPlusB REAL REAL ImagUnits

In fact, since it doesn't look as though the type |ImagUnits| will
receive more elements, we can dispense with it altogether:

> data ComplexC = CPlusC REAL REAL

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

> type ComplexD = ComplexS REAL
> newtype ComplexS r = CS (r , r)    deriving Eq

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

> re :: ComplexS r      ->  r
> re z @ (CS (x , y))   =   x

> im :: ComplexS r      ->  r
> im z @ (CS (x , y))   =   y

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
terms of the concrete datatype |ComplexS|, for example:

> (+.) :: Num r =>  ComplexS r -> ComplexS r -> ComplexS r
> (CS (a , b)) +. (CS (x , y))  =  CS ((a + x) , (b + y))

\noindent
or we can build a datatype of ``syntactic'' complex numbers from the
algebraic operations to arrive at a \emph{deep embedding} as seen in
the next section.

Exercises:
\begin{itemize}
\item implement |(*.)| for |ComplexS|
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

> evalE :: ComplexE -> ComplexD

The datatype |ComplexE| should collect ways of building syntactic
expression representing complex numbers and we have so far seen
%
the symbol |i|, an embedding from |REAL|, plus and times.
%
We make these four \emph{constructors} in one recursive datatype as
follows:

> data ComplexE  =  ImagUnit
>                |  ToComplex REAL
>                |  Plus   ComplexE  ComplexE
>                |  Times  ComplexE  ComplexE
>  deriving (Eq, Show)

And we can write the evaluator by induction over the syntax tree:

> evalE ImagUnit         = CS (0 , 1)
> evalE (ToComplex r)    = CS (r , 0)
> evalE (Plus  c1 c2)    = evalE c1   +.  evalE c2
> evalE (Times c1 c2)    = evalE c1   *.  evalE c2

We also define a function to embed a semantic complex number in the
syntax:

> fromCS :: ComplexD -> ComplexE
> fromCS (CS (x , y)) = Plus (ToComplex x) (Times (ToComplex y) ImagUnit)

> testE1 = Plus (ToComplex 3) (Times (ToComplex 2) ImagUnit)
> testE2 = Times ImagUnit ImagUnit

There are certain laws we would like to hold for operations on complex
numbers.
%
The simplest is perhaps |square i = -1| from the start of the lecture,

> propImagUnit :: Bool
> propImagUnit = Times ImagUnit ImagUnit === ToComplex (-1)
>
> (===) :: ComplexE -> ComplexE -> Bool
> z === w  =  evalE z == evalE w

and that |fromCS| is an embedding:

> propFromCS :: ComplexD -> Bool
> propFromCS c =  evalE (fromCS c) == c

but we also have that |Plus| and |Times| should be associative and
commutative and |Times| should distribute over |Plus|:

> propAssocPlus  x y z  =  Plus (Plus x y) z === Plus x (Plus y z)
> propAssocTimes x y z  =  Times (Times x y) z === Times x (Times y z)
> propDistTimesPlus x y z = Times x (Plus y z) === Plus (Times x y) (Times x z)

These three laws actually fail, but not because of the implementation
of |evalE|.
%
We will get back to that later but let us first generalise the
properties a bit by making the operator a parameter:

> propAssocA :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
> propAssocA (+?) x y z =  (x +? y) +? z == x +? (y +? z)

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

> propAssocAInt    = propAssocA (+) :: Int -> Int -> Int -> Bool
> propAssocADouble = propAssocA (+) :: Double -> Double -> Double -> Bool

The first is fine, but the second fails due to rounding errors.
%
QuickCheck can be used to find small examples - I like this one best:

> notAssocEvidence :: (Double , Double , Double , Bool)
> notAssocEvidence = (lhs , rhs , lhs-rhs , lhs==rhs)
>   where  lhs = (1+1)+1/3
>          rhs =  1+(1+1/3)

For completeness: this is the answer:

<   (  2.3333333333333335     -- Notice the five at the end
<   ,  2.333333333333333,     -- which is not present here.
<   ,  4.440892098500626e-16  -- The difference
<   ,  False)

This is actually the underlying reason why some of the laws failed for
complex numbers: the approximative nature of |Double|.
%
But to be sure there is no other bug hiding we need to make one more
version of the complex number type: parameterise on the underlying
type for |REAL|.
%
At the same time we generalise |ToComplex| to |FromCartesian|:

> data ComplexP r  =  FromCartesian r r
>                  |  ComplexP r  :+:  ComplexP r
>                  |  ComplexP r  :*:  ComplexP r

> toComplexP :: Num a => a -> ComplexP a
> toComplexP x = FromCartesian x (fromInteger 0)

> evalCP :: Num r => ComplexP r -> ComplexS r
> evalCP (FromCartesian x y) = CS (x , y)
> evalCP (l :+: r) = evalCP l +. evalCP r
> evalCP (l :*: r) = evalCP l *. evalCP r

> instance Num a => Num (ComplexP a) where
>    (+) = (:+:)
>    (*) = (:*:)
>    fromInteger = toComplexP . fromInteger
>    -- TODO: add a few more operations


----------------------------------------------------------------

\subsection{Some helper functions}

> propAssocAdd :: (Eq a, Num a) => a -> a -> a -> Bool
> propAssocAdd = propAssocA (+)

> (*.) :: Num r =>  ComplexS r -> ComplexS r -> ComplexS r
> CS (ar, ai) *. CS (br, bi) = CS (ar*br - ai*bi, ar*bi + ai*br)

> instance Show r => Show (ComplexS r) where
>   show = showCS

> showCS :: Show r => ComplexS r -> String
> showCS (CS (x, y)) = show x ++ " + " ++ show y ++ "i"
