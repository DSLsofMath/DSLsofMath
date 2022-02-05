\section[A DSL of complex numbers]{A DSL of complex numbers}
\label{sec:complexcase}
%TODO: May benefit from more "sectioning" structure (now 5p + just one subsection of 3p)

This section is partly based on material by
\citet{DBLP:journals/corr/IonescuJ16}.
%
and we collect our definitions in a Haskell module which is available
in the GitHub repository of the book.
%
\begin{code}
module DSLsofMath.ComplexSem where
\end{code}
%
These definitions together form a DSL for complex numbers.
%
\index{DSL!complex numbers}%

We now turn to our first study of mathematics as found ``in the
wild'': we will do an analytic reading of a piece of the
introduction of complex numbers by \citet{adams2010calculus}.
%
We choose a simple domain to allow the reader to concentrate on the
essential elements of our approach without the distraction of
potentially unfamiliar mathematical concepts.
%
In fact, for this section, we temporarily pretend to forget any
previous knowledge of complex numbers, and study the textbook as we
would approach a completely new domain, even if that leads to a
somewhat exaggerated attention to detail.

Adams and Essex introduce complex numbers in Appendix A of their book.
%
The section \emph{Definition of Complex Numbers} starts with:

\index{imaginary unit (|i|)}%
\begin{quote}
  We begin by defining the symbol |i|, called \textbf{the imaginary
    unit}, to have the property
%
\begin{spec}
      square i = -1
\end{spec}
%
  Thus, we could also call |i| the square root of |-1| and denote it
  |sqrt (-1)|.
%
  Of course, |i| is not a real number; no real number has a negative
  square.
\end{quote}

%*TODO: Perhaps use this as an example of "specification by equation" pattern which is also used in the lab.
At this stage, it is not clear what the type of |i| is meant to be, we
only know that |i| is not a real number.
%
Moreover, we do not know what operations are possible on |i|, only
that |square i| is another name for |-1| (but it is not obvious that,
say |i * i| is related in any way with |square i|, since the
operations of multiplication and squaring have only been introduced so
far for numerical types such as |Nat| or |REAL|, and not for
``symbols'').

For the moment, we introduce a type for the symbol |i|, and, since we
know nothing about other symbols, we make |i| the only member of this
type.
%
We use a capital |I| in the |data| declaration because a lowercase
constructor name is a syntax error in Haskell, but for
convenience we add also a value |i = I|.
%
\begin{code}
data ImagUnits = I

i :: ImagUnits
i = I
\end{code}
%
We can give the translation from the abstract \addtoindex{syntax} to
the concrete syntax as a function |showIU|:
%
\begin{code}
showIU ::  ImagUnits       ->  String
showIU     I               =   "i"
\end{code}
% \footnote{Here we could also redefine |sqrt| so that |sqrt(-1) = I|. Doing so would force generalising the codomain. Adam and Essex say that we 'can' do it, but it is unclear if we should, at least at this stage.}

Next, in the book, we find the following definition:
%
\begin{quote}
  \textbf{Definition:} A \textbf{complex number} is an expression of
  the form

<  a + bi {-"\qquad \mathrm{or} \qquad"-} a + ib,

  where |a| and |b| are real numbers, and |i| is the imaginary unit.
\end{quote}
%
This definition clearly points to the introduction of a syntax (notice
the keyword ``form'').
%
This is underlined by the presentation of \emph{two} forms, which can
suggest that the operation of juxtaposing |i| (multiplication?) is not
commutative.

A profitable way of dealing with such concrete syntax in functional
programming is to introduce an abstract representation of it in the
form of a datatype:
%
\begin{code}
data ComplexA  =  CPlus1 REAL REAL ImagUnits  -- the form |a + bi|
               |  CPlus2 REAL ImagUnits REAL  -- the form |a + ib|
\end{code}
%
We can give the translation from the (abstract) syntax to its concrete
representation as a string of characters, as the function |showCA|:
%
\begin{code}
showCA ::  ComplexA       ->  String
showCA     (CPlus1 x y i)  =  show x ++ " + " ++ show y ++ showIU i
showCA     (CPlus2 x i y)  =  show x ++ " + " ++ showIU i ++ show y
\end{code}
%
\index{real@@|REAL| (real numbers)}%
%
Notice that the type |REAL| is not implemented yet (and it is not even
clear how to implement it with fidelity to mathematical convention at
this stage) but we want to focus on complex numbers so we will simply
approximate |REAL| by double precision floating point numbers for now.
%
\begin{code}
type REAL = Double
\end{code}
%
\citeauthor{adams2010calculus} continue with examples:
%
\begin{quote}
  For example, $3 + 2i$, $\frac{7}{2} - \frac{2}{3}i$,
  $i\pi = 0 + i\pi$ and $-3 = -3 + 0i$ are all complex numbers.
  %
  The last of these examples shows that every real number can be
  regarded as a complex number.
\end{quote}
%
The second example is somewhat problematic: it does not seem to be of
the form |a + bi|.
%
Given that the last two examples seem to introduce shorthand for
various complex numbers, let us assume that this one does as well, and
that |a - bi| can be understood as an abbreviation of |a + (-b)i|.
%
With this provision, in our Haskell encoding the examples are written
as in \cref{tab:CompleSyntaxExamplesMathHaskell}.
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
%
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
%
We interpret the sentence ``The last of these examples \ldots'' to
mean that there is an embedding of the real numbers in |ComplexA|,
which we introduce explicitly:
%
\begin{code}
toComplex :: REAL -> ComplexA
toComplex x = CPlus1 x 0 I
\end{code}
%
Again, at this stage there are many open questions.
%
For example, we can assume that the mathematical expression $i 1$
stands for the complex number |CPlus2 0 I 1|, but what about the
expression $i$ by itself?
%
If juxtaposition is meant to denote some sort of multiplication, then
perhaps $1$ can be considered as a unit, in which case we would have
that $i$ abbreviates $i 1$ and therefore |CPlus2 0 I 1|.
%
But what about, say, $2 i$?
%
Abbreviations with |i| have only been introduced for the |ib| form,
and not for the |bi| one!

The text then continues with a parenthetical remark which helps us
dispel these doubts:
%
\begin{quote}
  (We will normally use |a + bi| unless |b| is a complicated
  expression, in which case we will write |a + ib| instead.
%
  Either form is acceptable.)
\end{quote}
%
This remark suggests strongly that the two syntactic forms are meant
to denote the same elements, since otherwise it would be strange to
say ``either form is acceptable''.
%
After all, they are acceptable according to the definition provided
earlier.

Given that |a + ib| is only ``syntactic sugar'' for |a + bi|, we can
simplify our representation for the abstract syntax, merging the two
constructors:
%
\begin{code}
data ComplexB = CPlusB REAL REAL ImagUnits
\end{code}
%
In fact, since it doesn't look as though the type |ImagUnits| will
receive more elements, we can dispense with it altogether:
%
\begin{code}
data ComplexC = CPlusC REAL REAL
\end{code}
%
(The renaming of the constructor to |CPlusC| serves as a guard against
the case that we have suppressed potentially semantically relevant syntax.)

We read further:
%
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
%
First, let us notice that we are given an important semantic
information:
%
to check equality for complex numbers, it is enough to check equality
of the components (the arguments to the constructor |CPlusC|).
%
Another way of saying this is that |CPlusC| is \addtoindex{injective}.
%
In Haskell we could define this equality as:
%
\index{instance@@|instance| (keyword)}%
\index{Eq@@|Eq| (type class)}%
%
\begin{code}
instance Eq ComplexC where
    CPlusC a b == CPlusC x y = a == x && b == y
\end{code}
%
The line |instance Eq ComplexC| is there to explain to Haskell that
|ComplexC| supports the |(==)| operator.
%
(The cognoscenti would prefer to obtain an equivalent definition using
the shorter |deriving Eq| clause upon defining the type.)
%*TODO: explain more about deriving - perhaps in a \footnote{}

This shows that the set of complex numbers is, in fact, isomorphic
with the set of pairs of real numbers, a point which we can make
explicit by reformulating the definition in terms of a |newtype|:
%
\begin{code}
newtype ComplexD = CD (REAL, REAL)   deriving Eq
\end{code}
%
As we see it, the somewhat confusing discussion of using ``letters''
to stand for complex numbers serves several purposes.
%
First, it hints at the implicit typing rule that the symbols |z| and
|w| should be complex numbers.
%
Second, it shows that, in mathematical arguments, one needs not
abstract over two real variables: one can instead abstract over a
single complex variable.
%
We already know that we have an \addtoindex{isomorphism} between pair
of reals and complex numbers.
%
But additionally, we have a notion of \emph{\addtoindex{pattern
  matching}}, as in the following definition:
%
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
%
\begin{code}
re :: ComplexD        ->  REAL
re z @ (CD (x , y))   =   x

im :: ComplexD        ->  REAL
im z @ (CD (x , y))   =   y
\end{code}
%
a potential source of confusion being that the symbol |z| introduced
by the as-pattern is not actually used on the right-hand side of the
equations (although it could be).

The use of as-patterns such as ``|z = x + yi|'' is repeated throughout
the text, for example in the definition of the algebraic operations on
complex numbers:
%
\begin{quote}
  \textbf{The sum and difference of complex numbers}

  If |w = a + bi| and |z = x + yi|, where |a|, |b|, |x|, and |y| are real numbers,
  then

< w  +  z  =  (a + x)  +  (b + y)i
<
< w  -  z  =  (a - x)  +  (b - y)i

\end{quote}
%
With the introduction of algebraic operations, the domain-specific
language of complex numbers becomes much richer.
%
We can describe these operations in a \emph{shallow embedding} in
terms of the concrete datatype |ComplexD|, for example:
%
\begin{code}
addD :: ComplexD -> ComplexD -> ComplexD
addD (CD (a , b)) (CD (x , y))  =  CD ((a + x) , (b + y))
\end{code}
%
\noindent
or we can build a datatype of ``syntactic'' complex numbers from the
algebraic operations to arrive at a \emph{deep embedding} as seen in
the next section.
%
Both shallow and deep embeddings will be further explained in
\cref{sec:evalD,sec:expressions-of-one-var} (and in several other
places: this is a recurrent idea of the \course{}).

At this point we can sum up the ``evolution'' of the datatypes
introduced so far.
%
Starting from |ComplexA|, the type has evolved by successive
refinements through |ComplexB|, |ComplexC|, ending up in |ComplexD|
(see Fig.~\ref{fig:ComplexTypeSummary}).
%
We can also make a parameterised version of |ComplexD|, by noting
that the definitions for complex number operations work fine for a
range of underlying numeric types.
%
The operations for |ComplexSem| are defined in module |CSem|,
available in Appendix~\ref{app:CSem}.
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
%*TODO: explain deriving
  \caption{Complex number datatype refinement (semantics).}
  \label{fig:ComplexTypeSummary}
\end{figure}
\index{deriving@@|deriving| (keyword)}%
