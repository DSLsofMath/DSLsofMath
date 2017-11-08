\section{Week 1: a DSL for arithmetic expressions and complex numbers}

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
definition \(f(x) = x^2\) reveals the a rather odd rule: instead of
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
For example, a standard text on differential equations by Edwards,
Penney and Calvis \cite{edwards2008elementary} contains at page 266
the following remark:

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
programming, for example with the conventional use of |as| to denote a
list of |a|s).
%
Moreover, by using this (implicit!) convention, it is easier to deal
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
To start out we introduce some of the ways types are described in
Haskell.

TODO: make the following few paragraphs flow better

TODO: Introduce: newtype, products, sums, recursion, parameters

%TODO: (by DaHe) Some that appear in the Q&A of this week are:
% * newtype vs type vs data, which people might have forgotten since the Haskell course.
% * how syntax trees are defined using data
% * Env, Var and variable lookup.
%
% A way of clarifying these might be to have a breif recap at the start of this chapter, so that
% students' Haskell chops are up to speed before we start using it to describe maths. For instance, we could
% present a data type that represents basic arithmetic expressions with real numbers. This would give the opportunity
% to explain type synonyms (type R = Double) and using data to define syntax trees (data Expr = Add Expr Expr | Mul Expr Expr ...).
% It would also be a good opportunity to explain syntax vs semantics, deep vs shallow embedding, (which I remember
% some of my classmates had trouble grasping when I took the course), as these words are used throughout the chapter.
% The data type could then be expanded to include variables, making possible expressions like 5*x + 7, and how we need
% to be able look up the value of the variable (from an env) in order to eval the expression.
%
% This way, the idea of a math DSL would be presented in the most basic way possible, before we make it one step more complicated
% by trying to construct a DSL from a book's definition of complex numbers.


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

The |Env| type is commonly used in evaluator functions for syntax trees
containing variables:

\begin{code}
evalCP :: Eq v => Env v (ComplexSem r) -> (ComplexSy v r -> ComplexSem r)
evalCP env (Var x) = case lookup x env of
                       Just c -> undefined -- ...
-- ...
\end{code}

Notice that |env| maps ``syntax'' (variable names) to ``semantics'',
just like the evaluator does.


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
For example, we can assume that |i 1| stands for the complex number
|CPlus2 0 i 1|, but what about |i| by itself?
%
If juxtaposition is meant to denote some sort of multiplication, then
perhaps |1| can be considered as a unit, in which case we would have
that |i| abbreviates |i 1| and therefore |CPlus2 0 i 1|.
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
%TODO: (by DaHe) There's a few expressions in the sentence below that students might find confusing:
% syntactically/semantically injective, isomorphic. Is it possible to explain in a more basic way?
% Perhaps if we did as I suggested in the TODO at the start of this chapter, the words 'syntax' and 'semantics'
% could be defined and clarified with some examples, since I remember several people having trouble with these concepts.
%
|CPlusC| is not just syntactically injective (as all constructors
are), but also semantically.
%
The equality on complex numbers is what we would obtain in Haskell by
using |deriving Eq|.

This shows that complex numbers are, in fact, isomorphic with pairs of
real numbers, a point which we can make explicit by re-formulating the
definition in terms of a |newtype|:
%
%TODO: (by DaHe) Is it really necessary to parametrize the type this early? I feel like it might
% just add confusion at this point. (i.e. why not just newtype ComplexD = CS(REAL, REAL) ?).
% The type ComplexSyn r gets parametrized later on, with some motivation. Can't we wait to
% parametrize the semantic type until that point?
%
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


TODO: Sum up the datatype ``evolution'' (refinement) so far: |ComplexA|, |ComplexB|, |ComplexC|, |ComplexD|, |ComplexSem|.
%**TODO: (cmp. blackboard/W1/20170116_114631.jpg)

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
%TODO: also draw them as syntax trees - see blackboard/W1/20170116_114619.jpg

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

% TODO: Add as an exercise the version with I | ToComplex | Plus ... | Times ...
% See data blackboard/W1/20170116_114608.jpg, eval blackboard/W1/20170116_114613.jpg
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

TODO: place this paragraph properly.
\paragraph{From syntax to semantics and back}

We have seen evaluation functions from abstract syntax to semantics
(|eval :: Syn -> Sem|).
%
Often a partial inverse is also available: |embed :: Sem -> Syn|.
%
For our complex numbers we have
%
TODO: fill in a function from |ComplexSem r -> ComplexSyn r|.
%
(Roughly |embed (CS (x, y)) = Plus (ToC x) (Times I (ToC y))|.)
%
TODO: draw diagram of the types and the functions |eval| and |embed|
to give an intuition for the ``round-trip'' property

The embedding should satisfy a round-trip property:
%
|eval (embed s) == s| for all |s|.
%
TODO: Add typed quantification
%
Exercise: What about the opposite direction?
%
When is |embed (eval e) == e|?
%
(Step 0: type the quantification.
%
Step 1: what equality is suitable here?
%
Step 2: if you use ``equality up to eval'' - how is the resulting
property related to the first round-trip property?
%See blackboard/W1/20170116_161148.jpg
)

%
We can also state and check properties relating the semantic and the
syntactic operations:

|a + b = eval (Plus (embed a) (embed b))| for all |a| and |b|.



\subsubsection{Other}
TODO: find a good place for this part

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

%TODO: Perhaps also add as an exercise to use Num to make the parameter implicit. But this should perhaps be placed in a later chapter after Num has been properly introduced.

\paragraph{Laws}

TODO: Associative, Commutative, Distributive, \ldots

|Associative (+) = Forall (a, b, c) ((a+b)+c = a+(b+c))|

|Commutative (+) = Forall (a, b) (a+b = b+a)|

Non-examples: division is not commutative, average is commutative but
not associative.

TODO: Talk more about the properties tested above in
|propDistTimesPlus|, \ldots.  (The underlying set matters: |(+)| for
|REAL| has some properties, |(+)| for |Double| has other, \ldots.
Approximation is often necessary, but makes many laws false. Thus,
attempt to do it late.)

TODO: Draw the syntax tree diagrams for distributivity. See \verb+blackboard/W1/20170116_161151.jpg+

|Distributive (*) (+) = Forall (a, b, c) ((a+b)*c = (a*c)+(b*c))|

TODO: Forward pointer to homomorphisms in a later chapter (|*c| is a |(+)|-homomorphism).
\begin{verbatim}
    *    |       +
   / \   |     /   \
  +   c  |    *     *
 / \     |   / \   / \
a   b    |  a   c b   c
\end{verbatim}

Exercise: Find some operator |(#)| which satisfies |Distributive (+) (#)|
% Answer: |max|

Exercise: Find other pairs of operators satisfying a distributive law.



\paragraph{TODO[PaJa]: move earlier}

Table of examples of notation and abstract syntax for some complex numbers:
%\label{tab:CompleSyntaxExamplesMathHaskell}
\begin{tabular}{l||l}
    Mathematics & Haskell
\\\hline
    $3 +2i$                          & |CPlus1 3 2 i|
\\ $\frac{7}{2} - \frac{2}{3} i$ = $\frac{7}{2} + \frac{-2}{3} i$  & |CPlus1 (7/2) (-2/3) i|
\\ $i \pi$ = $0 + i \pi$               & |CPlus2 0 i pi|
\\ $-3$ = $-3 + 0 i$                 & |CPlus1 (-3) 0 i|
\end{tabular}






\subsection{More Haskell}

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


\subsubsection{Notation and abstract syntax for (infinite) sequences}

As a bit of preparation for the language of sequences and limits in
later lctures we here spend a few lines on the notation and abstract
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

TODO: add examples: |id|, |\i->1/(i+1)|, |(2^)|, |const c|, \ldots

TODO: Operations of sequences? lifting constants, |fmap|, lifting binary operators

Exercices: what does function composition do to a sequence?
(composition on the left?, on the right?)

(TODO: perhaps mention limits, sums, just a teasers for later chapters)

% ----------------------------------------------------------------

\subsection{Some helper functions (can be skipped)}

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

\subsection{Exercises for Week 1: complex numbers and DSLs}

TODO: formulate simpler exercises to start with.

\begin{enumerate}
\item Read the full chapter and complete the definition of the
  instance for |Num| for the datatype `ComplexSyn`.
  %
  Also add a constructor for variables to enable writing expressions
  like |(Var "z") :*: toComplex 1|.
\item Read the next few pages of Appendix I (in
  \citep{adams2010calculus}) defining the polar view of Complex Numbers
  and try to implement complex numbers again, this time based on
  magnitude and phase for the semantics.
\item Implement a simplifier |simp :: ComplexSyn r -> ComplexSyn r|
  that handles a few cases like |0 * x = 0|, |1 * x = x|, |(a + b) * c
  = a * c + b * c|, \ldots
  %
  What class context do you need to add to the type of |simp|?
\end{enumerate}


TODO: Perhaps formulate exercise to implement more efficient show
using an ackumulating parameter.
