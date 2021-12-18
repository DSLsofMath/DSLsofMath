\section{A syntax for (complex) arithmetical expressions}
\label{sec:complex-arithmetic}

By following \citet{adams2010calculus}, we have arrived at
a representation which captures the \emph{\addtoindex{semantics}} of
complex numbers.
%
This kind of representation is often called a ``\addtoindex{shallow
  embedding}''.
%
Now we turn to the study of the \emph{\addtoindex{syntax}} instead
(``\addtoindex{deep embedding}'').
%
We collect these syntactic definitions in a separate module which
imports the earlier semantic definitions.

\begin{code}
module DSLsofMath.ComplexSyn where
import DSLsofMath.CSem (Complex(C), addC, mulC, Ring)
import DSLsofMath.ComplexSem
\end{code}

\index{AST||see {abstract syntax tree}}%
%
We want a datatype |ComplexE| for the \addtoindex{abstract syntax
  tree} (AST) of expressions (a DSL for complex arithmetical
expressions).
%
\index{DSL!complex expressions}%
%
The syntactic expressions can later be evaluated to semantic values.
%
The concept of ``an evaluator'', a function from the syntax to the
semantics, is something we will return to many times in this
\course{}.
%
\index{eval@@|eval : Syn -> Sem|}%
%
\begin{code}
evalE :: ComplexE -> ComplexD
\end{code}
%
The datatype |ComplexE| should collect ways of building syntactic
expressions representing complex numbers and we have so far seen
%
the symbol |i|, an embedding from |REAL|, addition and multiplication.
%
\index{constructor function}%
%
We make these four \emph{constructors} in one recursive datatype as
follows:
%
%format I2 = I
\begin{code}
data ComplexE  =  I2
               |  ToComplex REAL
               |  Add   ComplexE  ComplexE
               |  Mul  ComplexE  ComplexE
 deriving (Eq, Show)
\end{code}
\index{deriving@@|deriving| (keyword)}%
\index{Eq@@|Eq| (type class)}%
%
Note that, in |ComplexA| above, we also had a constructor for
addition (|CPlus1|), but it was playing a different role.
%
They are distinguished by type: |CPlus1| took two real numbers as
arguments, while |Add| here takes two complex expressions as
arguments.

Here are two examples of type |ComplexE| as Haskell code and as
ASTs:
\begin{code}
testE1 = Mul I2 I2
testE2 = Add (ToComplex 3) (Mul (ToComplex 2) I2)
\end{code}

\hspace{2em}
\begin{tikzpicture}[level 1/.style={sibling distance=3cm},baseline]
\node{|Mul|}
child {node {|I2|}}
child {node {|I2|}};
\end{tikzpicture}
\hspace{2em}
\begin{tikzpicture}[level 1/.style={sibling distance=3cm},baseline]
\node{|Add|}
child {node {|ToComplex|} child {node {|3|}}}
child {node {|Mul|}
  child {node {|ToComplex|} child {node {|2|}}}
  child {node {|I2|}}};
\end{tikzpicture}

We can implement the evaluator |evalE| by pattern matching on the
constructors of the syntax tree and by recursion.
%
To write a recursive function requires a small leap of faith.
%
It can be difficult to get started implementing a function (like
|evalE|) that should handle all the cases and all the levels of a
recursive datatype (like |ComplexE|).
%
One way to overcome this difficulty is through what may seem at first
glance ``\addtoindex{wishful thinking}'': assume that all but one case
have been implemented already.
%
All you need to do is to focus on that one remaining case, and you can
freely call the function (that you are implementing) recursively, as
long as you do it for subexpressions (subtrees of the abstract syntax
tree datatype).
%
This pattern is called \emph{\addtoindex{structural induction}}.

For example, when implementing the |evalE (Add c1 c2)| case, you can
assume that you already know the values |s1, s2 :: ComplexD|
corresponding to the subtrees |c1| and |c2| of type |ComplexE|.
%
The only thing left is to add them up componentwise and we can assume
there is a function |addD :: ComplexD -> ComplexD -> ComplexD| taking
care of this step (in fact, we implemented it earlier in
\refSec{sec:complexcase}).
%
Continuing in this direction (by structural induction; or ``wishful
thinking'') we arrive at the following implementation.
%
\begin{code}
evalE I2              = iD
evalE (ToComplex r)   = toComplexD r
evalE (Add  c1 c2)    = addD  (evalE c1)  (evalE c2)
evalE (Mul  c1 c2)    = mulD  (evalE c1)  (evalE c2)
\end{code}
%
Note the pattern here: for each constructor of the syntax datatype we
assume that there exists a corresponding semantic function.
%
The next step is to implement these functions, but let us first list
their types and compare them with the types of the syntactic constructors:
%
\begin{spec}
I2   :: ComplexE
iD   :: ComplexD

ToComplex   :: REAL -> ComplexE
toComplexD  :: REAL -> ComplexD

Mul       :: ComplexE  -> ComplexE  -> ComplexE
mulD      :: ComplexD  -> ComplexD  -> ComplexD
\end{spec}
%addD   :: ComplexD -> ComplexD -> ComplexD  -- |ComplexE -> ComplexE -> ComplexE|
As we can see, each use of |ComplexE| has been replaced be a use of
|ComplexD|.
%
Finally, we can start filling in the implementations:
%
\begin{code}
iD            = CD (0 ,  1)
toComplexD r  = CD (r ,  0)
\end{code}
%
The function |addD| was defined earlier and |mulD| is left as an
exercise for the reader.
%
To sum up we have now implemented a recursive datatype for
mathematical expressions describing complex numbers, and an evaluator
that computes the underlying number.
%
Note that many different syntactic expressions will evaluate to the
same number (|evalE| is not injective).

Generalising from the example of |testE2| we also define a function to
embed a semantic complex number in the syntax:
%
\begin{code}
fromCD :: ComplexD -> ComplexE
fromCD (CD (x , y)) = Add (ToComplex x) (Mul (ToComplex y) I2)
\end{code}
%
This function is injective: different complex numbers map to different
syntactic expressions.

\section{Laws, properties and testing}
There are certain laws that we would like to hold for operations on complex
numbers.
%
To specify these laws, in a way which can be easily testable in
Haskell, we use functions to |Bool| (also called
\emph{\addtoindex{predicate}s} or \emph{properties}).
%
The intended meaning of such a boolean function (representing a law)
is ``forall inputs, this should return |True|''.
%
This idea is at the core of \emph{\addtoindex{property based testing}}
(pioneered by \citet{claessen_quickcheck_2000}) and conveniently
available in the Haskell library QuickCheck.
%
\index{QuickCheck||see {property based testing}}%

Note that a predicate |p : A -> Bool| can also be used to specify the
\addtoindex{subset} of |A| for which |p| returns |True|.
%
QuickCheck is very good at finding \addtoindex{counterexample}s to
candidate laws: values for which |p| returns |False|.
%
With the subset interpretation this means that QuickCheck helps
finding elements in the set specified by the opposite predicate |not
. p : A -> Bool|.
%
When it fails, we hope that the set of counterexamples is actually
empty, but we cannot know that for sure without a proof (or by
exhaustive testing of all cases).

%
The simplest law is perhaps |square i = -1| from the start of
\refSec{sec:complexcase},
%
\begin{code}
propI2 :: Bool
propI2 =  Mul I2 I2 === ToComplex (-1)
\end{code}
%
Note the we use a new operator here, |(===)|.
%
Indeed, we reserve the usual equality |(==)| for syntactic equality
(and here the left hand side (LHS) is clearly not syntactically equal
to the right hand side).
%
The new operator |(===)| corresponds to semantic equality, that is,
equality \emph{after evaluation}:
%

%{
%format .=. = ===
\begin{code}
(.=.) :: ComplexE -> ComplexE -> Bool
z .=. w {-"\quad"-} = {-"\quad"-} evalE z == evalE w
\end{code}
%}

%if false
Unfortunately we have not explained classes yet.
\begin{code}
infix 0 ===
class SemEq a where
  (===) :: a -> a -> Bool
instance SemEq Int where
  (===) = (==)
instance SemEq Double where
  (===) = (==)
instance SemEq ComplexE where
  (===) = (.=.)
\end{code}
%endif

Another law is that |fromCD| is an embedding: if we start from a
semantic value, embed it back into syntax, and evaluate that syntax we
get back to the value we started from.
%
\begin{code}
propFromCD :: ComplexD -> Bool
propFromCD s =  evalE (fromCD s) == s
\end{code}

Other desirable laws are that |+| and |*| should be
\addtoindex{associative} and \addtoindex{commutative} and |*| should
\addtoindex{distribute over} |+|:
%if false
\begin{code}
propAssocAdd    :: (Num a, SemEq a) => a -> a -> a -> Bool
propDistMulAdd  :: (Num a, SemEq a) => a -> a -> a -> Bool
\end{code}
%endif
\begin{code}
propCommAdd     x y                = {-"\quad"-}  x + y          ===  y + x
propCommMul     x y                = {-"\quad"-}  x * y          ===  y * x
propAssocAdd    x y z              = {-"\quad"-}  (x + y) + z    ===  x + (y + z)
propAssocMul    x y z              =              (x * y) * z    ===  x * (y * z)
propDistMulAdd  x y z {-"\quad"-}  =              x * (y + z)    ===  (x * y) + (x * z)
\end{code}

These laws actually fail, but not due to any mistake in the
implementation of |evalE| in itself.
%
To see this, let us consider associativity at different types:

\begin{code}
propAssocInt     = propAssocAdd ::  Int     -> Int     -> Int     -> Bool
propAssocDouble  = propAssocAdd ::  Double  -> Double  -> Double  -> Bool
\end{code}
%
The first property is fine, but the second fails.
%
Why?
%
QuickCheck can be used to find small examples --- this one is perhaps
the best one:
%
\begin{code}
notAssocEvidence :: (Double , Double , Double , Bool)
notAssocEvidence = (lhs , rhs , lhs-rhs , lhs==rhs)
  where  lhs = (1+1)+1/3
         rhs =  1+(1+1/3)
\end{code}
%
For completeness: these are the values:
%
\begin{spec}
  (  2.3333333333333335     -- Notice the five at the end
  ,  2.333333333333333,     -- which is not present here.
  ,  4.440892098500626e-16  -- The (very small) difference
  ,  False)
\end{spec}
%
We can now see the underlying reason why some of the laws failed for
complex numbers: the approximative nature of |Double|.
%
Therefore, to ascertain that there is no other bug hiding, we need to
move away from the implementation of |REAL| as |Double|.
%
We do this by abstraction: we make one more version of the complex
number type, which is parameterised on the underlying representation
type for~|REAL|.
%
At the same time, to reduce the number of constructors, we combine
|I2| and |ToComplex| to |ToComplexCart|, which corresponds to
the primitive form |a + bi| discussed above:
%*TODO: perhaps explain more about the generalisation step.

%TODO: Add as an exercise the version with I | ToComplex | Add ... | Mul ...
% See data blackboard/W1/20170116_114608.jpg, eval blackboard/W1/20170116_114613.jpg
\label{sec:toComplexSyn}
\begin{code}
data ComplexSyn r  =  ToComplexCart r r
                   |  ComplexSyn r  :+:  ComplexSyn r
                   |  ComplexSyn r  :*:  ComplexSyn r

toComplexSyn :: Num a => a -> ComplexSyn a
toComplexSyn x = ToComplexCart x 0
\end{code}
%
From Appendix~\ref{app:CSem} we import |newtype Complex r = C (r ,
r) deriving Eq| and the semantic operations |addC| and |mulC|
corresponding to |addD| and |mulD|.
%
\begin{code}
evalCSyn :: Ring r => ComplexSyn r -> Complex r
evalCSyn (ToComplexCart x y)  = C (x , y)
evalCSyn (l  :+:  r)          = addC  (evalCSyn l)  (evalCSyn r)
evalCSyn (l  :*:  r)          = mulC  (evalCSyn l)  (evalCSyn r)
\end{code}
%
%if False
\label{sec:firstFromInteger}
\begin{code}
negateCS :: Num a => ComplexSyn a -> ComplexSyn a
negateCS = ((-1) :*:)
absCS = error "absCS: missing constructor"
signumCS = error "signumCS: missing constructor"
instance Num a => Num (ComplexSyn a) where
   (+)  = (:+:)
   (*)  = (:*:)
   fromInteger = fromIntegerCS
   negate = negateCS
   abs = absCS
   signum = signumCS
fromIntegerCS :: Num r =>  Integer -> ComplexSyn r
fromIntegerCS = toComplexSyn . fromInteger
\end{code}
%endif

% \begin{exercise}
%   Add a few more operations (hint: extend |ComplexSyn| as well) and extend |eval| appropriately.
% \end{exercise}

With this parameterised type we can test the code for ``complex
rationals'' to avoid rounding errors.
%**TODO: add concrete example
(The reason why math textbooks rarely talk about complex rationals is
because complex numbers are used to handle roots of all numbers
uniformly, and roots are in general irrational.)

%TODO: perhaps include
% We can also state and check properties relating the semantic and the syntactic operations:
%
% |a + b = eval (Add (embed a) (embed b))| for all |a| and |b|.
\subsection{Generalising laws}
\label{sec:generalising-laws}
Some laws appear over and over again in different mathematical
contexts.
%
\index{binary (arity 2)}%
%
For example, binary operators are often associative or commutative, and
sometimes one operator distributes over another.
%
We will work more formally with logic in \cref{sec:logic} but
we introduce a few definitions already here:

|Associative (⊛) = Forall (a, b, c) ((a⊛b)⊛c = a⊛(b⊛c))|

|Commutative (⊛) = Forall (a, b) (a⊛b = b⊛a)|

|Distributive (⊗) (⊕) = Forall (a, b, c) ((a⊕b)⊗c = (a⊗c)⊕(b⊗c))|

\index{parameterised laws}%
%
The above laws are \emph{parameterised} over some operators
(|(⊛),(⊗),(⊕)|).
%
These laws will hold for some operators, but not for others.
%
For example, division is not commutative; taking the average of two
quantities is commutative but not associative.
%
(See also \crefatpage{distributivity-as-homomorphism} for further
analysis of distributivity.)
%
Such generalisations can be reflected in QuickCheck properties as well.

\begin{code}
propAssoc :: SemEq a => (a -> a -> a) -> a -> a -> a -> Bool
propAssoc (⊛) x y z =  (x ⊛ y) ⊛ z === x ⊛ (y ⊛ z)
\end{code}
%
Note that |propAssocA| is a higher order function: it takes a function
|(⊛)| (declared as a binary operator) as its first parameter, and
tests if it is associative.
%
The property is also \addtoindex{polymorphic}: it works for many
different types |a| (all types which have an |===| operator).

Thus we can specialise it to |Add|, |Mul| and any other binary
operator, and obtain some of the earlier laws (|propAssocAdd|,
|propAssocMul|).
%
The same can be done with distributivity.
%
Doing so we learnt that the underlying set matters: |(+)| for |REAL|
has some properties, but |(+)| for |Double| has others.
%
When formalising math as DSLs, \addtoindex{approximation} is sometimes
convenient, but makes many laws false.
%
Thus, we should attempt to do it late, and if possible, leave a
parameter to make the degree of approximation tunable (|Int|,
|Integer|, |Float|, |Double|, |QQ|, syntax trees, etc.).

%**TODO: hide or give hints / method (otherwise too hard and a bit off topic)
%Exercise: Find some operator |(#)| which satisfies |Distributive (+) (#)|
% Answer: |max|

It is a good exercise to find other pairs of operators satisfying
distributive laws.

%if False
\section{Some helper functions (can be skipped)}
\begin{code}
-- mulD :: ComplexD -> ComplexD -> ComplexD
mulD (CD (ar, ai)) (CD (br, bi)) = CD (ar*br - ai*bi, ar*bi + ai*br)

instance Show ComplexD where
  show = showCD

showCD :: ComplexD -> String
showCD (CD (x, y)) = show x ++ " + " ++ show y ++ "i"

propAssocAdd2 :: (SemEq a, Num a) => a -> a -> a -> Bool
propAssocAdd2 = propAssoc (+)
\end{code}
%endif

% end of formatting "bi" as just that (and not as \(b_i\)).
%}

