\newpage
\section{Exercises}
%if False
\begin{code}
import DSLsofMath.FunExp
import DSLsofMath.Simplify
\end{code}
%endif

\begin{exercise}\label{exc:homomorphisms}
\textbf{Homomorphisms.}
\index{homomorphism}%
Consider the following definitions:
\index{H2@@|H2(h,Op,op)|{}||textbf}%
\index{H1@@|H1(h,F,f)|{}||textbf}%
\index{H0@@|H0(h,E,e)|{}||textbf}%
%{
%format -> = "\!\to\!"
\begin{spec}
  -- |h : A -> B| is a homomorphism from |Op : A->A->A| to |op : B->B->B|
  H2(h,Op,op)  =  Forall x (Forall y (h(Op x y) == op (h x) (h y)))
  -- |h : A -> B| is a homomorphism from |F : A -> A| to |f : B -> B|
  H1(h,F,f)    =  Forall x (h(F x) == f (h x))
  -- |h : A -> B| is a homomorphism from |E : A| to |e : B|
  H0(h,E,e)    =  h E == e
\end{spec}
%}
Prove or disprove the following claims:
\begin{itemize}
\item |H2((2*),(+),(+))|
\item |H2((2*),(*),(*))|
\item |H2(exp,(+),(*))|
\item |H2(eval',(:+:),(+))|
\item |H1(sqrt,(4*),(2*))|
\item |Exists f (H1(f,(2*).(1+),(1+).(2*)))|
  % |Exists f (Forall x (f.(2*).(1+) == (1+).(2*).f))|
  % |Exists f (Forall x (f (2*(1+x)) == 1+2*(f x)))|
  % |Exists f (Forall x (f (2*(1+x)) == 1+2*(f x)))|
  % Assume f = const c
  % |c == 1+2*c|
  % |-1 == c|
  % So, yes, there exists such a function, and |const (-1)| is an example.
\end{itemize}

\end{exercise}

%TODO (by DaHe): One or more exercises on "characterise the homomorphisms from
% X to Y"

\begin{exercise}
  Complete the numeric instance declarations for |FunExp|.
\end{exercise}

\begin{exercise}
  Complete the instance declarations for |Dup REAL|, deriving them
  from the homomorphism requirement for |applyFD| (in
  \refSec{sec:apply}).
\end{exercise}

\begin{exercise}
  We now have three different ways of computing the derivative of a
  function such as |f x = sin x + exp (exp x)| at a given point, say
  |x = pi|.
  \begin{enumerate}

    \item Find |e :: FunExp| such that |eval e = f| and use |eval'|.

    \item Find an expression of type |FD REAL| and use |apply|.

    \item Apply |f| directly to the appropriate |(x, x')| and use |snd|.

\end{enumerate}

  Do you get the same result?
\end{exercise}



\begin{exercise}
  \fromExam{2017-08-22}

  In Exercise~\ref{exc:e1_semiring} we looked at the datatype |SR v|
  for the language of \addtoindex{semiring} expressions.
  %
  We will now use some of the concepts discussed in this chapter to
  expand on this language.

  \begin{enumerate}

    \item Define a type class |SemiRing| that corresponds to the semiring
      structure.

    \item Define a |SemiRing| instance for the datatype |SR v| that you
      defined in exercise 1.3.

    \item Find two other instances of the |SemiRing| class.

    \item Specialise the evaluator that you defined in
      Exercise~\ref{exc:e1_semiring} to the two |SemiRing| instances
      defined above.
      %
      Take three semiring expressions of type |SR String|, give the
      appropriate assignments and compute the results of evaluating,
      in each case, the three expressions.

  \end{enumerate}
\end{exercise}

\begin{exercise}
  Show that arithmetic modulo |n| satisfies the semiring laws (it is
  even a ring).
%
  In more details: show that |Zn = {0,1,...,n-1}| with |plus x y = mod (x+y)
  n| and |times x y = mod (x*y) n| forms a semiring.
%

  With |h x = mod x n|, show that |h| is a homomorphism from |ZZ| to
  |Zn|.
\end{exercise}

\begin{exercise}
  \fromExam{2016-03-15}

  In \cref{exc:e1_lattice}, we looked at a datatype for the language of
  \addtoindex{lattice} expressions.
  %
%  We will now use some of the concepts discussed in this chapter to
%  expand on this language.

  \begin{enumerate}

    \item Define a type class |Lattice| that corresponds to the
      lattice structure.

    \item Define a |Lattice| instance for the datatype for lattice
      expressions that you defined in \cref{exc:e1_lattice}.

    \item Find two other instances of the |Lattice| class.

    \item Specialise the evaluator you defined in
      \cref{exc:e1_lattice} to the two |Lattice| instances defined
      above.
      %
      Take three lattice expressions, give the appropriate assignments
      and compute the results of evaluating, in each case, the three
      expressions.

  \end{enumerate}

\end{exercise}

 \begin{exercise}
  \fromExam{2016-08-23}

  In Exercise~\ref{exc:e1_abMon}, we looked at a datatype for the
  language of abelian \addtoindex{monoid} expressions.
  %
  We will now use some of the concepts discussed in this chapter to
  expand on this language.

  \begin{enumerate}

    \item Define a type class |AbMonoid| that corresponds to the
      abelian monoid structure.

    \item Define an |AbMonoid| instance for the datatype for abelian
      monoid expressions that you defined in
      Exercise~\ref{exc:e1_abMon}.

    \item Find one other instance of the |AbMonoid| class and give an
      example which is \textbf{not} an instance of |AbMonoid|.

    \item Specialise the evaluator that you defined in
      Exercise~\ref{exc:e1_abMon} to the |AbMonoid| instance defined
      above.
      %
      Take three |AbMonoidExp| expressions, give the appropriate
      assignments and compute the results of evaluating the three
      expressions.

  \end{enumerate}

\end{exercise}


\begin{exercise}

  \lnOnly{(Closely related to exam question)}

  A \textit{\addtoindex{ring}} is a set |A| together with two
  constants, |0| and |1|, one unary operation, |negate|, and two
  binary operations, |(+)| and |(*)|, such that

  \begin{enumerate}[label=\alph*.,itemsep=0ex]
  \item |0| is the neutral element of |(+)|
  \label{item:addZero}
%
      \begin{spec}
        Forall (x ∈ A)      (x + 0 = 0 + x = x)
      \end{spec}
%
  \item |(+)| is associative
  \label{item:addAssoc}
%
      \begin{spec}
        Forall (x, y, z ∈ A)     (x + (y + z) = (x + y) + z)
      \end{spec}
%
  \item |negate| inverts elements with respect to addition
  \label{item:addNeg}
%
      \begin{spec}
        Forall (x ∈ A)      (x + negate x = negate x + x = 0)
      \end{spec}
%
  \item |(+)| is commutative
  \label{item:addComm}
%
      \begin{spec}
        Forall (x, y ∈ A)      (x + y = y + x)
      \end{spec}
%
  \item |1| is the unit (neutral element) of |(*)|
  \label{item:mulOne}
%
      \begin{spec}
        Forall (x ∈ A)     (x * 1 = 1 * x = x)
      \end{spec}
%
  \item |(*)| is associative
  \label{item:mulAssoc}
%
      \begin{spec}
        Forall (x, y, z ∈ A)      (x * (y * z) = (x * y) * z)
      \end{spec}
%
  \item |(*)| distributes over |(+)|
%
      \begin{spec}
        Forall (x, y, z ∈ A)      (x * (y + z)  =  (x * y) + (x * z))
        Forall (x, y, z ∈ A)      ((x + y) * z  =  (x * z) + (y * z))
      \end{spec}
%
  \end{enumerate}
%
  Remarks:
  \begin{itemize}[itemsep=0ex]
  \item \ref{item:addZero} -- \ref{item:addAssoc} say that |(A, 0, +)|
    is a monoid and \ref{item:mulOne} -- \ref{item:mulAssoc} that |(A,
    1, *)| is a monoid
  \item \ref{item:addZero} -- \ref{item:addNeg} say that |(A, 0, +,
    negate)| is a group
  \item \ref{item:addZero} -- \ref{item:addComm} say that |(A, 0, +,
    negate)| is a commutative (abelian) group
  \end{itemize}

  \begin{enumerate}[label=\roman*,itemsep=0ex]
  \item Define a type class |Ring| that corresponds to the ring
    structure.
  \item Define a datatype for the language of ring expressions
    (including variables) and define a |Ring| instance for it.
  \item \label{point:otherinstances} Find two other instances of the
    |Ring| class.
  \item Define a general evaluator for |Ring| expressions on the basis
    of a given \addtoindex{assignment function} (mapping variables to
    semantic values).
  \item Specialise the evaluator to the two |Ring| instances defined
    at point~\ref{point:otherinstances}.
    %
    Take three ring expressions, give the appropriate assignments and
    compute the results of evaluating, in each case, the three
    expressions.
  \end{enumerate}
\end{exercise}

\begin{exercise}
  \fromExam{2017-03-14} (Note that the |Num| hierarchy has been
  replaced by |Additive|, |AddGroup|, etc.)

  Recall the type of expressions of one variable from
  \cref{sec:FunExp}.

\begin{spec}
data FunExp  =  Const Rational           |  X
             |  FunExp  :+:  FunExp      |  Exp  FunExp
             |  FunExp  :*:  FunExp      |  Sin  FunExp
             |  FunExp  :/:  FunExp      |  Cos  FunExp
             -- and so on
             deriving Show
\end{spec}
%
  and consider the function
%
\begin{code}
f :: REAL -> REAL
f x = exp (sin x) + x
\end{code}
%
\begin{enumerate}
  \item Find an expression |e| such that |eval e == f| and show this
    using equational reasoning.


  \item Implement a function |deriv2| such that, for any
%
    |f :: Fractional a => a -> a| constructed with the grammar of
    |FunExp| and any |x| in the domain of |f|, we have that
    %
    |deriv2 f x| computes the second derivative of |f| at |x|.
  %
    Use the function |derive :: FunExp -> FunExp| from the lectures
    (|eval (derive e)| is the derivative of |eval e|).
  %
    What instance declarations do you need?

    The type of |deriv2 f| should be |Fractional a => a -> a|.

  \end{enumerate}


\end{exercise}


%if lectureNotes
\begin{exercise}
  %
  Complete all the instance and datatype declarations and definitions
  in the files
  \href{https://github.com/DSLsofMath/DSLsofMath/tree/master/L/DSLsofMath/FunNumInst.lhs}{FunNumInst.lhs},
  \href{https://github.com/DSLsofMath/DSLsofMath/tree/master/L/DSLsofMath/FunExp.lhs}{FunExp.lhs},
  \href{https://github.com/DSLsofMath/DSLsofMath/tree/master/L/DSLsofMath/Derive.lhs}{Derive.lhs},
  \href{https://github.com/DSLsofMath/DSLsofMath/tree/master/L/DSLsofMath/EvalD.lhs}{EvalD.lhs},
  and
  \href{https://github.com/DSLsofMath/DSLsofMath/tree/master/L/DSLsofMath/ShallowD.lhs}{ShallowD.lhs}.

\end{exercise}
%endif


\begin{exercise}\label{exc:simplifyFunExp}
  Write a function |simplify :: FunExp -> FunExp|
  %
  to simplify the expression resulting from |derive|.
  %
  For example, the following tests should work:
\begin{code}
testSimplify = -- all evaluate to |True|
  [  simplify  (Const 0  :*:  Exp X)    ==  Const 0
  ,  simplify  (Const 0  :+:  Exp X)    ==  Exp X
  ,  simplify  (Const 2  :*:  Const 1)  ==  Const 2
  ,  simplify  (derive (X:*:X))         ==  Const 2 :*: X
  ]
\end{code}

As a motivating example, note that without |simplify| we have |derive
(X:*:X)| |==| |(Const 1 :*: X) :+: (X :*: Const 1)|, and that the
syntax tree of the second derivative is twice that size.
\end{exercise}

\subsection{Project: Optimisation using Newton's method}

This subsection describes a larger exercise (or small project) you can
use to practice what you have learnt so far.
%
It is heavily based on \cref{sec:types} and \cref{sec:CompSem} (the
|FunExp| type, |eval|, |derive|, |D|, tupling, homomorphisms, |FD|,
|apply|, \ldots) so it pays off to work through those parts carefully.

\paragraph{Part 1}
The evaluation of the second derivative is given by
%
\begin{spec}
  eval'' = eval' . derive = eval . derive . derive
\end{spec}
%
\begin{enumerate}[label=\alph*)]
\item Let |P(h) = | ``|h| is a homomorphism from |FunExp| to
  |FunSem = REAL -> REAL|''.
  %
  Express |P| in logic and show |not P(eval'')|.

\item Given the types in the skeleton code below, define instances of
  the classes |Additive|, |AddGroup|, |Multiplicative|, |MulGroup|,
  and |Transcendental|, for |Tri a|.
%
  Test your results using algebraic identities like |sin^2 + cos^2 =
  const one|.

\item Define a homomorphism |evalDD| from |FunExp| to |FunTri a| (for
  any type |a| in the class |Transcendental|).
  %
  You don't need to prove that it is a homomorphism in this part.

\item Show that |evalDD| is a homomorphism for the case of
  multiplication.

\end{enumerate}
\paragraph{Part 2}
Newton's method allows us to find zeros of a large class of functions
in a given interval.  The following description of Newton's method
follows \cite{bird1988introduction}, page 23:

\begin{code}
newton :: (REAL -> REAL) -> REAL -> REAL -> REAL
newton f eps x  =  if abs fx < eps   then  x
                   else if fx' /= 0  then  newton f eps next
                                     else  newton f eps (x + eps)
          where  fx    = f x
                 fx'   = undefined -- should be |f' x| (derivative of |f| at |x|)
                 next  = x - (fx / fx')
\end{code}

\begin{enumerate}[label=\alph*)]
\item
Implement Newton's method, using |Tri REAL -> Tri REAL| for the type
of the first argument.
%
In other words, use the code above to implement
%
\begin{spec}
newtonTri :: (Tri REAL -> Tri REAL) -> REAL -> REAL -> REAL
\end{spec}
%
in order to obtain the appropriate value for |f' x|.

\item
Test your implementation on the following functions:
%
\begin{code}
test0 x = x^2                   -- one (double) zero, in zero
test1 x = x^2 - one             -- two zeros, in +-one
test2 x = sin x                 -- many, many zeros (in |n*pi| for all |n :: ZZ|)
test3 n x y = y^n - constTri x  -- |test3 n x|, has zero in "nth roots of |x|"
  -- where |constTri| is the embedding of |Const|
\end{code}
Note that these functions can work at different types: |REAL -> REAL|
or |Dup REAL -> Dup REAL| or |Tri REAL -> Tri REAL|, etc.

For each of these functions, apply Newton's method to a number of
starting points from a sensible interval.
%
For example:
%
\begin{spec}
  map (newton test1 0.001) [-2.0, -1.5 .. 2.0]
\end{spec}
%
but be aware that the method might not always converge!
%

For debugging is advisable to implement |newton| in terms of
|newtonList|, a minor variation which returns a list of the
approximations encountered on the way to the final answer:
%
\begin{spec}
newton f eps x = last (newtonList f eps x)

newtonList f eps x = x : if ... then [] else ...
\end{spec}
\end{enumerate}

\paragraph{Part 3} We can find the optima of a twice-differentiable
function on an interval by finding the zeros of its derivative on that
interval, and checking the second derivative.
%
  If |f' x0| is zero, then
  \begin{itemize}
  \item if |f'' x0 < 0|, then |x0| is a maximum
  \item if |f'' x0 > 0|, then |x0| is a minimum
  \item if |f'' x0 = 0|, then, if |f'' (x0 - ε) * f'' (x0 + ε) < 0|
    (i.e., |f''| changes its sign in the neighbourhood of |x0|), |x0|
    is an inflection point (not an optimum)
  \item otherwise, we don't know
  \end{itemize}

  Use Newton's method to find the optima of the test functions from
  point 2.
%
  That is, implement a function
%
\begin{spec}
optim :: (Tri REAL -> Tri REAL) -> REAL -> REAL -> Result REAL
\end{spec}
%
so that |optim f eps x| uses Newton's method to find a zero of |f'|
starting from |x|.
%
If |y| is the result (i.e. |f' y| is within |eps| of |0|), then check
the second derivative, returning |Maximum y| if |f'' y < 0|, |Minimum y|
if |f'' y > 0|, and |Dunno y| if |f'' = 0|.

As before, use several starting points to test if you get the expected
behaviour.

\paragraph{Skeleton code}
Here is some useful
\href{https://github.com/DSLsofMath/DSLsofMath/blob/master/L/A2_Skeleton.lhs}{skeleton
  Haskell code} to start from, and the
\href{https://github.com/DSLsofMath/DSLsofMath/blob/master/L/DSLsofMath/Algebra.hs}{|Algebra|}
and
\href{https://github.com/DSLsofMath/DSLsofMath/blob/master/L/DSLsofMath/FunExp.lhs}{|FunExp|}
modules are also available on
\href{https://github.com/DSLsofMath/DSLsofMath/}{github}.

%include ../A2_Skeleton.lhs

