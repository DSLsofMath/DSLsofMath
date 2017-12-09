% TODO (by DaHe): One or more exercises on "characterize the homomorphisms from
% X to Y"

% TODO (by DaHe): One or more exercises on "is X a homomorphism? Show that Y is
% a homomorphism". Can start off with some really easy ones to get students
% familiar with the idea of homomorphism, and how to check if something is a
% homomorphism. Then move on to an example that requires some more mathematcal
% thought, like "show that exp is a homomorphism". They will of course be able
% to look up this proof in a math textbook, but it will still be forced to think
% of the definition in terms of homomorphisms.

% TODO (by DaHe): Expand on the 'GoodClass' exercise from the chapter, making it
% a little more approachable. Maybe show some more examples of what we want to
% be able to do with it.
%

\begin{exercise}
  Complete the instance declarations for |FunExp| (for |Num|,
  |Fractional|, and |Floating|).
\end{exercise}

\begin{exercise}
  Complete the instance declarations for |(Double, Double)|, deriving
  them from the homomorphism requirement for |apply| (from the end of
  the week 4 lecture notes).
\end{exercise}

\begin{exercise}
  We now have three different ways of computing the derivative of a
  function such as |f x = sin x + exp (exp x)| at a given point, say
  |x = pi|.
  \begin{enumerate}

    \item Find |e :: FunExp| such that |eval e = f| and use |eval'|.

    \item Find an expression of (the first version of) type |FD Double|
    and use |apply|.

    \item Apply |f| directly to the appropriate |(x, x')| and use |snd|.

\end{enumerate}

  Do you get the same result?
\end{exercise}



\begin{exercise}
  \textit{From exam 2017-08-22}

  In exercise \ref{exc:e1_semiring} we looked at the datatype |SR v| for the language of semiring
  expressions. We will now use some of the concepts discussed in this chapter to
  expand on this language.

  \begin{enumerate}

    \item Define a type class |SemiRing| that corresponds to the semiring
      structure.

    \item Define a |SemiRing| instance for the datatype |SR v| that you
      defined in exercise 1.3.

    \item Find two other instances of the |SemiRing| class. 

    \item Specialise the evaluator that you defined in exercise 1.3.2 to the two
      |SemiRing| instances defined above. Take three semiring
      expressions of type |SR String|, give the appropriate assignments and
      compute the results of evaluating, in each case, the three expressions.

  \end{enumerate}
\end{exercise}

\begin{exercise}
  \textit{From exam 2016-03-15}

  In exercise \ref{exc:e1_lattice}, we looked a datatype for the language of lattice
  expressions. We will now use some of the concepts discussed in this chapter to
  expand on this language.

  \begin{enumerate}

    \item Define a type class |Lattice| that corresponds to the lattice
      structure.

    \item Define a |Lattice| instance for the datatype for lattice expressions
      that you defined in 1.4.1.

    \item Find two other instances of the |Lattice| class.

    \item Specialise the evaluator you defined in exercise 1.4.2 to the two
      |Lattice| instances defined above. Take three lattice expressions, give
      the appropriate assignments and compute the results of evaluating, in each
      case, the three expressions.

  \end{enumerate}

\end{exercise}

 \begin{exercise}
  \textit{From exam 2016-08-23}


  In exercise \ref{exc:e1_abMon}, we looked a datatype for the language of abelian monoid
  expressions. We will now use some of the concepts discussed in this chapter to
  expand on this language.

  \begin{enumerate}

    \item Define a type class |AbMonoid| that corresponds to the abelian monoid
      structure.

    \item Define an |AbMonoid| instance for the datatype for abelian monoid
      expressions that you defined in exercise 1.5.1.

    \item Find one other instance of the |AbMonoid| class and give an example
      which is *not* an instance of |AbMonoid|.

    \item Specialise the evaluator that you defined in exercise 1.5.2 to the
      |AbMonoid| instance defined above. Take three `AbMonoidExp` expressions,
      give the appropriate assignments and compute the results of evaluating the
      three expressions.

  \end{enumerate}

\end{exercise}


\begin{exercise}
  (Closely related to exam question)

  A \textit{ring} is a set |A| together with two constants (or nullary
  operations), |0| and |1|, one unary operation, |negate|, and two
  binary operations, |+| and |*|, such that

  \begin{enumerate}

    \item |0| is the neutral element of |+|

      \begin{spec}
        ∀ x ∈ A      x + 0 = 0 + x = x
      \end{spec}

    \item |+| is associative

      \begin{spec}
        ∀ x, y, z ∈ A      x + (y + z) = (x + y) + z
      \end{spec}

    \item |negate| inverts elements with respect to addition

      \begin{spec}
        ∀ x ∈ A      x + negate x = negate x + x = 0
      \end{spec}

    \item |+| is commutative

      \begin{spec}
        ∀ x, y ∈ A      x + y = y + x
      \end{spec}

    \item |1| is the unit of |*|

      \begin{spec}
        ∀ x ∈ A     x * 1 = 1 * x = x
      \end{spec}

    \item |*| is associative

      \begin{spec}
        ∀ x, y, z ∈ A      x * (y * z) = (x * y) * z
      \end{spec}

    \item |*| distributes over |+|

      \begin{spec}
        ∀ x, y, z ∈ A      x * (y + z)  =  (x * y) + (x * z)
        ∀ x, y, z ∈ A      (x + y) * z  =  (x * z) + (y * z)
      \end{spec}


  \end{enumerate}

  Remarks:

  \begin{itemize}
    \item a. and b. say that |(A, 0, +)| is a monoid
    \item a---c. say that |(A, 0, +, negate)| is a group
    \item a---d.  say that |(A, 0, +, negate)| is a commutative group
    \item e. and f. say that |(A, 1, *)| is a monoid
  \end{itemize}

  \begin{enumerate}
    \item  Define a type class `Ring` that corresponds to the ring structure.

    \item  Define a datatype for the language of ring expressions (including
           variables) and define a `Ring` instance for it.

    \item  Find two other instances of the `Ring` class.

    \item  Define a general evaluator for `Ring` expressions on the basis of
    a given assignment function.

    \item  Specialise the evaluator to the two `Ring` instances defined at point
      3.  Take three ring expressions, give the appropriate assignments and
      compute the results of evaluating, in each case, the three expressions.
  \end{enumerate}
\end{exercise}

\begin{exercise}
  \textit{From exam 2017-03-14}

  Recall the type of expressions

  \begin{code}
  data FunExp  =  Const Rational
               |  Id
               |  FunExp  :+:  FunExp
               |  FunExp  :*:  FunExp
               |  FunExp  :/:  FunExp
               |  Exp  FunExp
               |  Sin  FunExp
               |  Cos  FunExp
               -- and so on
               deriving Show
  \end{code}

  and consider the function

  \begin{code} f :: Double -> Double f x = exp (sin x) + x \end{code}

  \begin{enumerate}

    \item Find an expression |e| such that |eval e == f| and show this using
      equational reasoning.

    \item Implement a function |deriv2| such that, for any |f : Fractional a =>
      a -> a| constructed with the grammar of |FunExp| and any |x| in the domain
      of |f|, we have that |deriv2 f x| computes the second derivative of |f| at
      |x|.
  %
      Use the function |derive :: FunExp -> FunExp| from the lectures (|eval
      (derive e)| is the derivative of |eval e|).
  %
      What instance declarations do you need?

      The type of |deriv2 f| should be |Fractional a => a -> a|.

  \end{enumerate}


\end{exercise}


\begin{exercise}

  Based on the \hyperref[types-in-mathematics]{lecture notes}, complete all the instance and
  datatype declarations and definitions in the files \\
  \href{https://github.com/DSLsofMath/DSLsofMath/tree/master/L/DSLsofMath/FunNumInst.lhs}{FunNumInst.lhs}, \\
  \href{https://github.com/DSLsofMath/DSLsofMath/tree/master/L/DSLsofMath/FunExp.lhs}{FunExp.lhs}, \\
  \href{https://github.com/DSLsofMath/DSLsofMath/tree/master/L/DSLsofMath/Derive.lhs}{Derive.lhs}, \\
  \href{https://github.com/DSLsofMath/DSLsofMath/tree/master/L/DSLsofMath/EvalD.lhs}{EvalD.lhs}, and \\
  \href{https://github.com/DSLsofMath/DSLsofMath/tree/master/L/DSLsofMath/ShallowD.lhs}{ShallowD.lhs}.

\end{exercise}


\begin{exercise}
  Write a function

  |simplify  ::  FunExp -> FunExp|

  to simplify the expression resulted from |derive|.  For example

  |simplify (Const 0 :*: Exp Id)   =  Const 0|
  |simplify (Const 0 :+: Exp Id)   =  Exp Id|
  |simplify (Const 2 :*: Const 1)  =  Const 2|

\end{exercise}
