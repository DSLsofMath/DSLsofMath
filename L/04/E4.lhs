% TODO (by DaHe): One or more exercises on 'characterize the homomorphisms from
% X to Y'

% TODO (by DaHe): One or more exercises on 'is X a homomorphism? Show that Y is
% a homomorphism'. Can start off with some really easy ones to get students
% familiar with the idea of homomorphism, and how to check if something is a
% homomorphism. Then move on to an example that requires some more mathematcal
% thought, like "show that exp is a homomorphism". They will of course be able
% to look up this proof in a math textbook, but it will still be forced to think
% of the definition in terms of homomorphisms.

% TODO (by DaHe): Expand on the 'GoodClass' exercise from the chapter, making it
% a little more approachable. Maybe show some more examples of what we want to
% be able to do with it.
%

% TODO (by DaHe): Proper way to reference exercises?
\begin{exercise}
  \textit{From exam 2017-08-22}

  In exercise 1.3, we looked at the datatype |SR v| for the language of semiring
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

  In exercise 1.4, we looked a datatype for the language of lattice
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


  In exercise 1.5, we looked a datatype for the language of abelian monoid
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
