\subsection{Exercises for Week 1: complex numbers and DSLs}

TODO: formulate simpler exercises to start with. (Working on it (They will hopefully be simpler //DaHe)).

\begin{enumerate}
%
\item Consider the following data type for simple arithmetic
   expressions:
   %
   % TODO more/less confusing to use infix vs prefix notation here?
   %
   \begin{code}
   data Exp = Num Integer 
            | Exp `Plus`  Exp
            | Exp `Minus` Exp 
            | Exp `Times` Exp
   deriving(Eq, Show)
   \end{code}
   %
   \begin{enumerate}
   %
    \item Write the following expressions in Haskell, using the |Exp| data type:
    \begin{enumerate}
        \item \(a1 = 2 + 2\) 
        \item \(a2 = 5 + 7 * 9\) 
        \item \(a3 = 8(2 + 11) - (3 + 7)(4 + 4)\) 
    \end{enumerate}
    %
    \item Create a function |eval :: Exp -> Integer| that takes a value of the |Exp| data type
       and returns the corresponding number (for instance, |eval ((Num 3) `Plus` (Num 3)) == 6|).
       Try it on the expressions from part a), and verify that it works as expected.
    %
    \item Consider the following expression: 
       $$c2 = (x - 15)*(y + 12)*z$$ 
       where: \\
       $x = 5$, \\
       $y = 8$, \\
       $z = 13$

       In order to represent this with our |Exp| data type, we are going to have
       to make some modifications:
       %
       \begin{enumerate}
        %
        \item Update the |Exp| data type with a new constructor |Var String| that allows variables
        with strings as names to be represented. Use the updated |Exp| to write
        an expression for |c2| in Haskell.
        %
        \item Create a function |varVal :: String -> Integer| that takes a
        variable name, and returns the value of that variable. For now, the
        function just needs to be defined for the variables in the expression
        above, i.e. |varVal x| should return 5, |varVal y| should return 8, and
        |varVal x| should return 13.
        %
        \item Update the |eval| function so that it supports the new |Var|
        constructor, and use it get a numeric value of the expression $c2$.
        %
       \end{enumerate}
    %
   \end{enumerate}
 
TODO (DaHe): Exercise on type class/parametrised types, with evaluator and env,
var lookup
 
TODO (DaHe): Exercise introducing deep vs shallow embedding using type classes
also, maybe introduce the concept of using type classes to return a deep
embedding by using the operators of the data type, and casting to the 
syntactic type
 
TODO (DaHe): Exercise asking to implement a deep and a shallow embedding of some data
type, using knowlege acquired from above
 
TODO (DaHe): Describe a thing, ask to implement DSL for that thing by introducing
data type, type class, evaluator, (similar to Q1 from old exams)
%
%
%
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
