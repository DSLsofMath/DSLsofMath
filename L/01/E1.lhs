\subsection{Exercises for Week 1: complex numbers and DSLs}

TODO: formulate simpler exercises to start with. (Working on it (They will hopefully be simpler //DaHe)).

\begin{exercise}
  Consider the following data type for simple arithmetic expressions:
   %
   % TODO more/less confusing to use infix vs prefix notation here?
   %
   \begin{code}
   data Exp  =  Num Integer
             |  Exp  `Plus`   Exp
             |  Exp  `Minus`  Exp
             |  Exp  `Times`  Exp
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
       Try it on the expressions from the first part, and verify that it works as expected.
    %
    \item Consider the following expression:
       $$c1 = (x - 15)*(y + 12)*z$$
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
        an expression for |c1| in Haskell.
        %
        \item Create a function |varVal :: String -> Integer| that takes a
        variable name, and returns the value of that variable. For now, the
        function just needs to be defined for the variables in the expression
        above, i.e. |varVal "x"| should return 5, |varVal "y"| should return 8, and
        |varVal "x"| should return 13.
        %
        \item Update the |eval| function so that it supports the new |Var|
        constructor, and use it get a numeric value of the expression $c1$.
        %
       \end{enumerate}
    %
   \end{enumerate}
\end{exercise}

\begin{exercise}
    %
    We will now look at a slightly more generalized version of the |Exp| type from the previous exercise:
    %
    % TODO Override Exp or rename Exp2?
    %
    \begin{code}
    data Exp a  =  Num a
                |  Var String
                |  Exp a  `Plus`   Exp a
                |  Exp a  `Minus`  Exp a
                |  Exp a  `Times`  Exp a
    deriving(Eq, Show)
    \end{code}
    %
    The type has now been parametrized, so that it is no longer limited to representing expressions with integers, but can instead represent expressions with any type. For instance, we could have an |Exp Double| to represent expressions with doubles, or an |Exp ComplexD| to represent expressions with complex numbers.
    %
    \begin{enumerate}
    %
        \item Write the following expressions in Haskell, using the new |Exp| data type.
        \begin{enumerate}
            \item \(a1 = 2.0 + a\)
            \item \(a2 = 5.3 + b * c\)
            \item \(a3 = a(b + c) - (d + e)(f + a)\)
        \end{enumerate}
        %
        % TODO I limited the type to String variable names to keep things simple for the time being. Should I parametrize this instead, so we get |Env v a| instead of just |Env a|?
        %
        \item In order to evaluate these expressions, we will need a way of translating a variable name into the value. The following table shows the value of each variable in the expressions above:

        \begin{table}[h]
        \centering
        \begin{tabular}{ll}
        Name & Value \\
        a    & 1.5   \\
        b    & 4.8   \\
        c    & 2.4   \\
        d    & 7.4   \\
        e    & 5.8   \\
        f    & 1.7
        \end{tabular}
        \end{table}
        %
        In Haskell, we can represent this table using the type |type Env a = [(String, a)]|, which is a list of pairs of variable names and values, where each entry in the list corresponds to a row in the table.
        %
        \begin{enumerate}
            \item Express the table above in Haskell by creating |vars :: Env Double|.
            \item Create a function |varVal :: Env a -> String -> a| that returns the value of a variable, given an |Env| and a variable name. For instance, |varVal vars "d"| should return 7.4
            \item Create a function |eval:: Num a => Env a -> Exp -> a| that takes a value of the new |Exp| data type and returns the corresponding number. For instance, |eval vars ((Num 2) `Plus` (Var "a")) == 3.5|. Try it on the expressions from the first part, and verify that it works as expected.

        \end{enumerate}
    %
    \end{enumerate}
    %
\end{exercise}
%
\begin{exercise}
  Read the full chapter and complete the definition of the
  instance for |Num| for the datatype `ComplexSyn`.
  %
  Also add a constructor for variables to enable writing expressions
  like |(Var "z") :*: toComplex 1|.
\end{exercise}
\begin{exercise}
 Read the next few pages of Appendix I (in
  \citep{adams2010calculus}) defining the polar view of Complex Numbers
  and try to implement complex numbers again, this time based on
  magnitude and phase for the semantics.
\end{exercise}
\begin{exercise}
 Implement a simplifier |simp :: ComplexSyn r -> ComplexSyn r|
  that handles a few cases like |0 * x = 0|, |1 * x = x|, |(a + b) * c
  = a * c + b * c|, \ldots
  %
  What class context do you need to add to the type of |simp|?
\end{exercise}


TODO: prepare for the "tupling transform" by an exercise converting back and forth between

\begin{spec}
  a -> (b, c)
\end{spec}

and

\begin{spec}
  (a->b, a->c)
\end{spec}

TODO: perhaps do it again in the logic exercises


TODO: Perhaps formulate exercise to implement more efficient show
using an ackumulating parameter.
