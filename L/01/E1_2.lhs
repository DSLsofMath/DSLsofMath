\begin{exercise}
%
We will now look at a more general version of the |Exp| type from the
previous exercise:
%
\begin{code}
data E2 a  =  Con a
           |  Var String
           |  Plus  (E2 a) (E2 a)
           |  Minus (E2 a) (E2 a)
           |  Times (E2 a) (E2 a)
  deriving(Eq, Show)
\end{code}
%
The type has now been parametrized, so that it is no longer limited to
representing expressions with integers, but can instead represent
expressions with any type.
%
For instance, we could have an |E2 Double| to represent expression
trees with doubles at the leaves, or an |E2 ComplexD| to represent
expression trees with complex numbers at the leaves.
%
\begin{enumerate}
%
\item Write the following expressions in Haskell, using the new |E2| data type.
  \begin{enumerate}
  \item |a1 = 2.0 + a|
  \item |a2 = 5.3 + b * c|
  \item |a3 = a*(b + c) - (d + e)*(f + a)|
  \end{enumerate}
  %
\item In order to evaluate these expressions, we will need a way of
  translating a variable name into the value.
  %
  The following table shows the value of each variable in the
  expressions above:

  \begin{table}[h]
    \centering
    \begin{tabular}{lrrrrrr}
      Name  &  |a|   & |b|   & |c|   & |d|   & |e|   & |f|   \\
      Value &  |1.5| & |4.8| & |2.4| & |7.4| & |5.8| & |1.7| \\
    \end{tabular}
  \end{table}
  %
\index{Env@@|Env| (environment type)}
  %
  In Haskell, we can represent this table using a value of type |Table
  a = Env String a = [(String, a)]|, which is a list of pairs of
  variable names and values, where each entry in the list corresponds
  to a column in the table.
  %**TODO: remind about (partial) functions
  \begin{enumerate}
  \item Express the table above in Haskell by creating |vars :: Table Double|.
  \item Create a function |varVal :: Table a -> String -> a|
    that returns the value of a variable, given a table and a
    variable name.
    %
    For instance, |varVal vars "d"| should return 7.4
  \item Create a function |eval :: Num a => Table a -> E2 a ->
    a| that takes a value of the new |E2| data type and returns
    the corresponding number.
    %
    For instance, |eval vars (Plus (Con 2) (Var "a")) == 3.5|.
    %
    Try it on the expressions from the first part, and verify that it
    works as expected.
  \end{enumerate}
\end{enumerate}
\end{exercise}
