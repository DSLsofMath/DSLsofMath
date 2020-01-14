\begin{exercise}
  Consider the following data type for simple arithmetic expressions:
  %
\begin{code}
data Exp  =  Con Integer
          |  Exp  `Plus`   Exp
          |  Exp  `Minus`  Exp
          |  Exp  `Times`  Exp
  deriving (Eq, Show)
\end{code}
  %
  Note the use of ``backticks'' around |Plus| etc. which makes it
  possible to use a name as an infix operator.
  %
  \begin{enumerate}
   %
  \item Write the following expressions in Haskell, using the |Exp| data type:
  \begin{enumerate}
  \item |a1 = 2 + 2|
  \item |a2 = a1 + 7 * 9|
  \item |a3 = 8*(2 + 11) - (3 + 7)*(a1 + a2)|
  \end{enumerate}
    %
    \item Create a function |eval :: Exp -> Integer| that takes a value of the |Exp| data type
       and returns the corresponding number (for instance, |eval ((Con 3) `Plus` (Con 3)) == 6|).
       Try it on the expressions from the first part, and verify that it works as expected.
    %
    \item Consider the following expression:
       \begin{spec}
         c1 = (x - 15)*(y + 12)*z
          where  x = 5; y = 8; z = 13
       \end{spec}

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
        |varVal "z"| should return 13.
        %
        \item Update the |eval| function so that it supports the new |Var|
        constructor, and use it get a numeric value of the expression $c1$.
        %
       \end{enumerate}
    %
   \end{enumerate}
\end{exercise}
