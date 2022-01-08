\begin{code}
data Exp  =  Con Integer
          |  Exp  `Plus`   Exp
          |  Exp  `Minus`  Exp
          |  Exp  `Times`  Exp
  deriving(Eq, Show)
\end{code}

1a: a1 = 2 + 2

\begin{code}
two :: Exp
two = Con 2
a1 :: Exp
a1 = Plus two two
\end{code}

1b: a2 = a1 + 7 * 9

\begin{code}
a2 :: Exp
a2 = a1 `Plus` (Con 7 `Times` Con 9)
\end{code}

1c: a3 = 8*(2 + 11) - (3 + 7)*(a1 + a2)

Here is one way to shorten the notation (but be careful with the
parentheses):
\begin{code}
a3 = (c 8 * (c 2 + c 11)) - ((c 3 + c 7)*(a1 + a2))
  where  c = Con
         (+) = Plus
         (-) = Minus
         (*) = Times
\end{code}

2. Create a function |eval :: Exp -> Integer| ... for instance,
   |eval ((Con 3) `Plus` (Con 3)) == 6|
   Try it on the expressions from the first part, and verify that it works as expected.

\begin{code}
eval :: Exp -> Integer
eval (Con i)        =  evalCon i
eval (Plus   a  b)  =  evalPlus   (eval a) (eval b)
eval (Minus  a  b)  =  evalMinus  (eval a) (eval b)
eval (Times  a  b)  =  evalTimes  (eval a) (eval b)

evalCon :: Integer -> Integer
evalCon i = i
-- evalCon    = id

evalPlus, evalMinus, evalTimes :: Integer -> Integer -> Integer
evalPlus   = (+)
evalMinus  = (-)
evalTimes  = (*)

eval' :: Exp -> Integer
eval' (Con i)        =  i
eval' (Plus   a  b)  =  eval' a  +  eval' b
eval' (Minus  a  b)  =  eval' a  -  eval' b
eval' (Times  a  b)  =  eval' a  *  eval' b


test0 =  eval ((Con 3) `Plus` (Con 3)) == 6
test1 =  eval a1 == 4
test2 =  eval a2 == eval a1 + 7 * 9
test3 =  eval a3 == 8*(2 + 11) - (3 + 7)*(eval a1 + eval a2)
\end{code}

3. Consider the following expression:
     c1 = (x - 15)*(y + 12)*z
       where  x = 5
              y = 8
              z = 13

3a. Update the |Exp| data type with a new constructor |Var String|
    that allows variables with strings as names to be represented. Use
    the updated |Exp| to write an expression for |c1| in Haskell.

(In the solution I make a new datatype to avoid name clashes.)
\begin{code}
data E  =  C Integer | P E E | M E E | T E E | V String deriving (Eq, Show)

c1 :: E
c1 = (V "x" - C 15)*(V "y" + C 12)*V "z"
  where (+)=P; (-)=M; (*)=T;
\end{code}

3b. Create a function |varVal :: String -> Integer| that takes a
    variable name, and returns the value of that variable. For now,
    the function just needs to be defined for the variables in the
    expression above, i.e. |varVal "x"| should return 5, |varVal "y"|
    should return 8, and |varVal "x"| should return 13.

\begin{code}
varVal :: String -> Integer
varVal "x" = 5
varVal "y" = 8
varVal "z" = 13
\end{code}

3c. Update the |eval| function so that it supports the new |Var|
    constructor, and use it get a numeric value of the expression
    $c1$.

\begin{code}
evalE :: (String -> Integer) -> E -> Integer
evalE f = ev where
  ev (C i) = i
  ev (P a b) = ev a  +  ev b
  ev (M a b) = ev a  -  ev b
  ev (T a b) = ev a  *  ev b
  ev (V v) = f v

test4 =  evalE varVal c1 == c1Sem

c1Sem :: Integer
c1Sem = (x - 15)*(y + 12)*z
  where  x = 5
         y = 8
         z = 13
\end{code}
