\begin{code}
data E2 a  =  Con a
           |  Var String
           |  E2 a  `Plus`   E2 a
           |  E2 a  `Minus`  E2 a
           |  E2 a  `Times`  E2 a
  deriving (Eq, Show)
\end{code}

1. Write the following expressions in Haskell, using the new |E2| data type.

1a. |a1 = 2.0 + a|

\begin{code}
a1 :: E2 Double
a1 = Con 2.0 `Plus` Var "a"
\end{code}

1b. |a2 = 5.3 + b * c|

\begin{code}
a2 :: E2 Double
a2 = Con 5.3 `Plus` (Var "b" `Times` Var "c")
\end{code}

1c. |a3 = a*(b + c) - (d + e)*(f + a)|

Be careful with the parentheses: without the extra parentheses below
the syntax tree is different. This is because the local definitions of
the arithmetic operators do not have the right precedences.

\begin{code}
a3 :: E2 a
a3 = (a*(b + c)) - ((d + e)*(f + a))
  where  [a,b,c,d,e,f] = map (\c -> Var [c]) "abcdef"
         (+) = Plus
         (-) = Minus
         (*) = Times
\end{code}

2. Lookup tables

  Name  &  a   & b   & c   & d   & e   & f
  Value &  1.5 & 4.8 & 2.4 & 7.4 & 5.8 & 1.7

 In Haskell, we can represent this table using a value of type |Table a = Env
 String a = [(String, a)]|, which is a list of pairs of variable names
 and values, where each entry in the list corresponds to a column in the
 table.

\begin{code}
type Table a = Env String a
type Env v a = [(v, a)]
\end{code}

(Note that the table represents a partial function. It may fail if
used on variable names (keys) not in the list.)

2a.

Here is one way of filling the table |vars|:
\begin{code}
vars :: Table Double
vars = zip (map (\c->[c]) "abcdef")
           [1.5, 4.8, 2.4, 7.4, 5.8, 1.7]
\end{code}

2b.

Here is an implementation of a partial lookup-function for our
variable tables:

\begin{code}
varVal :: Table a -> String -> a
varVal []           _   = error "varVal: variable not found"
varVal ((v,x):vxs)  v'
       | v == v'        = x
       | otherwise      = varVal vxs v'
\end{code}

2c.

\begin{code}
eval :: Num a => Table a -> E2 a -> a
eval env = e where
  e (Con c) = c
  e (Var v) = varVal env v
  e (Plus   x  y) = e x  +  e y
  e (Minus  x  y) = e x  -  e y
  e (Times  x  y) = e x  *  e y

test0 :: Bool
test0 =  eval vars ((Con 2) `Plus` (Var "a")) == 3.5
\end{code}

Try it on the expressions from the first part, and verify that it
works as expected.

\begin{code}
evalV :: E2 Double -> Double
evalV = eval vars

test1, test2, test3 :: Bool
test1 = evalV a1 == 2.0 + evalV (Var "a")
test2 = evalV a2 == 5.3 + evalV (Var "b") * evalV (Var "c")
test3 = evalV a3 == a*(b + c) - (d + e)*(f + a)
  where [a,b,c,d,e,f] = map (\c -> evalV (Var [c])) "abcdef"
\end{code}
