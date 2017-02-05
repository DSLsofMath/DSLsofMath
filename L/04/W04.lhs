% Week 4-5: Poly, PowerSeries, ...

\section{Week 4}
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module DSLsofMath.W04 where
\end{code}

% (Based on ../../2016/Lectures/Lecture06  )

\subsection{A simpler example of a non-compositional function}

Consider a very simple datatype of integer expressions:
%
\begin{code}
data E = Add E E | Mul E E | Con Integer deriving Eq
e1, e2 :: E
e1 = Add (Con 1) (Mul (Con 2) (Con 3))
e2 = Mul (Add (Con 1) (Con 2)) (Con 3)
\end{code}
%
When working with expressions it is often useful to have a
``pretty-printer'' to convert the abstract syntax trees to strings
like |"1+2*3"|.
%
\begin{code}
pretty :: E -> String
\end{code}
%
We can view |pretty| as an alternative |eval| for a semantics using
|String| as the semantic domain instead of the more natural |Integer|.
%
We can implement |pretty| in the usual way as a ``fold'' over the
syntax tree using one ``semantic constructor'' for each syntact
constructor:
%
\begin{code}
pretty (Add x y)  = prettyAdd (pretty x) (pretty y)
pretty (Mul x y)  = prettyMul (pretty x) (pretty y)
pretty (Con c)    = prettyCon c

prettyAdd :: String -> String -> String
prettyMul :: String -> String -> String
prettyCon :: Integer -> String
\end{code}
%
Now, if we try to implement the semantic constructors without thinking
too much we would get the following:
\begin{code}
prettyAdd sx sy  = sx ++ "+" ++ sy
prettyMul sx sy  = sx ++ "*" ++ sy
prettyCon i      = show i

p1, p2 :: String
p1 = pretty e1
p2 = pretty e2

trouble :: Bool
trouble = p1 == p2
\end{code}
%
Note that both |e1| and |e2| are different but they pretty-print to
the same string.
%
There are many ways to fix this, some more ``pretty'' than others, but
the main problem is that some information is lost in the translation.
%

TODO(perhaps): Explain using two pretty printers: for a sum of terms,
for a product of factors, ... then combine them with the tupling
transform just as with |evalD|.

Exercise: One way to make this example go through is to refine the
semantic domain from |String| to |Precedence -> String|.

\subsection{}

% Compositionality:
% * simpler example of non-compositional function [See L9]
% * then back to |eval'|
% * and show |evalD|
% * general pattern: tupling
%
----

Shallow and deep embeddings - based on ../../2016/Lectures/07/

----

Lecture 9: Algebraic Structures and DSLs
based on ../../2016/Lectures/Lecture09.lhs


\section{Some helper functions}

\begin{code}
foldE ::  (t -> t -> t) -> (t -> t -> t) -> (Integer -> t) ->
          E -> t
foldE add mul con = rec
  where  rec (Add x y)  = add (rec x) (rec y)
         rec (Mul x y)  = mul (rec x) (rec y)
         rec (Con i)    = con i

evalEI :: E -> Integer
evalEI = foldE (+) (*) id

evalEN :: Num a => E -> a
evalEN = foldE (+) (*) fromInteger

instance Num E where -- Some abuse of notation
  (+) = Add
  (*) = Mul
  fromInteger = Con
\end{code}