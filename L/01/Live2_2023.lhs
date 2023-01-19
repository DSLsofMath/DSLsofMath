This file is from the "live coding" session at the end of L1.2
(study week 1, lecture 2, of the DSLsofMath course, 2023).

It is all from Chapter 1 in the book
  Domain-Specific Languages of Mathematics
  https://www.collegepublications.co.uk/computing/?00024
More specifically it covers
  Section 1.4 (A DSL of complex numbers)
  Section 1.5 (A syntax for (complex) arithmetical expressions)
and some of
  Section 1.2 (Types in Haskell: |type|, |newtype|, and |data|)

{- iimage-mode for inlining the quotes -}

\begin{code}
{-# LANGUAGE GADTs #-}
module Live2_2023 where
type REAL = Double
\end{code}

A datatype for "semantic complex numbers" as defined on page A.2 in
the book [Adams and Essex, Calculus, 2010].

Quote 1:
file://Adams_Complex_01.png

\begin{code}
data ImagUnit -- TODO
i :: ImagUnit
i = error "TODO"
\end{code}
Quote 2:
file://Adams_Complex_02.png
\begin{code}
data CA -- TODO
        
data CB -- TODO GADT syntax

showCB :: CB -> String
showCB = error "TODO"
\end{code}
Quote 3:
file://Adams_Complex_03.png
\begin{code}
e1, e2, e3, e4 :: CB
e1 = error "TODO: e1"
e2 = error "TODO: e2"
e3 = error "TODO: e3"
e4 = error "TODO: e4"
\end{code}
Quote 4:
file://Adams_Complex_04.png
\begin{code}
data CC -- TODO

equalityCheck :: CC -> CC -> Bool
equalityCheck = error "TODO"
\end{code}
Quote 5: 
file://Adams_Complex_05.png

Quote 6:
file://Adams_Complex_06.png
We define the functions |re| and |im| using pattern-matching.
\begin{code}
re, im :: CC -> REAL
re = error "TODO: re"
im = error "TODO: im"
\end{code}
An example of the use of pattern-matching:
Quote 7:
file://Adams_Complex_07.png
\begin{code}
      -- w     z     result
addCC :: CC -> CC -> CC
addCC = error "TODO: addCC"
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
So far we have defined "semantic complex numbers" (in cartesian form).
Now we turn to "syntactic complex number expressions" to define a DSL.
Three parts:
+ an abstract syntax type CE,
+ a  semantic type CC,
+ an evaluator eval :: CE -> CC
\begin{code}
data CE -- TODO syntax trees for complex number expressions

-- "wishful thinking" -- call eval recursively on subtrees
eval :: CE -> CC   -- x, y :: CE
eval = error "TODO: eval"

-- semantic "constructors"
-- addCC  :: CC -> CC -> CC  -- defined above
mulCC   :: CC -> CC -> CC
mulCC = error "TODO: mulCC"

rconCC  :: REAL -> CC
rconCC = error "TODO: rconCC"

iCC     :: CC
iCC = error "TODO: iCC"

e5 :: CE
e5 = error "TODO: e5" -- 1+i
\end{code}


exercise: implement mulCC

Add    :: CE -> CE -> CE   -- syntactic constructor
addCC  :: CC -> CC -> CC   -- semantic "smart constructor"

----------------------------------------------------------------

Types in Haskell
"data" for syntax trees
\begin{code}
-- pure enumeration types

-- Parameterised types: optional values, sum types
data Option a -- TODO
\end{code}
