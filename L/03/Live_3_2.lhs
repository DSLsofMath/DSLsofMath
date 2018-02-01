Week & chapter 3: Types in mathematics

Learning outcomes

* Knowledge and understanding
** L3.1: organize areas of mathematics in DSL terms
** L3.1: explain main concepts of elementary real analysis
** L3: design and implement a DSL for derivatives

* Skills and abilities
** L3.1: develop adequate notation for mathematical concepts
** L3.2: perform calculational proofs

* Judgement and approach
** A1: discuss and compare different software implementations of mathematical concepts

----------------

Reminder of Lecture 3.1:

Examples of type signatures in mathematics, and the lack of types for
higher-order operators.

Derivative of a function f : F1 = REAL -> REAL
  use of anonymous function, x fixed, h varies
  sections
  reminder that D : F1 -> F1 cannot be computed, we need syntax

(Partial derivative of |f : F2 = (REAL × REAL) -> REAL|

Lagrangian case study
  infer types from mathematical text + expressions
  system state
  path, path in state space

\begin{code}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Live_3_2 where
import DSLsofMath.FunNumInst
import Text.Printf
\end{code}

----
Now onto new L3.2 material: type classes, Num (x->a), derivatives, FunExp

Type classes

  implicit type casts
  collect operations of the same shape

  Num class in more detail

  instance Num a => Num (x->a)

Overloaded integer literals

class Fractional

class Floating

function instaces for Fractional and Floating

General type classes in Haskell [see code below]

Computing derivatives

  Rules: note that they require the syntax for practical use.

  data FunExp  -- a new DSL, now for functions

  |eval : FunExp -> F1|

  commuting diagram with |eval|

  equational reasoning to derive definitions of |derive|

TODO: left for the start of week 4

Shallow embeddings:
  eval'
  evalD

Here are some examples of using the |Num (x->a)| instance

\begin{code}
test0 :: REAL -> REAL
test0 = (\x-> 5*sin (5*x)) + (\x -> x^2)

hej :: Bool -> Int
hej True  = 5
hej False = 7

test :: Bool -> Int
test = hej + const 2
\end{code}

An example class |C|:
\begin{code}
class C a where         foo :: a -> a
instance C Int where    foo i = 1+i
instance C Bool where   foo b = not b

test1 = foo (3::Int)
\end{code}

Another class example:
\begin{code}
class Stringify a where  str :: a -> String
instance Stringify Char where
  str c | c `notElem` " aouåeiyäö" = c:'o':c:""
        | otherwise            = c:""

instance Stringify a => Stringify [a] where
  str = concatMap str

instance Stringify Double where
  str d = show d
\end{code}

Question: can anyone redefine pi to whatever they like??

Answer: Yes, if you add a new instance declaration.
Example:

\begin{code}
instance Fractional Bool where
  recip = not
instance Floating Bool where  -- (Don't do this at home;-)
  pi = False
  sin x = True
\end{code}

But, there are usually laws associated with the class,
and you (the instance author) is responsible for
making sure that these laws are satisfied.

We will talk more about this, in example in this style:
\begin{code}
class Monoid a where
  oper :: a -> a -> a
  unit :: a
  -- forall x. oper unit x == x
  -- forall x. oper x unit == x
\end{code}

For reference: the Haskell Prelude numerical classes:

\begin{spec}
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a

class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a

class Fractional a => Floating a where
  pi :: a
  exp :: a -> a
  log :: a -> a
  sqrt :: a -> a
  (**) :: a -> a -> a
  logBase :: a -> a -> a
  sin :: a -> a
  cos :: a -> a
  tan :: a -> a
  asin :: a -> a
  acos :: a -> a
  atan :: a -> a
  sinh :: a -> a
  cosh :: a -> a
  tanh :: a -> a
  asinh :: a -> a
  acosh :: a -> a
  atanh :: a -> a
\end{spec}

----


----------------------------------------------------------------

Some unicode examples in emacs:
* describe-char with cursor on a character gives information about it
* C-x 8 * x gives ×
* also C-x 8 RET MULTIPLICATION SIGN  -- or any other unicode code point name
* C-x 8 RET 3b4 gives δ
* C-x 8 RET 3b5 gives ε

----------------

Just for fun, some ascii plotting of functions.

\begin{code}
plot = putStrLn . plot' 0 (2*pi) 30

type REAL = Double
plot' :: REAL -> REAL -> Int -> (REAL -> REAL) -> String
plot' start stop n f = unlines (infoline:
                                map oneline ys)
  where  width  = stop - start
         xstep  = width / fromIntegral n
         xs = [start, start+xstep .. stop]
         ys = map f xs
         top = maximum ys
         bot = minimum ys
         infoline = "["++printf "%.2f" bot++","++printf "%.2f" top++"]"
         height = top - bot
         ystep  = height / resolution
         resolution = 50
         oneline :: REAL -> String
         oneline y = replicate (dis y) ' ' ++ "*"
           where dis = discretize bot ystep

discretize :: REAL -> REAL -> REAL -> Int
discretize bot step y = round ((y-bot)/step)


-- test1 = plot (sin)
-- test2 = plot (sin^2)
-- test3 = plot (\x->signum(x-pi))
-- test4 = plot (\x->abs(x-pi))
-- test5 = plot (sin + sin^2/2)
\end{code}
