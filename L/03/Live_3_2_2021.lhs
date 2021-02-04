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

* Week 3, Lecture 2:

** Lagrangian case study
+ infer types from mathematical text + expressions
+ system state
+ path, path in state space
** Type classes
** instance Num (x->a)

Type classes

  implicit type casts
  collect operations of the same shape

  Numeric classes in more detail

  function instaces for numeric classes

  General type classes in Haskell [see code below]

\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Live_3_2 where
-- import DSLsofMath.FunNumInst
import Text.Printf
\end{code}

\begin{code}
\end{code}

Here are some examples of using the function instances:
metoder: add och zero
\begin{code}
class    Additive a      where add :: a -> a -> a; zero :: a   -- (egentligen Monoid)
  -- forall x. add zero x == x == add x zero
instance Additive Int    where add = (+);          zero = 0
instance Additive Double where add = (+);          zero = 0

class    Multiplicative a       where
  mul :: a -> a -> a
  one :: a
instance Multiplicative Int     where
  mul = (*)
  one  = 1
instance Multiplicative Double  where
  mul = (*);
  one  = 1
  -- forall x. mul one x == x == mul x one

two :: (Additive a, Multiplicative a) => a
two = add one one

instance Additive a => Additive (t -> a) where
  add = oadd
  zero = const zero

-- add f1 zero 1 == oadd f1 id 1 == add (f1 1) (id 1)
--    == add (1^2) 1 = 1+1 = 2 /= 1^2

type Fun = REAL -> REAL
-- oadd :: Fun -> Fun -> Fun
oadd :: Additive a => (t -> a) -> (t -> a) -> (t -> a)
oadd f g = \ x -> add (f x) (g x)
\end{code}

Mängden Additive = {Int, Double, t->Int, t->Double,
                    t1 -> t2 -> Int, t1 -> t2 -> Double,
                    , ...}

\begin{code}
f1, f2 :: Fun
f1 = (^2)
f2 = (2*)

f3, f4 :: String -> Int
f3 "Patrik" = 1738
f3 "x"      = 7
f3 _        = 1

f4 "x" = 2
f4 "y" = 3
f4 _   = 0


\end{code}


add :: Additive a => a -> a -> a
  -- nu är mångden Additive helt tom!

An example class |C|:

  "en mängd av typer med vissa gemensamma egenskaper"

klassen C är mängden {Int, Bool, String}
  egenskapen är att ha en implementation av foo :: a -> a
\begin{code}
class C a where         foo :: a -> a
instance C Int where    foo i = 1+i
instance C Bool where   foo b = not b
instance C String where foo s = reverse s
testC1 = foo (3::Int)
testC2 = foo False
testC3 = foo "Patrik"

\end{code}

































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
plot' start stop n f = unlines (infolines ++
                                map oneline ys)
  where  width  = stop - start
         xstep  = width / fromIntegral n
         xs = [start, start+xstep .. stop]
         ys = map f xs
         bot = minimum ys
         top = maximum ys
         infolines = [ printf "domain: [%.2f,%.2f]" start stop
                     , printf "range:  [%.2f,%.2f]" bot top
                     ]
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
