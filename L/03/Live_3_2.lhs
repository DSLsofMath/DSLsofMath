Week & chapter 3: Types in mathematics
Lecture 3.2 

\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Live_3_2 where
import Text.Printf
\end{code}

Here are some examples of using the function instances:
metoder: add och zero
\begin{code}
class    Additive a        where  add :: a -> a -> a;  zero :: a
class    Multiplicative a  where  mul :: a -> a -> a;  one  :: a
\end{code}
Axioms:   forall x. add zero x == x == add x zero
          forall x. mul one  x == x == mul x one

TODO: Add instances for Double and for functions
\begin{code}
two :: (Additive a, Multiplicative a) => a
two = add one one

instance Additive a => Additive (t -> a) where
  add   = funAdd
  zero  = funZero

funAdd :: Additive a => (t -> a) -> (t -> a) -> (t -> a)
funAdd = error "TODO: funAdd"
funZero :: Additive a => (t -> a)
funZero = error "TODO: funZero"
\end{code}

Explain the set of instances of Additive 
  = TODO

\begin{code}
type Fun = REAL -> REAL
f1, f2 :: Fun
f1 = (^2)
f2 = (2*)
\end{code}

An example class |C|:

  "a collection of types with some common properties / methods"

The class C has the set {Int, Bool, String} as instances
  the only method is foo :: C a =>  a -> a

\begin{code}
class C a where         foo :: a -> a
instance C Int where    foo i = -i
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
