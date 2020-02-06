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
** (derivatives, FunExp)

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

\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Live_3_2 where
-- import DSLsofMath.FunNumInst
import Text.Printf
\end{code}

Computing derivatives

  Rules: note that they require the syntax for practical use.

  data SynF  -- a new DSL, now for functions

  |eval : SynF -> SemF|

  commuting diagram with |eval|, |derive| and |D|

                    eval
            SynF ---------> SemF
             |               |
             |               |
      derive |               | D
             |               |
             v               v
            SynF ---------> SemF
                    eval

   or equivalently:

     eval . derive == D . eval

  equational reasoning to calculate definitions of |derive|

\begin{code}
data SynF = Kvadrera | Id | Add SynF SynF | Mul SynF SynF | Con REAL
  deriving Show
type SemF = REAL -> REAL
derive :: SynF -> SynF
derive (Con c)   = Con 0
derive Kvadrera  = tvåGånger
derive (Add f g) = Add (derive f) (derive g)
derive (Id)      = Con 1
derive (Mul f g) = Add (Mul (derive f) g) (Mul f (derive g))
   -- (fg)' == f'g + fg'

tvåGånger :: SynF
tvåGånger = Mul (Con 2) Id

eval :: SynF -> SemF
eval (Con c)    = \x -> c
eval Id         = id      -- \x -> x
eval Kvadrera   = (^2)
eval (Add f g)  = oadd (eval f) (eval g)
eval (Mul f g)  = omul (eval f) (eval g)

oadd f g = \x -> f x + g x
omul f g = \x -> f x * g x

f1, f2, f3 :: SynF
f1 = Kvadrera          -- x²
f2 = Add (Con 2) f1    -- 2 + x²
f3 = Mul Id Id         -- x*x

-- (+), (*), negate, fromInteger, signum, abs,
instance Num a => Num (x -> a) where
  (+) = oplus  -- \f g x -> f x + g x
  (*) = otimes
  negate = onegate
  fromInteger  = ofI

hoj, haj :: Bool -> Int
hoj b = if b then 17 else 38
haj b = if b then  1 else  0


oplus :: Num a => (x -> a) -> (x -> a) -> (x -> a)
oplus f g = \x -> f x + g x

otimes :: Num a => (x -> a) -> (x -> a) -> (x -> a)
otimes f g = \x -> f x * g x

onegate :: Num a => (x -> a) -> (x -> a)
onegate f = negate . f

ofI :: Num a => Integer -> (x -> a)
ofI i = \x -> fromInteger i   -- i :: Integer

instance Fractional a => Fractional (x -> a) where
  recip  f         =  recip . f
  fromRational     =  ofR

ofR :: Fractional a => Rational -> (x -> a)
ofR r = \x -> fromRational r

instance Floating a => Floating (x -> a) where
  pi       =  const pi
  exp f    =  exp . f
  f ** g   =  \ x -> (f x)**(g x)
  cos f    =  cos . f
  -- and so on
\end{code}

Here are some examples of using the |Num (x->a)| instance

\begin{code}
test0 :: REAL -> REAL
test0 = (\x-> 5*sin (2*x))  +
        (\x -> x^2)
\end{code}

An example class |C|:
\begin{code}
class C a where         foo :: a -> a
instance C Int where    foo i = 1+i
instance C Bool where   foo b = not b
instance C String where foo s = reverse s
testC = foo (3::Int)
\end{code}

Equational reasoning example:

We want to calculate the definition of |derive| from our
knowledge of |D|. Let's pick the simple case of |Add fe ge|
for |fe :: SynF| and |ge :: SynF| and try to find out
what constraints need to be satisfied.

  eval (derive (Add fe ge))
= {- Def. of (.) -}
  (eval . derive) (Add fe ge)
= {- Use the spec.: eval . derive == D . eval -}
  (D . eval) (Add fe ge)
= {- Def. of (.) -}
  D (eval (Add fe ge))
= {- def. eval -}
  D (oplus (eval fe) (eval ge))      -- D (f+g),   f = eval fe, ...
= {- D is linear -}
  oplus (D (eval fe)) (D (eval ge))  -- (D f) + (D g)
= {- Def. of (.), twice -}
  oplus ((D . eval) fe) ((D . eval) ge)
= {- Use the spec.: eval . derive == D . eval -}
  oplus ((eval . derive) fe) ((eval . derive) ge)
= {- Def. of (.), twice -}
  oplus (eval (derive fe)) (eval (derive ge))
-- We want an expression for derive (Add fe ge)
  let f = derive fe
      g = derive ge
  in oplus (eval f) (eval g)
= {- eval (Add f g)  = oplus (eval f) (eval g) -}
  let f = derive fe
      g = derive ge
  in eval (Add f g)
= -- expand let
  eval (Add (derive fe) (derive ge))

Summera resultatet:

  eval (derive (Add fe ge))
= (for all fe, ge :: SynF)
  eval (Add (derive fe) (derive ge))

Lets define:

derive (Add fe ge) = Add (derive fe) (derive ge)

\begin{code}

-- Just testing:
x = Id
hello = Mul x (Mul x x) -- x^3

test2 = derive hello
test3 = derive (derive (derive hello))

main = print (eval test3 2 == 6)
\end{code}
Note that the result needs simplification to be readable.

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
