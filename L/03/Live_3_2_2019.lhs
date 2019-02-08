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
  operator sections (^2), (2*), etc.
  reminder that D : F1 -> F1 cannot be computed, we need syntax

Partial derivative (typing).

Lagrangian case study
+ infer types from mathematical text + expressions
+ system state
+ path, path in state space

\begin{code}
{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Live_3_2 where
-- import DSLsofMath.FunNumInst
import Text.Printf
\end{code}

----
Now onto new L3.2 material: type classes, Num (x->a),
derivatives, FunExp

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

  commuting diagram with |eval|, |derive| and |D|

  equational reasoning to calculate definitions of |derive|

\begin{code}

instance Num a => Num (x -> a) where
  (+) = oplus
  (*) = otimes
  negate = onegate
  fromInteger  = ofI

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
instance C String where foo s = reverse s
-- instance Num a => C a where foo x = 4
testC = foo (3::Int)
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
instance Num Bool where
  (+) = (/=)
  (*) = (&&)
  fromInteger n = if n==0 then False else True
instance Fractional Bool where
  recip = not
instance Floating Bool where  -- (Don't do this at home;-)
  pi = True
  sin x = False
  cos x = True
\end{code}

But, there are usually laws associated with the class,
and you (the instance author) is responsible for
making sure that these laws are satisfied. (See next week.)

After the break:
\begin{code}
data FunExp where -- a new DSL, now for functions
  Id  :: FunExp
  Con :: REAL -> FunExp
  Add :: FunExp -> FunExp -> FunExp
  Mul :: FunExp -> FunExp -> FunExp
 deriving Show

-- oplus :: Sem -> Sem -> Sem

type Sem = REAL -> REAL
eval :: FunExp -> Sem
eval Id          = id                  -- \x -> x
eval (Con c)     = const c             -- \x -> c
eval (Add fe ge) = eval fe  +  eval ge -- oplus (eval fe) (eval ge)
eval (Mul fe ge) = eval fe  *  eval ge
\end{code}

Equational reasoning example:

We want to calculate the definition of |derive| from our
knowledge of |D|. Let's pick the simple case of |Add fe ge|
for |fe :: FunExp| and |ge :: FunExp| and try to find out
what constraints need to be satisfied.

  eval (derive (Add fe ge))
= {- (f . g) x = f (g x), where f=eval, g=derive, x=Add fe ge -}
  (eval . derive) (Add fe ge)
= {- specification of derive -}
  (D . eval) (Add fe ge)
= {- (f . g) x = f (g x), where f=D, g=eval, x=Add fe ge -}
  D (eval (Add fe ge))
= {- By. def. of eval -}
  D (eval fe  +  eval ge)
= {- By def. of D for addition: D (f+g) = (D f) + (D g) -}
  D (eval fe) + D (eval ge)
= {- Spec. of derive, twice, in combination with def. of |(.)| -}
  eval (derive fe) + eval (derive ge)
= {- Def. of eval for Add ("backwards") -}
  eval (Add (derive fe) (derive ge))

To sum up:
    eval (derive (Add fe ge))
  =
    eval (Add (derive fe) (derive ge))

which is implied by

    derive (Add fe ge) = Add (derive fe) (derive ge)

Thus we can add that as a definition for |derive|.

\begin{code}
derive :: FunExp -> FunExp
derive (Add f g) = Add (derive f) (derive g)
derive (Id)      = Con 1
derive (Con c)   = Con 0
derive (Mul f g) = Add (Mul (derive f) g) (Mul f (derive g))
   -- (fg)' == f'g + fg'

-- Just testing:
x = Id
hello = Mul x (Mul x x) -- x^3

instance Num FunExp where
  (+) = Add
  (*) = Mul
  fromInteger = Con . fromInteger

test1, test2, test3 :: FunExp
test1 = x^3

test2 = derive (x^3)
test3 = derive (derive (derive (x^3)))

main = print (eval test3 2 == 6)
\end{code}
Note that the the result needs simplification to be readable.

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
