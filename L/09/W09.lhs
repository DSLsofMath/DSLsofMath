




\section{Spaces}


We have four basic
space constructions.

\paragraph{Discrete distributions}
\paragraph{Continuous distributions}

\paragraph{Scaling}

\paragraph{(Dependent) product of distributions}
- New binder. Similar to forall in Ch. 3, but be also give the
 distribution of the bound variable; thus giving a precise meaning to
 "random" variable.



(TODO: explain HOAS somewhere)

We have:

\begin{code}
{-# LANGUAGE GADTs #-}
import Prelude hiding (Real)
import Control.Monad (ap)
import Data.List ((\\))

type Real = Double -- pretend...

data Space a where
  Sigma :: (Space a) -> (a -> Space b) -> Space (a,b)
  Factor :: Real -> Space ()
  Discrete :: [a] -> (a -> Real) -> Space a
  Continuous :: (Real -> Real) -> Space Real

  Bind :: (Space a) -> (a -> Space b) -> Space b
  Project :: a -> Space a

instance Functor Space where
  fmap f = (pure f <*>)
instance Applicative Space where
  pure = Project
  (<*>) = ap
instance Monad Space where
  (>>=) = Bind
\end{code}

we could try to define the probabilities or densities of possible
 values of a space: |spaceDensity :: Space a -> (a -> Real)|, and from
 there define the expected values (or other statistical moments), but
 it's simpler to directly give a generalized version of the
 integration of a function for spaces:

\begin{code}
integrate :: Space a -> (a -> Real) -> Real
integrate (Sigma a f) g = integrate a $ \x -> integrate (f x) $ \y -> g (x,y)
integrate (Factor f) g = f * g ()
integrate (Discrete a p) g = bigsum a $ \x -> p x * g x
integrate (Continuous p) g = integral $ \x -> p x * g x

integrate (Bind a f) g = integrate (Sigma a f) (g . snd)
integrate (Project x) g = g x
\end{code}

(perhaps) familiar distributions can be embedded using the "discrete" and "continuous" constructors:

\begin{code}
uniformDiscrete :: [a] -> Space a
uniformDiscrete xs = Discrete xs (\_ -> 1.0 / fromIntegral (length xs))

bernoulli :: Real -> Space Bool
bernoulli p = Discrete [False,True] (\x -> if x then p else 1-p)


-- sum of some terms (finite so we can compute this)
bigsum :: [a] -> (a -> Real) -> Real
bigsum xs f = foldl (+) 0 [f x | x <- xs]

-- indefinite integral over the whole real line
integral :: (Real -> Real) -> Real
integral = undefined

\end{code}

\section{Random Variables}
- Variables; AGAIN!

  Wikipedia:

  Variable may refer to:
    Variable (computer science), a symbolic name associated with a value and whose associated value may be changed
    Variable (mathematics), a symbol that represents a quantity in a mathematical expression, as used in many sciences


  By now we have a pretty good grip on 1. and we saw in ch. 3. a way
  to reduce mathematical variables (q and v in lagrangian mechanics)
  to computer science variables, by making expressing variables in
  terms of a symbolic, truly free variable (t).

- A random variable is simply an expression whose value is the outcome
   of a particular experiment (Introduction to Probability, Charles
   M. Grinstead and J. Laurie Snell)

Spaces represent the "experiments" that Grinstead and Snell mention


Then, we can define the expected value, and other statistical
moments. The first, most simple quantity that we need is the measure
of the space. Indeed, summing the probabilities should be 1, but a
space can have a different value, and thus we must re-normalise a
space to get a distribution.

To compute the measure of a space, we can simply integrate the constant 1.

\begin{code}
measure :: Space a -> Real
measure d = integrate d (const 1)
\end{code}

A random variable can now be defined as a function from the space of situations. A real-valued random variable is of type 

For example, the expected value of real-valued random variable is then:
\begin{code}
expectedValue :: Space a -> (a -> Real) -> Real
expectedValue d f = integrate d f / measure d
\end{code}

exercise: define the variance; etc.

An event can be defined as a boolean-valued function over a space. If
 the space accurately represents all possible situations with a correct density,
then the expected value of the event is its probability. (However we need to map booleans to reals:)

\begin{code}
indicator :: Num p => Bool -> p
indicator True = 1
indicator False = 0

probability :: Space a -> (a -> Bool) -> Real
probability d f = expectedValue d (indicator . f)
\end{code}

Conditional probability:

\begin{code}
condProb :: Space a -> (a -> Bool) -> (a -> Bool) -> Real
condProb s g f = probability (Sigma s (isTrue . g)) (f . fst)
\end{code}

\begin{code}
dirac :: Real -> Real
dirac = undefined

isTrue :: Bool -> Space ()
isTrue c = Factor (indicator c)

equal :: Real -> Real -> Space ()
equal x y = Factor (dirac (x-y))

probability' :: Space Bool -> Real
probability' d = expectedValue d indicator

-- (maybe) we're making some error in case |measure d| is 0.

\end{code}

Example: drug test (wikipedia; bayes theorem)

Suppose that a test for using a particular drug is 99% sensitive and
99% specific. That is, the test will produce 99% true positive results
for drug users and 99% true negative results for non-drug
users. Suppose that 0.5% of people are users of the drug. What is the
probability that a randomly selected individual with a positive test
is a drug user?



Solution
\begin{code}
drugSpace :: Space (Bool, (Bool, ()))
drugSpace = Sigma (bernoulli 0.005) $ \isUser ->
            Sigma (bernoulli (if isUser then 0.99 else 0.01)) $ \testPositive ->
            isTrue testPositive -- we have ``a positive test'' by assumption

userProb :: Real
userProb = probability drugSpace (\ (isUser,_) -> isUser)

-- >>> solution
-- 0.33221476510067116

space' :: Space Bool
space' = Bind (bernoulli 0.005) $ \isUser ->
         Bind (bernoulli (if isUser then 0.99 else 0.01)) $ \testPositive ->
         Bind (isTrue testPositive) $ \() -> -- we have ``a positive test'' by assumption
         Project isUser

solution' :: Real
solution' = probability' space'

-- >>> solution'
-- 0.33221476510067116

{-
Doing it manually:

measure space
=
integrate (bernoulli 0.005) $ \isUser ->
integrate (bernoulli(if isUser then 0.99 else 0.01)) · $ \testPositive ->
integrate (isTrue testPositive) (constant 1)
=
integrate (bernoulli 0.005) $ \isUser ->
integrate (bernoulli(if isUser then 0.99 else 0.01)) · $ \testPositive ->
integrate (Factor (if testPositive then 1 else 0)) (constant 1)
= 
sum [False,True] $ \isUser -> (if isUser then 0.005 else 9.995) *
sum [False,True] $ \testPositive -> if testPositive then (if isUser then 0.99 else 0.01) else (if isUser then 0.01 lese 0.99) · $ \testPositive ->
(if testPositive then 1 else 0)) * 1
=
sum [False,True] $ \isUser -> (if isUser then 0.005 else 9.995) *
sum [True] $ (if isUser then 0.99 else 0.01) $ \testPositive -> 1
=
sum [False,True] $ \isUser -> (if isUser then 0.005 else 9.995) *
(if isUser then 0.99 else 0.01)
=
0.99 * 0.005  + 0.995 * 0.01
= 
0.0149

Numerator:
integrate (bernoulli 0.005) $ \isUser ->
integrate (bernoulli(if isUser then 0.99 else 0.01)) $ \testPositive ->
integrate (isTrue testPositive) (\ () -> if isUser then 1 else 0)
=
0.005 *
integrate (bernoulli(0.99)) $ \testPositive ->
integrate (isTrue testPositive) (\ _ -> 1)
=
0.005 *
integrate (bernoulli(0.99)) $ \testPositive ->
if testPositive then 1 else 0
=
0.005 *
0.99 *
1
=
0.00495

-}

\end{code}

Suppose you’re on Monty Hall’s \emph{Let’s Make a Deal!} You are given
 the choice of three doors, behind one door is a car, the others,
 goats. You pick a door, say 1, Monty opens another door, say 3, which
 has a goat. Monty says to you “Do you want to pick door 2?” Is it to
 your advantage to switch your choice of doors?

(Remember that, at the time of writing, cars were a highly prized
 item.)

\begin{code}
montySpace1 :: Bool -> Space Bool
montySpace1 changing = do
  winningDoor <- uniformDiscrete [1::Int,2,3] -- any door can be the winning one
  let montyPickedDoor = 3
  isTrue (montyPickedDoor /= winningDoor)
  let newPickedDoor = if changing then 2 else 1
  return (newPickedDoor == winningDoor)

-- >>> probability' (montySpace1 False)
-- 0.5

-- >>> probability' (montySpace True)
-- 0.6666666666666666
\end{code}

\begin{code}
montySpace :: Bool -> Space Bool
montySpace changing = do
  winningDoor <- uniformDiscrete [1::Int,2,3] -- any door can be the winning one
  pickedDoor <-  uniformDiscrete [1,2,3] -- player picks door blindly
  montyPickedDoor <-
    uniformDiscrete ([1,2,3] \\ [pickedDoor, winningDoor]) -- monty cannot pick the same door, nor a winning door.
  let newPickedDoor =
        if changing
        then head ([1,2,3] \\ [pickedDoor, montyPickedDoor])
        -- player takes a door which is NOT the previously picked, nor the showed door.
        else pickedDoor
  return (newPickedDoor == winningDoor)

-- >>> probability' (montySpace False)
-- 0.3333333333333333

-- >>> probability' (montySpace True)
-- 0.6666666666666666
\end{code}



% Local Variables:
% dante-methods : (bare-ghci)
% mode: literate-haskell
% End:
