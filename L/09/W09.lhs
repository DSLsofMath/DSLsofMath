\begin{code}
{-# LANGUAGE GADTs #-}
import Prelude hiding (Real)
import Control.Monad (ap)
import Data.List ((\\))

type Real = Double -- pretend...
\end{code}

Define a DSL to describe, reason about, problems such as the
following (sometimes compute the probabilites involved)

\begin{enumerate}
\item Assume you throw two 6-face dice, what is the probability that
  the product is greater than 10 if their sum is greater than 7.
\label{ex:dice}
\item Suppose that a test for using a particular drug is 99\% sensitive and
99\% specific. That is, the test will produce 99\% true positive results
for drug users and 99\% true negative results for non-drug
users. Suppose that 0.5\% of people are users of the drug. What is the
probability that a randomly selected individual with a positive test
is a drug user?
\footnote(found in Wikipedia article on bayes theorem)
\label{ex:drugtest}

\item Suppose you’re on Monty Hall’s \emph{Let’s Make a Deal!} You are given
 the choice of three doors, behind one door is a car, the others,
 goats. You pick a door, say 1, Monty opens another door, say 3, which
 has a goat. Monty says to you “Do you want to pick door 2?” Is it to
 your advantage to switch your choice of doors?
% (Remember that, at the time of writing, cars were a highly prized item.)
\label{ex:monty}
\end{enumerate}

Our method will be to:
\begin{itemize}
\item Describe the space of possible situations
\item Define the events whose probabilities we will consider
\item Evaluate such probabilities.
\end{itemize}

\section{Spaces}


Generally textbook problems involving probability involve the
description of some situation, with an explicit uncertainty, including
the outcome of certain measures, then the student is asked to compute
the probability of some event.

It is common to refer to a sample space by the labels \(S\), $\Omega$,
or \(U\). While this space of events underpins modern understandings
of probability theory, textbooks sometimes give a couple of examples
involving coin tosses and promptly forget this concept in the body of
the text. Here we will instead develop this concept using our DSL
methodology.  Once this is done, we'll see that it'll become our basic
tool to solve probability problems.

Our first task is to describe the possible structure of sample space,
and model them as a DSL.

We will use a type to represent spaces. This type is indexed by the
underlying Haskell type of possible situtations.
\begin{spec}
Space :: Type -> Type
\end{spec}

\paragraph{Finite space}
In Example~\ref{ex:dice}, we consider dice with 6 faces. For such a
purpose we define a constructor |Finite| embedding a list of possible outcomes into a space:
\begin{spec}
Finite :: [a] -> Space a
\end{spec}
Our die is then:
\begin{code}
die :: Space Int
die = Finite [1..6]
\end{code}

\paragraph{Scaling space}
If the die is well-balanced, then all cases will have the same
probability (or probability mass) in the space. But this is not always
the case.  Hence we'll need a way to represent this. We use the
following combinator:
\begin{spec}
Factor :: Real -> Space ()
\end{spec}
It underlying type is the unit type |()|, but its mass (or
density) is given by the a real number, which must be
non-negative\footnote{should we use a type for that}?

On its own, it may appear useless, but we can setup the |Space| type
so that this mass or density can depend on (previously introduced)
spaces.
\paragraph{Product of distributions}
Indeed, we can construct the product of two spaces as
\begin{code}
prod :: Space a -> Space b -> Space (a,b)
prod a b = Sigma a (const b) -- TODO: hide definition
\end{code}
For example two dice are represented as follows:

\begin{code}
twoDice :: Space (Int,Int)
twoDice = prod die die
\end{code}
%
But let's say now that we need the sum is greater than 7. We
can define the following space, which has a mass 1 if the condition is satisfied and 0 otherwise:
\begin{code}
sumAbove7 :: (Int,Int) -> Space ()
sumAbove7 (x,y) = Factor (if x+y > 7 then 1 else 0)
\end{code}
We want to take the product of the |twoDice| space and the |sumAbove7|
space; but the issue is that |sumAbove7| \emph{depends} on the outcome
of |twoDice|. To support this we need a generalisation of the
product\footnote{convensionally called $Σ$ because of its mathematical
  similarity with the summation operation.}
\begin{spec}
Sigma :: (Space a) -> (a -> Space b) -> Space (a,b)
\end{spec}
Hence:
\begin{code}
problem1Situations :: Space ((Int, Int), ())
problem1Situations = Sigma twoDice sumAbove7
\end{code}

\paragraph{Real line}
Before we continue, we may also add a way to represent for real-valued
variables:
\begin{spec}
RealLine :: Space Real
\end{spec}


\paragraph{Summary}
In sum, we have:

\begin{code}

data Space a where
  Sigma :: (Space a) -> (a -> Space b) -> Space (a,b)
  Factor :: Real -> Space ()
  Finite :: [a] -> Space a
  RealLine :: Space Real

  -- Bind :: (Space a) -> (a -> Space b) -> Space b
  Project :: (a -> b) -> Space a -> Space b

instance Functor Space where
  fmap f = (pure f <*>)
instance Applicative Space where
  pure x = Finite [x]
  (<*>) = ap
instance Monad Space where
  a >>= f = Project snd (Sigma a f)
\end{code}

\subsection{Distributions}

Another important notion in probability theory is that of a
distribution. A distribution is a space whose total mass is equal to
1.

\begin{code}
uniformDiscrete :: [a] -> Space a
uniformDiscrete xs = do
  x <- Finite xs
  Factor (1.0 / fromIntegral (length xs))
  return x
\end{code}

\begin{code}
bernoulli :: Real -> Space Bool
bernoulli p = do
  x <- Finite [False,True]
  Factor (if x then p else 1-p)
  return x
\end{code}

\begin{code}
normal :: Real -> Real -> Space Real
normal μ σ = do
  x <- RealLine
  Factor (1 / exp(- ( (x - μ)**2 / (2 * σ**2)) ))
  return x
\end{code}

In some textbooks, distributions are sometimes called ``random
variables''. However we consider this terminology to be misguided ---
random variables will be defined precisely later on.

We could try to define the probabilities or densities of possible
values of a space: |spaceDensity :: Space a -> (a -> Real)|, and from
there define the expected values (or other statistical moments), but
it's simpler to directly give a generalized version of the
integration of a function for spaces:

\begin{code}
integrate :: Space a -> (a -> Real) -> Real
integrate (Sigma a f) g = integrate a $ \x -> integrate (f x) $ \y -> g (x,y)
integrate (Factor f) g = f * g ()
integrate (Finite a) g = bigsum a g
integrate (RealLine) g = integral g

integrate (Project f a) g = integrate a (g . f)

-- sum of some terms (finite so we can compute this)
bigsum :: [a] -> (a -> Real) -> Real
bigsum xs f = foldl (+) 0 [f x | x <- xs]

-- indefinite integral over the whole real line
integral :: (Real -> Real) -> Real
integral = undefined
\end{code}

The first, most simple quantity that we need is the measure
of the space. Indeed, summing the probabilities should be 1, but a
space can have a different value, and thus we must re-normalise a
space to get a distribution.

To compute the measure of a space, we can simply integrate the
constant 1.

\begin{code}
measure :: Space a -> Real
measure d = integrate d (const 1)
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

A random variable can now be defined as a function from the space of
situations.


Then, we can define the expected value, and other statistical
moments.

For example, the expected value of real-valued random variable is then:
\begin{code}
expectedValue :: Space a -> (a -> Real) -> Real
expectedValue d f = integrate d f / measure d
\end{code}

exercise: define the variance, skew, curtosis, etc.


\subsection{Events and probability}

Events are boolean-valued random variables.

Thus an event can be defined as a boolean-valued function over a
space. If the space accurately represents all possible situations with
a density, then the expected value of the event is its
probability. (However we need to map booleans to reals.)

\begin{code}
indicator :: Num p => Bool -> p
indicator True = 1
indicator False = 0

probability :: Space a -> (a -> Bool) -> Real
probability d f = expectedValue d (indicator . f)
\end{code}

An alternative way to define probability is as the ratio the measures of the subspace where f holds and the complete space.
First we need the following helper, corresponding to a subspace which has measure 1 if c is true and 0 otherwise.

\begin{code}
isTrue :: Bool -> Space ()
isTrue c = Factor (indicator c)
\end{code}

The subspace of s where f holds is then  |Sigma s (isTrue f)|, and we can state:

Lemma: measure s * probability s f = measure (Sigma s (isTrue f))
Proof:

\begin{spec}
probability s f
  = expectedValue s (indicator . f)
  = expectedValue s (indicator . f)
  = integrate s (indicator . f) / measure s
  = integrate s (indicator . f) / measure s
  = integrate s (\x -> indicator (f x)) / measure s
  = integrate s (\x -> indicator (f x) * constant 1 (x,())) / measure s
  = integrate s (\x -> integrate (isTrue f) (\y -> constant 1 (x,y)) / measure s)
  = measure (Sigma s (isTrue f)) / measure s
\end{spec}

\section{Conditional probability}

$P(F ∣ G)$

We can define the conditional probability of f wrt. g by taking the
probability of f in the sub space where g is guaranteed to hold:

\begin{code}
condProb :: Space a -> (a -> Bool) -> (a -> Bool) -> Real
condProb s g f = probability (Sigma s (isTrue . g)) (f . fst)
\end{code}

We find the above defintion more intuitive than the more usual P(F∣G)
 = P(F∩G ∣ G). However, this last equality can be recovered by calculation:

Lemma:  condProb s g f = probability s (\y -> f y && g y) / probability s g
Proof:
\begin{spec}
condProb s g f
  = probability (Sigma s (isTrue . g)) (f . fst)
  = expectedValue (Sigma s (isTrue . g)) (indicator . f . fst)
  = (1/measure(Sigma s (isTrue . g))) * (integrate (Sigma s (isTrue . g)) (indicator . f . fst))
  = (1/measure s/probability s g) * (integrate s $ \x -> integrate (isTrue . g) $ \y -> indicator . f . fst $ (x,y))
  = (1/measure s/probability s g) * (integrate s $ \x -> integrate (isTrue . g) $ \y -> indicator . f $ x)
  = (1/measure s/probability s g) * (integrate s $ \x -> indicator (g x) * indicator (f x))
  = (1/measure s/probability s g) * (integrate s $ \x -> indicator (\y -> g y &&  f y))
  = (1/probability s g) * (integrate s $ \x -> indicator (\y -> g y &&  f y)) / measure s
  = (1/probability s g) * probability s (\y -> g y &&  f y)
% emacs wakeup $
\end{spec}

\begin{code}
dirac :: Real -> Real
dirac = undefined

equal :: Real -> Real -> Space ()
equal x y = Factor (dirac (x-y))

probability' :: Space Bool -> Real
probability' d = expectedValue d indicator

-- (maybe) we're making some error in case |measure d| is 0.

\end{code}

The above drug test problem is often used as an illustration for the bayes
theorem.  We won't be needing the bayes theorm here!
In fact, through this example, we will see that it is a (computable)
consequence of our definitions.

Solution
We begin by describing the space of situations:

\begin{code}
drugSpace :: Space (Bool, (Bool, ()))
drugSpace = Sigma (bernoulli 0.005) $ \isUser ->
            Sigma (bernoulli (if isUser then 0.99 else 0.01)) $ \testPositive ->
            isTrue testPositive -- we have ``a positive test'' by assumption

userProb :: Real
userProb = probability drugSpace (\ (isUser,_) -> isUser)

-- >>> userProb
-- 0.33221476510067116

space' :: Space Bool
space' =
  do isUser <- bernoulli 0.005
     testPositive <- bernoulli (if isUser then 0.99 else 0.01)
     isTrue testPositive  -- we have ``a positive test'' by assumption
     return isUser

solution' :: Real
solution' = probability' space'

-- >>> solution'
-- 0.33221476510067116
\end{code}
Doing it manually:

\begin{spec}
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
\end{spec}
Numerator:
\begin{spec}
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
-- emacs $

\end{spec}


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

\section{Independent events}

\begin{code}
independentEvents :: Space a -> (a -> Bool) -> (a -> Bool) -> Bool
independentEvents s e f = probability s e == condProb s f e
\end{code}

According to Grinstead and Snell:

Theorem: Two events are independent iff. P(E ∩ F) = P(E) · P(F)





% Local Variables:
% dante-methods : (bare-ghci)
% mode: literate-haskell
% End:
