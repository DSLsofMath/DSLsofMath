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
problem1Situations' :: Space ((Int, Int), ())
problem1Situations' = Sigma twoDice sumAbove7
\end{code}

\paragraph{Projections}
In the end we may not be interested in all values and hide some of the
generated values. For this we use the following combinator:
\begin{spec}
Project :: (a -> b) -> Space a -> Space b
\end{spec}

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
  Project :: (a -> b) -> Space a -> Space b

instance Functor Space where
  fmap f = (pure f <*>)
instance Applicative Space where
  pure x = Finite [x]
  (<*>) = ap
instance Monad Space where
  a >>= f = Project snd (Sigma a f)
\end{code}

TODO: somehow explain the monadic interface.
\subsection{Distributions}

Another important notion in probability theory is that of a
distribution. A distribution is a space whose total mass (or measure) is equal to
1.
\begin{code}
isDistribution :: Space a -> Bool
isDistribution s = measure s == 1
\end{code}
We may use the following type synonym to indicate distributions:

\begin{code}
type Distr a = Space a
uniformDiscrete :: [a] -> Distr a
uniformDiscrete xs = do
  x <- Finite xs
  Factor (1.0 / fromIntegral (length xs))
  return x

dieDistr :: Distr Integer
dieDistr = (uniformDiscrete [1..6])  
\end{code}

\begin{code}
bernoulli :: Real -> Distr Bool
bernoulli p = do
  x <- Finite [False,True]
  Factor (if x then p else 1-p)
  return x
\end{code}

\begin{code}
normal :: Real -> Real -> Distr Real
normal μ σ = do
  x <- RealLine
  Factor (exp(- ( (x - μ)**2 / (2 * σ**2)) ) / (σ * sqrt (2*pi)))
  return x
\end{code}

In some textbooks, distributions are sometimes called ``random
variables''. However we consider this terminology to be misguided ---
random variables will be defined precisely later on.

We could try to define the probabilities or densities of possible
values of a distribution: |distributionDensity :: Space a -> (a -> Real)|, and from
there define the expected values (or other statistical moments), but
it's simpler to directly give a generalized version of the
integration of a function for \emph{spaces}:

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
constant 1 (so only mass matters).

\begin{code}
measure :: Space a -> Real
measure d = integrate d (const 1)

-- >>> measure (bernoulli 0.2)
-- 1.0
\end{code}

Integration over a real-valued distribution yields its expected value:
\begin{code}
expectedValueOfDistr :: Distr Real -> Real
expectedValueOfDistr d = integrate d id

-- >>> expectedValueOfDistr dieDistr
-- 3.5
\end{code}

\section{Random Variables}
Even though we have already \ref{ch:3} studied variables in detail, it
is good to come back to them for a moment before returning to
\emph{random} variables proper.

According to Wikipedia, a variable has a different meaning in computer
science and in mathematics:
\begin{quote}
  Variable may refer to:
  \begin{itemize}
  \item [Variable (computer science)], a symbolic name associated with a
    value and whose associated value may be changed
  \item[Variable (mathematics)], a symbol that represents a quantity in a
    mathematical expression, as used in many sciences
  \end{itemize}
\end{quote}


By now we have a pretty good grip on 1. In ch. 3. we have described a
way to reduce mathematical variables (position \(q\) and velocity
\(v\) in lagrangian mechanics) to computer science variables. This was
done by expressing variables as \emph{functions} of a truly free
variable, time (\(t\)). Time is a ``computer science'' variable in
this context: it can be substituted by any value without ``side
effects'' on other variables (unlike \(q\) and \(v\))

In this light, let us return to random variables. Wikipedia is not very helpful here, so
we can turn ourselves to \citet{(Introduction to Probability, Charles
  M. Grinstead and J. Laurie Snell)}
\begin{quote}
  A random variable is simply an expression whose value is the outcome
  of a particular experiment.
\end{quote}

We will be using spaces to represent the "experiments" that Grinstead
and Snell mention. More specifically, if |s : Space a|, each possible
situation is representable in |a| and the space will specify the mass
of each of them (via the integrator).

Then, a |b|-valued random variable |f| related to an experiment
represented by a space |s : Space a| is a function |f| of type |a
-> b|.


\footnote{|Project| correspond to having hidden
  variables.}

Then, we can define the expected value (and other statistical
moments).

In textbooks, one will often find the notation $E[t]$ for the expected
value of a real-valued random variable |t|. From our point of view,
this can be quite confusing because this leaves the space of
situations, completely implicit. In particular, it is not clear how
|t| even depends on the outcome of experiments.

With our DSL approach, we make all that completely explicit. For
example, the expected value of real-valued random variable is as
follows:
\begin{code}
expectedValue :: Space a -> (a -> Real) -> Real
expectedValue s f = integrate s f / measure s

-- >>> expectedValue twoDice (\(x,y) -> fromIntegral (x+y))
-- 7.0
\end{code}

TODO: (maybe) we're making some error in case |measure d| is 0.


exercise: define the variance, skew, curtosis, etc.


\subsection{Events and probability}

In textbooks, one typically find the notation |P(e)| for the
probability of an \emph{event} |e|. Again, the space of situations |s|
is implicit as well as the dependence between |e| and |s|.

Here, we can define events as boolean-valued random variables.

Thus an event |e| can be defined as a boolean-valued function |e : a
-> Bool| over a space |s : Space a|. Assuming that the space |s|
accurately represents the relative mass of all possible situations,
there are two ways to define the probability of |e|.

The first defintion is as the expected value of |indicator . e|,
where |indicator| maps boolean to reals as follows:
\begin{code}
indicator :: Bool -> Real
indicator True = 1
indicator False = 0

probability1 :: Space a -> (a -> Bool) -> Real
probability1 d f = expectedValue d (indicator . f)
\end{code}

The second definition of probability is as the ratio the measures of
the subspace where |e| holds and the complete space.

\begin{code}
probability2 :: Space a -> (a -> Bool) -> Real
probability2 s f = measure (Sigma s (isTrue . f)) / measure s

isTrue :: Bool -> Space ()
isTrue c = Factor (indicator c)
\end{code}
where |isTrue c| is the subspace subspace which has measure 1 if |c|
is true and 0 otherwise --- 
The subspace of |s| where |f| holds is then |Sigma s (isTrue f)|.

We can show that if |s| has a non-zero measure, then the two definitions are equivalent:

Lemma: |measure s * probability s f = measure (Sigma s (isTrue f))|
Proof:
\begin{spec}
probability1 s f
  = expectedValue s (indicator . f)
  = expectedValue s (indicator . f)
  = integrate s (indicator . f) / measure s
  = integrate s (indicator . f) / measure s
  = integrate s (\x -> indicator (f x)) / measure s
  = integrate s (\x -> indicator (f x) * constant 1 (x,())) / measure s
  = integrate s (\x -> integrate (isTrue . f) (\y -> constant 1 (x,y)) / measure s)
  = measure (Sigma s (isTrue . f)) / measure s
\end{spec}

TODO: other variant
\begin{code}
probability :: Space Bool -> Real
probability d = expectedValue d indicator
\end{code}

Sometimes one even finds in the literature and folklore the notation
|P(v)|, which stands for |P(t=v)|, for an implicit random variable
|t|. Here even more creativity is required from the reader, who must
also infer which random variable the author means.

\section{Conditional probability}

Another important notion is conditional probability, written
$P(F ∣ G)$ and read ``probability of |f| given |g|''.

We can define the conditional probability of |f| given |g| by taking
the probability of |f| in the sub space where |g| holds:

\begin{code}
condProb :: Space a -> (a -> Bool) -> (a -> Bool) -> Real
condProb s g f = probability1 (Sigma s (isTrue . g)) (f . fst)
\end{code}

We find the above defintion more intuitive than the more usual $P(F∣G)
 = P(F∩G ∣ G)$. However, this last equality can be proven, by calculation:

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

\section{Examples}

\subsection{Dice}
\begin{code}
diceProblem :: Real
diceProblem = probability diceSpace

diceSpace = do
  die1 <- dieDistr
  die2 <- dieDistr
  isTrue (die1 + die2 >= 7)
  return (die1 * die2 >= 10)

-- >>> dieProblem
-- 0.9047619047619047
\end{code}

\subsection{Drug test}
The above drug test problem \ref{ex:drugtest} is often used as an
illustration for the Bayes theorem.

Solution
We begin by describing the space of situations:

\begin{code}
drugSpace :: Space (Bool, (Bool, ()))
drugSpace = Sigma (bernoulli 0.005) $ \isUser ->
            Sigma (bernoulli (if isUser then 0.99 else 0.01)) $ \testPositive ->
            isTrue testPositive -- we have ``a positive test'' by assumption

userProb :: Real
userProb = probability1 drugSpace (\ (isUser,_) -> isUser)

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

Perhaps surprisingly, we never needed the Bayes theorem to solve the
problem.


\subsection{Monty Hall}


\begin{code}
montySpaceIncorrect :: Bool -> Space Bool
montySpaceIncorrect changing = do
  winningDoor <- uniformDiscrete [1::Int,2,3] -- any door can be the winning one
  let montyPickedDoor = 3
  isTrue (montyPickedDoor /= winningDoor)
  let newPickedDoor = if changing then 2 else 1
  return (newPickedDoor == winningDoor)

-- >>> probability (montySpace1 False)
-- 0.5

-- >>> probability (montySpace1 True)
-- 0.5
\end{code}

Generalisation to picking any door:
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

-- >>> probability (montySpace False)
-- 0.3333333333333333

-- >>> probability (montySpace True)
-- 0.6666666666666666
\end{code}

\section{Independent events}

\begin{code}
independentEvents :: Space a -> (a -> Bool) -> (a -> Bool) -> Bool
independentEvents s e f = probability1 s e == condProb s f e
\end{code}

According to Grinstead and Snell:

Theorem: Two events are independent iff. $P(E ∩ F) = P(E) · P(F)$

\section{Continuous spaces and equality}

\begin{itemize}
\item dirac :: Real -> Real
dirac = undefined

equal :: Real -> Real -> Space ()
equal x y = Factor (dirac (x-y))

\end{itemize}



% Local Variables:
% dante-methods : (bare-ghci)
% mode: literate-haskell
% End:
