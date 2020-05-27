\section{Probability Theory}

\begin{code}
{-# LANGUAGE GADTs #-}
module DSLsofMath.W09 where

import Prelude hiding (Real)
import Control.Monad (ap)
import Data.List ((\\))

type Real = Double -- pretend...
\end{code}

In this chapter, we will define a DSL to describe, reason about, problems such as the
following (sometimes compute the probabilites involved)

\begin{enumerate}[label=\arabic*.]
\item Assume you throw two 6-face dice, what is the probability that
  the product is greater than 10 if their sum is greater than 7?
\label{ex:dice}
\item Suppose that a test for using a particular drug is 99\% sensitive and
99\% specific. That is, the test will produce 99\% true positive results
for drug users and 99\% true negative results for non-drug
users. Suppose that 0.5\% of people are users of the drug. What is the
probability that a randomly selected individual with a positive test
is a drug user?
\footnote{Example found in \href{https://en.wikipedia.org/wiki/Bayes'_theorem}{Wikipedia article on Bayes' theorem}}
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
\item Describe the space of possible situations, or outcomes.
\item Define the events whose probabilities we will consider
\item Evaluate such probabilities.
\end{itemize}

Depending on the context, we use the word ``situation'' or ``outcome''
for the same mathematical objects. The word ``outcome'' evokes
that some experiment is explicitly performed, and we then
consider its outcome. When we use the word ``situation'' there
is not explict experiment, but still something happened and we
consider the situation at given moment.

\TODO{Improve this text}


\subsection{Spaces}


Generally textbook problems involving probability involve the
description of some situation, with an explicit uncertainty, including
the outcome of certain measures, then the student is asked to compute
the probability of some event.

It is common to refer to a sample space by the labels \(S\), $\Omega$,
or \(U\). While this space of events underpins modern understandings
of probability theory, textbooks sometimes give a couple of examples
involving coin tosses and promptly forget this concept in the body of
the text. Here we will instead develop this concept using our DSL
methodology.  Once this is done, we'll see that it will become our basic
tool to solve probability problems.

Our first task is to describe the possible structure of sample spaces,
and model them as a DSL.

We will use a type to represent spaces. This type is indexed by the
underlying Haskell type of possible situtations.
%
\TODO{From the use it looks like ``situtation'' here should be ``outcome''.}
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
\TODO{Perhaps parametrise |die| on the number of sides. (That is very common.)}
%
In particular the space |point x| is the space with a single point:
\begin{code}
point :: a -> Space a
point x = Finite [x]
\end{code}

\paragraph{Scaling space}
If the die is well-balanced, then all cases will have the same
probability (or probability mass) in the space. But this is not always
the case.  Hence we'll need a way to represent this. We use the
following combinator:
\footnote{This is in fact scaling, as defined in the LinAlg Chapter --- the spaces over a given domain form a vector space. However, we choose not to use this point of view, because we are generally not interested in the vector space structure of probability spaces. Furthermore, doing so risks adding more confusion than anything. In particular, the word "scale" can be misunderstood as scaling the \emph{value} of a numerical variable. Instead here we scale densites, and use |Factor| for this purpose.  }
\TODO{Consider the alternative at some point: |scale r s| would multiply each probability in the space |s| by the factor |r|. Scaling is talked about in the LinAlg Chapter.}
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
\end{code}

For example, the possible outcomes of the ``experiment'' of throwing two 6-faced
dice are represented as follows:

\begin{code}
twoDice :: Space (Int,Int)
twoDice = prod die die
\end{code}
%
But let's say now that we need the sum to be greater than 7. We
can define the following parametric space, which has a mass 1 if the condition is satisfied and 0 otherwise.
\begin{code}
sumAbove7 :: (Int,Int) -> Space ()
sumAbove7 (x,y) = Factor (if x+y > 7 then 1 else 0)
\end{code}
\TODO{Phrase this in terms of experiment again}
We want to take the product of the |twoDice| space and the |sumAbove7|
space; but the issue is that |sumAbove7| \emph{depends} on the outcome
of |twoDice|. To support this we need a generalisation of the
product\footnote{convensionally called $\Sigma$ because of its mathematical
  similarity with the summation operation.}
\begin{spec}
Sigma :: (Space a) -> (a -> Space b) -> Space (a,b)
\end{spec}
Hence:
\begin{code}
problem1 :: Space ((Int, Int), ())
problem1 = Sigma twoDice sumAbove7
\end{code}
\TODO{Again the word ``Situation'' is a bit confusing: how are outcomes and situations related?}

We can check that the product of spaces is a special case of |Sigma|:
\begin{code}
prod a b = Sigma a (const b)
\end{code}
If we compare to the usual sum notation the right hans side would be \(\sum_{i\in a} b\) which is the sum of |card a| copies of |b|, thus a kind of ``product'' of |a| and |b|.

\paragraph{Projections}
In the end we may not be interested in all values and hide some of the
generated values. For this we use the following combinator:
\begin{spec}
Project :: (a -> b) -> Space a -> Space b
\end{spec}

\paragraph{Real line}
Before we continue, we may also add a way to represent real-valued
spaces:
\begin{spec}
RealLine :: Space Real
\end{spec}


\paragraph{Summary}
In sum, we have a datatype for the abstract syntax of ``space expressions'':

\begin{code}
data Space a where
  Sigma     :: Space a -> (a -> Space b) -> Space (a,b)
  Factor    :: Real -> Space ()
  Finite    :: [a] -> Space a
  RealLine  :: Space Real
  Project   :: (a -> b) -> Space a -> Space b
\end{code}

\subsection{Bind and return}

Very often, one will be interested only in a particular projection
only. Hence, it is useful to combine |Sigma| and |Project| in a single
combinator, as follows:

\begin{code}
bind :: Space a -> (a -> Space b) -> Space b
bind a f = Project snd (Sigma a f)
\end{code}

This means that |Space| has a monadic structure:
\TODO{explain this somewhere in the book}
\begin{code}
instance Functor Space where
  fmap f = (pure f <*>)
instance Applicative Space where
  pure x = Finite [x]
  (<*>) = ap
instance Monad Space where
  (>>=) = bind
\end{code}

 \subsection{Distributions}

Another important notion in probability theory is that of a
distribution. A distribution is a space whose total mass (or measure) is equal to
1. (We will see how to compute the |measure| later.)
\begin{code}
isDistribution :: Space a -> Bool
isDistribution s = measure s == 1
\end{code}
We may use the following type synonym to indicate distributions:
\begin{code}
type Distr a = Space a
\end{code}

Let us define a few useful distributions. First, we present the
uniform distribution among a finite set of elements. It is essentially
the same as the |Finite| space, but we scale every element so that the
total measure comes down to 1.
%
\begin{code}
scaleWith :: (a -> Real) -> Space a -> Space a
scaleWith f s = do {x <- s; Factor (f x); return x}
scale :: Real -> Space a -> Space a
scale c = scaleWith (const c)
\end{code}
\TODO{The pattern |s >>= \x -> Factor (f x) >> return x| deserves a name. Perhaps |scaleWith f s|?}
\begin{code}
uniformDiscrete :: [a] -> Distr a
uniformDiscrete xs = scale  (1.0 / fromIntegral (length xs))
                            (Finite xs)
\end{code}

The distribution of the balanced die can then be represented as
follows:
\begin{code}
dieDistr :: Distr Integer
dieDistr = uniformDiscrete [1..6]
\end{code}

Another useful discrete distribution is the Bernoulli distribution of
parameter |p|. It is a distribution whose value is |True| with
probability |p| and and |False| with probability |1-p|.
\begin{code}
bernoulli :: Real -> Distr Bool
bernoulli p = scaleWith  (\b -> if b then p else 1-p)
                         (Finite [False,True])
\end{code}

%format mu = "\mu"
%format sigma = "\sigma"
Finally we can show the normal distribution with average |mu| and standard deviation |sigma|.
\begin{code}
normal :: Real -> Real -> Distr Real
normal mu sigma = scaleWith (normalMass mu sigma) RealLine

normalMass :: Floating r =>  r -> r -> r -> r
normalMass mu sigma x = exp(- ( ((x - mu)/sigma)^2 / 2)) / (sigma * sqrt (2*pi))
\end{code}

In some textbooks, distributions are sometimes called ``random
variables''. However we consider this terminology to be misguided ---
random variables will be defined precisely later on.

We could try to define the probabilities or densities of possible
values of a distribution, say |distributionDensity :: Space a -> (a -> Real)|, and from
there define the expected value (and other statistical moments), but
we'll take another route.

\subsection{Semantics of spaces}
First, we come back to general spaces. We define a function
|integrator|, which generalises the notion of weighted sum, and
weighted integral. When encountering |Finite| spaces, we will sum;
when encountering |RealLine| we integrate.  When encountering |Factor|
we will adjust the weights. The integrator of a product (in general
|Sigma|) is the nested integration of spaces.

\begin{code}
integrator :: Space a -> (a -> Real) -> Real
integrator (Finite a)     g = bigsum a g
integrator (RealLine)     g = integral g
integrator (Factor f)     g = f * g ()
integrator (Sigma a f)    g = integrator a $ \x -> integrator (f x) $ \y -> g (x,y)
integrator (Project f a)  g = integrator a (g . f)
\end{code}

Sum of some terms (finite list so we can compute this).
\begin{code}
bigsum :: [a] -> (a -> Real) -> Real
bigsum xs f = sum (map f xs)
\end{code}

Indefinite integral over the whole real line.
We will leave this undefined; to use in symbolic computations only.
\begin{code}
integral :: (Real -> Real) -> Real
integral = undefined
\end{code}

The simplest quantity that we can compute using the integrator is the measure
of the space --- the total mass.
%
To compute the measure of a space, we can simply integrate the
constant 1 (so only mass matters).
\TODO{Explain the |>>> some expr| syntax.}
\begin{code}
measure :: Space a -> Real
measure d = integrator d (const 1)

-- |>>> measure (bernoulli 0.2)|
-- 1.0
\end{code}

The integration of |id| over a real-valued distribution yields its
expected value.
%
\TODO{Later (in the event probability lemma) the name used is |expectedValue|.}
\begin{code}
expectedValueOfDistr :: Distr Real -> Real
expectedValueOfDistr d = integrator d id

-- |>>> expectedValueOfDistr dieDistr|
-- 3.5
\end{code}

Exercise: compute symbolically the expected value of the |bernoulli| distribution.

\subsection{Random Variables}
Even though we have already (in Chapter \ref{ch:3}) studied variables in detail, it
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


By now we have a pretty good grip on variables in computer science. In
Chapter \ref{ch:3}. we have described a way to reduce mathematical
variables (position \(q\) and velocity \(v\) in lagrangian mechanics)
to computer science variables. This was done by expressing variables
as \emph{functions} of a truly free variable, time (\(t\)). Time is a
``computer science'' variable in this context: it can be substituted
by any value without ``side effects'' on other variables (unlike \(q\)
and \(v\))

In this light, let us return to random variables. Wikipedia is not
very helpful here, so we can turn ourselves to
\citet{IntroProb_Grinstead_Snell_2003}, who give the following definition:
\begin{quote}
  A random variable is simply an expression whose value is the outcome
  of a particular experiment.
\end{quote}

Here we will be using spaces to represent the ``experiments'' that
Grinstead and Snell mention. More specifically, if |s : Space a|, each
possible situation is representable in |a| and the space will specify
the mass of each of them (via the integrator).

Then, a |b|-valued random variable |f| related to an experiment
represented by a space |s : Space a| is a function |f| of type |a
-> b|.



Then, we can define the expected value (and other statistical
moments).

In textbooks, one will often find the notation $E[t]$ for the expected
value of a real-valued random variable |t|. From our point of view,
this can be quite confusing because this leaves the space of
situations completely implicit. In particular, it is not clear how
|t| even depends on the outcome of experiments.

With our DSL approach, we make all that completely explicit. For
example, the expected value of real-valued random variable is as
follows:
\begin{code}
expectedValue :: Space a -> (a -> Real) -> Real
expectedValue s f = integrator s f / measure s

-- |>>> expectedValue twoDice (\ (x,y) -> fromIntegral (x+y))|
-- 7.0
\end{code}

TODO: (maybe) we're making some error in case |measure s| is 0.

Essentially, what the above does is computing the weighted
sum/integral of |f(x)| for every point |x| in the space.  Because |s|
is a space (not a distribution), we must normalise the result by
dividing by its measure.

Exercise: define the variance, skew, curtosis, etc.

\subsection{Events and probability}

In textbooks, one typically finds the notation |P(e)| for the
probability of an \emph{event} |e|. Again, the space of situations |s|
is implicit as well as the dependence between |e| and |s|.

Here, we define events as boolean-valued random variables.

Thus an event |e| can be defined as a boolean-valued function |e : a
-> Bool| over a space |s : Space a|. Assuming that the space |s|
accurately represents the relative mass of all possible situations,
there are two ways to define the probability of |e|.

The first defintion is as the expected value of |indicator . e|,
where |indicator| maps boolean to reals as follows:
\begin{code}
indicator :: Bool -> Real
indicator True   = 1
indicator False  = 0

probability1 :: Space a -> (a -> Bool) -> Real
probability1 d e = expectedValue d (indicator . e)
\end{code}

The second definition of probability is as the ratio the measures of
the subspace where |e| holds and the complete space.

\begin{code}
probability2 :: Space a -> (a -> Bool) -> Real
probability2 s e = measure (Sigma s (isTrue . e)) / measure s

isTrue :: Bool -> Space ()
isTrue c = Factor (indicator c)
\end{code}
where |isTrue c| is the subspace which has measure 1 if |c|
is true and 0 otherwise.
%
The subspace of |s| where |e| holds is then |Sigma s (isTrue . e)|.
%
\TODO{This construction also needs a name. Perhaps |subspace|? Or something with filter?}
\begin{code}
subspace :: (a->Bool) -> Space a -> Space a
subspace e s = Project fst (Sigma s (isTrue . e))
\end{code}

We can show that if |s| has a non-zero measure, then the two definitions are equivalent:

Lemma: |measure s * probability s e = measure (Sigma s (isTrue . e))|
Proof:
\TODO{Fill in step explanations.}
\begin{spec}
  probability1 s e
= {- Def. of |probability1| -}
  expectedValue s (indicator . e)
= {- Def. of |expectedValue| -}
  integrator s (indicator . e) / measure s
= {- Def. of |(.)| -}
  integrator s (\x -> indicator (e x)) / measure s
= {- mutiplication by 1 -}
  integrator s (\x -> indicator (e x) * const 1 (x,())) / measure s
=
  integrator s (\x -> integrator (isTrue . e) (\y -> const 1 (x,y)) / measure s)
=
  measure (Sigma s (isTrue . e)) / measure s
\end{spec}

It will be often convenient to define a space whose underlying set is
a boolean value and compute the probability of the identity event:
\begin{code}
probability :: Space Bool -> Real
probability d = expectedValue d indicator
\end{code}

Sometimes one even finds in the literature and folklore the notation
|P(v)|, which stands for |P(t=v)|, for an implicit random variable
|t|. Here even more creativity is required from the reader, who must
also infer which random variable the author means.

\subsection{Conditional probability}

Another important notion is conditional probability, written
$P(F ∣ G)$ and read ``probability of |f| given |g|''.

We can define the conditional probability of |f| given |g| by taking
the probability of |f| in the sub space where |g| holds:

\begin{code}
condProb :: Space a -> (a -> Bool) -> (a -> Bool) -> Real
condProb s f g = probability1 (subspace g s) f
\end{code}

We find the above defintion more intuitive than the more usual $P(F∣G)
 = P(F∩G) / P(G)$. However, this last equality can be proven, by calculation:

Lemma:  |condProb s f g == probability s (\y -> f y && g y) / probability s g|
Proof:
\TODO{Redo with step explanations. Probably split into helper lemma(s) to avoid diving ``too deep''.}
\begin{spec}
  condProb s f g
=
  probability (Sigma s (isTrue . g)) (f . fst)
=
  expectedValue (Sigma s (isTrue . g)) (indicator . f . fst)
=
  (1/measure(Sigma s (isTrue . g))) * (integrator (Sigma s (isTrue . g)) (indicator . f . fst))
=
  (1/measure s/probability s g) * (integrator s $ \x -> integrator (isTrue . g) $ \y -> indicator . f . fst $ (x,y))
=
  (1/measure s/probability s g) * (integrator s $ \x -> integrator (isTrue . g) $ \y -> indicator . f $ x)
=
  (1/measure s/probability s g) * (integrator s $ \x -> indicator (g x) * indicator (f x))
=
  (1/measure s/probability s g) * (integrator s $ \x -> indicator (\y -> g y &&  f y))
=
  (1/probability s g) * (integrator s $ \x -> indicator (\y -> g y &&  f y)) / measure s
=
  (1/probability s g) * probability s (\y -> g y &&  f y)
\end{spec}
% emacs wakeup $

\subsection{Examples}

\subsubsection{Dice problem}

We will use the monadic interface to define the experiment, hiding all
random variables except the outcome that we care about (is the product greater than 10?):
\begin{code}
diceSpace :: Space Bool
diceSpace = do
  x <- die  -- balanced die 1
  y <- die  -- balanced die 2
  isTrue (x + y >= 7)  -- observe that the sum is >= 7
  return (x * y >= 10) -- consider only the event ``product >= 10''
\end{code}
Then we can compute is probability:
\begin{code}
diceProblem :: Real
diceProblem = probability diceSpace

-- |>>> diceProblem|
-- 0.9047619047619047
\end{code}

Some helpers: \TODO{Merge with text above to explain more}
\begin{code}
p1 (x,y) = x+y >= 7
p2 (x,y) = x*y >= 10
test1     = measure (prod die die >>= isTrue . p1)                       -- 21
test2     = measure (prod die die >>= isTrue . p2)                       -- 19
testBoth  = measure (prod die die >>= isTrue . \xy -> p1 xy && p2 xy)    -- 19
prob21    = condProb (prod die die) p2 p1                                -- 19/21
\end{code}

\subsection{Drug test}
The above drug test problem \ref{ex:drugtest} is often used as an
illustration for the Bayes theorem. We can solve it in exactly the
same fashion as the Dice problem.

We begin by describing the space of situations. Here we make heavy use
of the |bernoulli| distribution. First we model the distribution of
drug users. Then we model the distribution of test outcomes ---
depending on whether we have a user or not. Finally, we project out
all variables, caring only about |isUser|.

\begin{code}
drugSpace :: Space Bool
drugSpace = do
  isUser <- bernoulli 0.005  -- model distribution of drug users
  testPositive <- bernoulli (if isUser then 0.99 else 0.01)
  isTrue testPositive        -- we have ``a positive test'' by assumption
  return isUser              -- we're interested in |isUser| only.
\end{code}
The probability is computed as usual:
\begin{code}
userProb :: Real
userProb = probability drugSpace

-- |>>> userProb|
-- 0.33221476510067116
\end{code}

Perhaps surprisingly, we never needed the Bayes theorem to solve the
problem. Indeed, the Bayes theorem is already incorporated in our
defintion of |integrator|.

\subsection{Monty Hall}

To illustrate that defining spaces can be tricky, we begin by
describing an \emph{incorrect} way to model the Monty Hall problem.

\begin{code}
montySpaceIncorrect :: Bool -> Space Bool
montySpaceIncorrect changing = do
  winningDoor <- uniformDiscrete [1::Int,2,3]    -- any door can be the winning one
  let pickedDoor = 1                             -- player picks door 1.
  let montyPickedDoor = 3                        -- monty opens door 3.
  isTrue (montyPickedDoor /= winningDoor)        -- door 3 is not winning
  let newPickedDoor = if changing then 2 else 1  -- we can change or not
  return (newPickedDoor == winningDoor)          -- has the player won?

-- |>>> probability (montySpace1 False)|
-- 0.5

-- |>>> probability (montySpace1 True)|
-- 0.5
\end{code}
The above is incorrect, because everything happens as if Monty chooses
a door before the player made its first choice. A correct model is
the following:
\begin{code}
doors :: [Int]
doors = [1,2,3]
montySpace :: Bool -> Space Bool
montySpace changing = do
  winningDoor      <-  uniformDiscrete doors  -- any door can be the winning one
  pickedDoor       <-  uniformDiscrete doors  -- player picks door blindly
  montyPickedDoor  <-  uniformDiscrete        -- Monty cannot pick the same door, nor a winning door.
                         (doors \\ [pickedDoor, winningDoor])
  let newPickedDoor =
        if changing
        then head (doors \\ [pickedDoor, montyPickedDoor])
        -- player takes a door which is NOT the previously picked, nor the showed door.
        else pickedDoor
  return (newPickedDoor == winningDoor)

-- |>>> probability (montySpace False)|
-- 0.3333333333333333

-- |>>> probability (montySpace True)|
-- 0.6666666666666666
\end{code}

\subsection{Independent events}

\begin{code}
independentEvents :: Space a -> (a -> Bool) -> (a -> Bool) -> Bool
independentEvents s e f = probability1 s e == condProb s e f
\end{code}

According to Grinstead and Snell:

Theorem: Two events are independent iff. $P(E ∩ F) = P(E) · P(F)$

\subsection{Continuous spaces and equality}

\begin{itemize}
\item dirac :: Real -> Real
dirac = undefined

equal :: Real -> Real -> Space ()
equal x y = Factor (dirac (x-y))
\end{itemize}

Number of times we need to throw a coin to get 3 heads in a row.
\begin{code}
coin = bernoulli 0.5

example = helper 3

helper 0 = return 0
helper n = do
  h <- coin
  (1 +) <$> if h
    then helper (n-1)
    else helper 3 -- when we have a tail, we start from scratch

-- >>> probability1 example (< 5)
-- Does not terminate (the "else" case must always be run.)


coins = do
  x <- coin
  xs <- coins
  return (x:xs)

threeHeads :: [Bool] -> Int
threeHeads (True:True:True:_) = 3
threeHeads (_:xs) = 1 + threeHeads xs

example' = threeHeads <$> coins




\end{code}

\begin{spec}
=  probability1 example (= n)
 {- Def. of probability1 -}
=  integrator example (indicator (= n))
\end{spec}

Note that we have an infinite list; and so the evaluator cannot solve this problem.

% Local Variables:
% dante-methods : (bare-ghci)
% mode: literate-haskell
% End:
