\section{Probability Theory}

\begin{code}
{-# LANGUAGE GADTs #-}
module DSLsofMath.W09 where

import Control.Monad (ap)
import Data.List ((\\))

type REAL = Double -- pretend...
\end{code}

In this chapter, we define a DSL to describe and reason about,
problems such as the following. Sometimes we can even compute the
probabilites involved by evaluating the DSL expressions.

\begin{enumerate}[label=\arabic*.]
\item Assume you throw two 6-faced dice, what is the probability that
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
\item Define the events whose probabilities we will consider.
\item Evaluate such probabilities.
\end{itemize}

Depending on the context, we use the word ``situation'' or ``outcome''
for the same mathematical objects. The word ``outcome'' evokes some
experiment, explicitly performed; and the ``outcome'' is the situation
after the experiment is over. When we use the word ``situation'' there
is not necessarily an explict experiment, but something happens
according to a specific scenario. We consider the situation at the end
of the scenario in question.

\subsection{Sample spaces}


Generally textbook problems involving probability involve the
description of some situation, with an explicit uncertainty, including
the outcome of certain measures, then the student is asked to compute
the probability of some event.

It is common to refer to a sample space by the labels \(S\), $\Omega$,
or \(U\). While this space of events underpins modern understandings
of probability theory, textbooks sometimes give a couple of examples
involving coin tosses and promptly forget the concept of sample space, in the body of
the text. Here we will instead develop this concept using our DSL
methodology.  Once this is done, we'll see that an accurate model of
the sample space is our essential tool to solve probability problems.

Our first task is to describe the possible structure of sample spaces,
and model them as a DSL.

We will use a data type to represent spaces. This type is indexed by the
underlying Haskell type of possible outcomes.
%
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
die :: Int -> Space Int
die n = Finite [1..n]
d6 = die 6
\end{code}
%
In particular the space |point x| is the space with a single point:
\begin{code}
point :: a -> Space a
point x = Finite [x]
\end{code}

\paragraph{Scaling space}
If the die is well-balanced, then all cases have the same
probability (or probability mass) in the space. But this is not always
the case.  Hence we need a way to represent this. We use the
following combinator: \footnote{This is in fact scaling, as defined in
  the LinAlg Chapter --- the spaces over a given domain form a vector
  space. However, we choose not to use this point of view, because we
  are generally not interested in the vector space structure of
  probability spaces. Furthermore, using this terminology risks adding more
  confusion than otherwise. In particular, the word ``scale'' could be
  misunderstood as scaling the \emph{value} of a numerical
  variable. Instead here we scale densites, and use |Factor| for this
  purpose.  } \TODO{Consider the alternative presentation?}
\begin{spec}
Factor :: REAL -> Space ()
\end{spec}
Its underlying type is the unit type |()|, but its mass (or
density) is given by the a real number.

On its own, |Factor| may appear useless, but we can setup the |Space| type
so that this mass or density can depend on (previously introduced)
spaces.
\paragraph{Product of spaces}
As a first instance of dependency, we introduce the product of two spaces as follows:
\begin{code}
prod :: Space a -> Space b -> Space (a,b)
\end{code}

For example, the possible outcomes of the ``experiment'' of throwing
two 6-faced dice are represented as follows:\footnote{The use of a
  pair corresponds to the fact that the two dice can be identified individually.}
%
\begin{code}
twoDice :: Space (Int,Int)
twoDice = prod d6 d6
\end{code}
%
But let's say now that we know the sum to be greater than 7. We can
define the following parametric space, which has a mass 1 if the
condition is satisfied and 0 otherwise. (This space is trivial in the
sense that no uncertainty is involved.)
\begin{code}
sumAbove7 :: (Int,Int) -> Space ()
sumAbove7 (x,y) = Factor (if x+y > 7 then 1 else 0)
\end{code}

We now want to take the product of the |twoDice| space and the |sumAbove7|
space; but the issue is that |sumAbove7| \emph{depends} on the outcome
of |twoDice|. To support this we need a generalisation of the
product which we call ``Sigma'' (|Sigma|) because of its
  similarity of structure with the summation operation.
\begin{spec}
Sigma :: Space a -> (a -> Space b) -> Space (a,b)
\end{spec}
Hence:
\begin{code}
problem1 :: Space ((Int, Int), ())
problem1 = Sigma twoDice sumAbove7
\end{code}
The values of the dice are the same as in |twoDice|, but the density of
any sum less than 7 is brought down to zero.

We can check that the product of spaces is a special case of |Sigma|:
\begin{code}
prod a b = Sigma a (const b)
\end{code}
If we compare the above to the usual sum notation, the right-hand-side would be
\(\sum_{i\in a} b\) which is the sum of |card a| copies of |b|, thus a
kind of ``product'' of |a| and |b|.

\paragraph{Projections}
In the end we may not be interested in all values and hide some of them. For this purpose we use the following combinator:
\begin{spec}
Project :: (a -> b) -> Space a -> Space b
\end{spec}
A typical use is |Project fst :: Space (a,b) -> Space a|.

\paragraph{Real line}
Before we continue, we may also add a way to represent real-valued
spaces, with the same probability for every real number.
\begin{spec}
RealLine :: Space REAL
\end{spec}

\paragraph{Summary}
In sum, we have a datatype for the abstract syntax of ``space expressions'':

\begin{code}
data Space a where
  Finite    :: [a] -> Space a
  Factor    :: REAL -> Space ()
  Sigma     :: Space a -> (a -> Space b) -> Space (a,b)
  Project   :: (a -> b) -> Space a -> Space b
  RealLine  :: Space REAL
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
%format <*> = "\mathbin{" < "\!\!" * "\!\!" > "}"
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

So far we have only defined spaces, not used them.
%
In section~\ref{sec:semanticsOfSpaces} we will see how to compute the |measure :: Space a -> REAL| of a space but before that we will talk about another important notion in probability theory: a distribution.
%
A distribution is a space whose total mass (or measure) is equal to 1.
\begin{code}
isDistribution :: Space a -> Bool
isDistribution s = measure s == 1
\end{code}
We may use the following type synonym to indicate distributions:
\begin{code}
type Distr a = Space a
\end{code}

Let us define a few useful distributions.
%
First, we present the uniform distribution among a finite set of elements.
%
It is essentially the same as the |Finite| space, but we scale every element so that the total measure comes down to |1|.
%
\begin{code}
factorWith :: (a -> REAL) -> Space a -> Space a
factorWith f s = do {x <- s; Factor (f x); return x}
scale :: REAL -> Space a -> Space a
scale c = factorWith (const c)

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

Another useful discrete distribution is the Bernoulli distribution of parameter |p|.
%
It is a distribution whose value is |True| with probability |p| and and |False| with probability |1-p|.
%
Hence it can be used to represent a biased coin toss.
\begin{code}
bernoulli :: REAL -> Distr Bool
bernoulli p = factorWith  (\b -> if b then p else 1-p)
                          (Finite [False,True])
\end{code}

%format mu = "\mu"
%format sigma = "\sigma"
Finally we can show the normal distribution with average |mu| and standard deviation |sigma|.
\begin{code}
normal :: REAL -> REAL -> Distr REAL
normal mu sigma = factorWith (normalMass mu sigma) RealLine

normalMass :: Floating r =>  r -> r -> r -> r
normalMass mu sigma x = exp(- ( ((x - mu)/sigma)^2 / 2)) / (sigma * sqrt (2*pi))
\end{code}

In some textbooks, distributions are sometimes called ``random
variables''. However we consider this terminology to be misguided ---
random variables will be defined precisely later on.

We could try to define the probabilities or densities of possible
values of a distribution, say |distributionDensity :: Space a -> (a -> REAL)|, and from
there define the expected value (and other statistical moments), but
we'll take another route.

\subsection{Semantics of spaces}\label{sec:semanticsOfSpaces}
First, we come back to general probability spaces without the restriction of |measure| one.
%
We define a function |integrator|, which generalises the notions of weighted sum, and weighted integral.
%
When encountering |Finite| spaces, we sum; when encountering |RealLine| we integrate.
%
When encountering |Factor| we will adjust the weights.
%
The integrator of a product (in general |Sigma|) is the nested integration of spaces.
%
The weight is given as a second parameter to |integrator|, as a function mapping elements of the space to a real value.

\TODO{PJ: I'd prefer swapping the argument order.}
\begin{code}
integrator :: Space a -> (a -> REAL) -> REAL
integrator (Finite a)     g = bigsum a g
integrator (RealLine)     g = integral g
integrator (Factor f)     g = f * g ()
integrator (Sigma a f)    g = integrator a $ \x -> integrator (f x) $ \y -> g (x,y)
integrator (Project f a)  g = integrator a (g . f)


integr :: (a -> REAL) -> Space a -> REAL
integr g (Finite a)     = bigsum a g
integr g (RealLine)     = integral g
integr g (Factor f)     = f * g ()
integr g (Sigma a f)    = integr (\x -> integr (\y -> g (x,y)) (f x)) a
integr g (Project f a)  = integr (g . f) a

\end{code}
%
The above definition relies on the usual notions of sums and integrals.
%
We can define the sum of some terms, for finite lists, as follows:
\begin{code}
bigsum :: [a] -> (a -> REAL) -> REAL
bigsum xs f = sum (map f xs)
\end{code}

We use also the definite integral over the whole real line.
%
However we will leave this concept undefined at the Haskell level --- thus whenever using real-valued spaces, our defintions are not usable for numerical computations, but for symbolic computations only.
\begin{code}
integral :: (REAL -> REAL) -> REAL
integral = undefined
\end{code}

The simplest quantity that we can compute using the integrator is the measure of the space --- the total mass.
%
To compute the measure of a space, we can simply integrate the constant |1| (so only mass matters).
\begin{code}
measure :: Space a -> REAL
measure d = integrator d (const 1)
\end{code}

As a sanity check, we can compute the measure of a Bernoulli
distribution (we use the |>>>| notation to indicate an expression
being computed) and find that it is indeed |1|.
\begin{code}
-- |>>> measure (bernoulli 0.2)|
-- 1.0
\end{code}

The integration of |id| over a real-valued distribution yields its
expected value.\footnote{We reserve the name |expectedValue| for the
  expected value of a random variable, defined later.}
%
\begin{code}
expectedValueOfDistr :: Distr REAL -> REAL
expectedValueOfDistr d = integrator d id

-- |>>> expectedValueOfDistr dieDistr|
-- 3.5
\end{code}

Exercise: compute symbolically the expected value of the |bernoulli| distribution.

\paragraph{Properties of spaces.} We can use the definitions to show some useful calculational properties of spaces.

\TODO{Format the lemmas}

\textbf{integrator/bind lemma}: |integrator (s >>= f) g == integrator s $ \x -> integrator (f x) g|
\begin{spec}
  integrator (s >>= f) g
= {- by Def. of bind |(>>=)| -}
  integrator (Project snd (Sigma s f)) g
= {- by Def. of integrator/project -}
  integrator (Sigma s f) (g . snd)
= {- by Def. of integrator/Sigma -}
  integrator s $ \x -> integrator (f x) $ \y -> (g . snd) (x,y)
= {- snd/pair -}
  integrator s $ \x -> integrator (f x) $ \y -> g y
= {- composition -}
  integrator s $ \x -> integrator (f x) g
\end{spec}
The same in |do| notation: |integrator (do x <- s; t) g == integrator s $ \x -> integrator t g|

\textbf{integrator/return lemma}: |integrator (return x) g == g x|
\begin{spec}
  integrator (return x) g
= {- by Def. of |return| -}
  integrator (Finite [x]) g
= {- integrator/Finite -}
  sum (map g) [x]
= {- by Def. of sum/map -}
  g x
\end{spec}

\textbf{integrator/fmap lemma}: |integrator (fmap f s) g == integrator s (g . f)|

\begin{spec}
  integrator (fmap f s) g
= {- by Def. of |fmap| -}
  integrator (s >>= (return . f)) g
= {- by integrator/bind lemma -}
  integrator s $ \x -> integrator (return (f x)) g
= {- by integrator/return lemma -}
  integrator s $ \x -> g (f x)
= {- definition of |(.)| -}
  integrator s (g . f)
\end{spec}

% $ emacs


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
this notation can be confusing because it leaves the space of
situations completely implicit. That is, it is not clear how
|t| depends on the outcome of experiments.

With our DSL approach, we make this dependency completely explicit. For
example, the expected value of real-valued random variable takes the space of outcomes as
its first argument:
\begin{code}
expectedValue :: Space a -> (a -> REAL) -> REAL
expectedValue s f = integrator s f / measure s
\end{code}

For instance, we can use the above to compute the expected value of
the sum of two dice throws:
\begin{code}
-- |>>> expectedValue twoDice (\ (x,y) -> fromIntegral (x+y))|
-- 7.0
\end{code}

Essentially, what the above does is computing the weighted
sum/integral of |f(x)| for every point |x| in the space.  Because |s|
is a space (not a distribution), we must normalise the result by
dividing by its measure.

Exercise: define various statistical moments (variance, skew, curtosis, etc.)

\subsection{Events and probability}

In textbooks, one typically finds the notation |P(e)| for the
probability of an \emph{event} |e|. Again, the space of situations |s|
is implicit as well as the dependence between |e| and |s|.

Here, we define events as \emph{boolean-valued} random variables.

Thus an event |e| can be defined as a boolean-valued function |e : a
-> Bool| over a space |s : Space a|. Assuming that the space |s|
accurately represents the relative mass of all possible situations,
there are two ways to define the probability of |e|.

The first defintion is as the expected value of |indicator . e|,
where |indicator| maps boolean to reals as follows:
\begin{code}
indicator :: Bool -> REAL
indicator True   = 1
indicator False  = 0

probability1 :: Space a -> (a -> Bool) -> REAL
probability1 d e = expectedValue d (indicator . e)
\end{code}

The second definition of probability is as the ratio of the measure of
the subspace where |e| holds, the measure and the complete space.

\begin{code}
probability2 :: Space a -> (a -> Bool) -> REAL
probability2 s e = measure (Sigma s (isTrue . e)) / measure s

isTrue :: Bool -> Space ()
isTrue = Factor . indicator
\end{code}
where |isTrue c| is the subspace which has measure 1 if |c|
is true and 0 otherwise.
%
The subspace of |s| where |e| holds is then the first projection of
|Sigma s (isTrue . e)|.
%
\begin{code}
subspace :: (a->Bool) -> Space a -> Space a
subspace e s = Project fst (Sigma s (isTrue . e))
\end{code}

We can show that if |s| has a non-zero measure, then the two definitions are equivalent:

Lemma: |measure s * probability s e = measure (Sigma s (isTrue . e))|
Proof:
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
= {- Def. of integrator -}
  integrator s (\x -> integrator (Factor (indicator (e x))) (\y -> const 1 (x,y))) / measure s
= {- Def. of isTrue -}
  integrator s (\x -> integrator (isTrue (e x)) (\y -> const 1 (x,y))) / measure s
= {- Def. of integrator -}
  measure (Sigma s (isTrue . e)) / measure s
\end{spec}

It will be often convenient to define a space whose underlying set is
a boolean value and compute the probability of the identity event:
\begin{code}
probability :: Space Bool -> REAL
probability d = expectedValue d indicator
\end{code}

Sometimes one even finds in the literature and folklore the notation
|P(v)|, where |v| is a value, which stands for |P(t=v)|, for an
implicit random variable |t|. Here even more creativity is required
from the reader, who must also infer which random variable the author
means.

\subsection{Conditional probability}

Another important notion is conditional probability, written
$P(F ∣ G)$ and read ``probability of |f| given |g|''.

We can define the conditional probability of |f| given |g| by taking
the probability of |f| in the sub space where |g| holds:

\begin{code}
condProb :: Space a -> (a -> Bool) -> (a -> Bool) -> REAL
condProb s f g = probability1 (subspace g s) f
\end{code}

We find the above defintion more intuitive than the more usual $P(F∣G)
 = P(F∩G) / P(G)$. However, this last equality can be proven, by calculation:

Lemma:  |condProb s f g == probability s (\y -> f y && g y) / probability s g|

Proof:
\TODO{Possibly split into helper lemma(s) to avoid diving ``too deep''.}
\begin{spec}
  condProb s f g
= {- Def of condProb -}
  probability1 (subspace g s) f
= {- Def of subspace -}
  probability1 (Sigma s (isTrue . g)) (f . fst)
= {- Def of probability1 -}
  expectedValue (Sigma s (isTrue . g)) (indicator . f . fst)
= {- Def of expectedValue -}
  (1/measure(Sigma s (isTrue . g))) * (integrator (Sigma s (isTrue . g)) (indicator . f . fst))
= {- Def of integrator (Sigma) -}
  (1/measure s/probability s g) * (integrator s $ \x -> integrator (isTrue . g) $ \y -> indicator . f . fst $ (x,y))
= {- Def fst -}
  (1/measure s/probability s g) * (integrator s $ \x -> integrator (isTrue . g) $ \y -> indicator . f $ x)
= {- Def of isTrue -}
  (1/measure s/probability s g) * (integrator s $ \x -> indicator (g x) * indicator (f x))
= {- Property of indicator -}
  (1/measure s/probability s g) * (integrator s $ \x -> indicator (\y -> g y &&  f y))
= {- associativity of multiplication -}
  (1/probability s g) * (integrator s $ \x -> indicator (\y -> g y &&  f y)) / measure s
= {- Definition of probability1 -}
  (1/probability s g) * probability1 s (\y -> g y &&  f y)
\end{spec}
% emacs wakeup $

\subsection{Examples}

\subsubsection{Dice problem}

We will use the monadic interface to define the experiment, hiding all
random variables except the outcome that we care about (is the product greater than 10?):
\begin{code}
diceSpace :: Space Bool
diceSpace = do
  x <- d6  -- balanced die 1
  y <- d6  -- balanced die 2
  isTrue (x + y >= 7)  -- observe that the sum is >= 7
  return (x * y >= 10) -- consider only the event ``product >= 10''
\end{code}
Then we can compute its probability:
\begin{code}
diceProblem :: REAL
diceProblem = probability diceSpace

-- |>>> diceProblem|
-- 0.9047619047619047
\end{code}

Some helpers: \TODO{Merge with text above to explain more}
\begin{code}
p1 (x,y) = x+y >= 7
p2 (x,y) = x*y >= 10
test1     = measure (prod d6 d6 >>= isTrue . p1)                       -- 21
test2     = measure (prod d6 d6 >>= isTrue . p2)                       -- 19
testBoth  = measure (prod d6 d6 >>= isTrue . \xy -> p1 xy && p2 xy)    -- 19
prob21    = condProb (prod d6 d6) p2 p1                                -- 19/21
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
  isUser <- bernoulli 0.005  -- model the distribution of drug users
  testPositive <- bernoulli (if isUser then 0.99 else 0.01)
    -- model test accuracy
  isTrue testPositive        -- we have ``a positive test'' by assumption
  return isUser              -- we're interested in |isUser|.
\end{code}
The probability is computed as usual:
\begin{code}
userProb :: REAL
userProb = probability drugSpace

-- |>>> userProb|
-- 0.33221476510067116
\end{code}

Perhaps surprisingly, we never needed the Bayes theorem to solve the
problem. Indeed, the Bayes theorem is already incorporated in our
defintion of |probability|.

\subsection{Monty Hall}
We can model the Monty Hall problem as follows: A correct model is
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

The Monty Hall is sometimes considered paradoxical: it is strange that
changing one's mind can change the outcome. The crucial point is that
Monty can never show a door which contains the prize. To illustrate,
an \emph{incorrect} way to model the Monty Hall problem, which still
appears to follow the example point by point, is the following:
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
a door before the player made its first choice.

\subsection{Advanced problem}
Consider the following problem: how many times must one throw a coin
before one obtains 3 heads in a row.

We can model the problem as follows:
\begin{code}
coin = bernoulli 0.5

coins = do
  x <- coin
  xs <- coins
  return (x:xs)

threeHeads :: [Bool] -> Int
threeHeads (True:True:True:_) = 3
threeHeads (_:xs) = 1 + threeHeads xs

example' = threeHeads <$> coins

\end{code}
Attempting to evaluate |probability1 threeHeads' (< 5)| does not
terminate. Indeed, we have an infinite list, which translates in
infinitely many cases to consider. So the evaluator cannot solve this
problem.  We have to resort to a symbolic method.  First, we can
unfold the definitions of |coins| in |threeHeads|, and obtain:

\begin{code}
threeHeads' = helper 3

helper 0 = return 0
helper n = do
  h <- coin
  (1 +) <$> if h
    then helper (n-1)
    else helper 3 -- when we have a tail, we start from scratch
\end{code}

Evaluating the probability still does not terminate: we no longer have
an infinite list, but we still have infinitely many possibilities to
consider: however small, there is always a probability to get a
``tail'' at the wrong moment, and the evaluation must continue.

To solve such problems symbolically, we define a so-called ``generator
function''. In our case |f m n|, equal to the probability that |helper
m| returns |n|.


\begin{code}
f m n = probability1 (helper m) (== n)
\end{code}

We can then evaluate the rhs symbolically;
\begin{spec}
=  probability1 (helper (m+1)) (== n)
 {- Def. of probability1 -}
=  integrator (helper m) (indicator . (== n))
 {- Lemma: integrator / bind -}
=  integrator coin $ \h -> integrator ((1+) <$> if h then helper m else helper 3) (indicator . (== n))
 {- Lemma: integrator / fmap -}
=  integrator coin $ \h -> integrator (if h then helper m else helper 3) (indicator . (== n) . (+1))
 {- Lemma: integrator / if -}
=  integrator coin $ \h -> if h then integrator (helper m) (indicator . (== n) . (+1)) else integrator (helper 3) (indicator . (== n) . (+1))
 {- Def. coin -}
=  integrator (bernoulli 0.5) $ \h -> if h then integrator (helper m) (indicator . (== n) . (+1)) else integrator (helper 3) (indicator . (== n) . (+1))
 {- Lemma: integrator / bernoulli -}
=  0.5 * integrator (helper m) (indicator . (== n) . (+1)) + 0.5 * integrator helper 3 (indicator . (== n) . (+1))
 {- Expand composition   and    n==x+1 iff. n-1==x -}
=  0.5 * integrator (helper m) (indicator . (== (n-1))) + 0.5 * integrator helper 3 (indicator . (== (n-1))
 {- Def of |f m n| -}
=  0.5 * f m (n-1) + 0.5 * f 3 (n-1)
\end{spec}
% $ emacs

Hence we obtain the following system of recursive equations:

\begin{spec}
f 0 0 = 1
f 0 n = 0  -- n ≠ 0
f (m+1) (n+1) = 0.5 * f m n + 0.5 * f 3 n
\end{spec}
% TODO: what about f (m+1) 0 ?


This function will \emph{still} not terminate. However, there exist
mathematical tools to solve such equations. (For example we could
cut-off the evaluation once the precision gets good enough.)

Regardless, the take home message is that, thanks to our formalism we
can obtain a system of recurrent equations which would yield a
solution to the problem.

\subsection{Independent events}
One possible way to define for independent events is as follows.  $E$
is independent from $F$ iff $P(E ∣ F) = P(E)$.  We can express this
definition in our DSL as follows:

\begin{code}
independentEvents :: Space a -> (a -> Bool) -> (a -> Bool) -> Bool
independentEvents s e f = probability1 s e == condProb s e f
\end{code}

According to \citet{IntroProb_Grinstead_Snell_2003}, two events
independent iff.  $P(E ∩ F) = P(E) · P(F)$.

Using our language, we would write instead:

\begin{spec}
probability1 s (\x -> e x && f e) == probability1 s e * probability1 s f
\end{spec}

Proof.

In the left to right direction:
\begin{spec}
  P(E ∩ F)
= {- by def. of cond. prob -}
  P(E ∣ F) · P (F)
= {- by def. of independent events -}
  P(E) · P (F)
\end{spec}

This part of the proof is written like so using our DSL:
\begin{spec}
probability1 s (\x -> e x && f e)
= condProb s e f * probability1 s f
= probability1 s e * probability1 s f
\end{spec}

We note that at this level of abstraction, the proofs follow the same
structure as the textbook proofs --- the underlying space |s| is
constant.


In the right to left direction:
\begin{spec}
  P(E ∣ F)
= {- by def. of cond. prob -}
  P(E ∩ F) / P (F)
= {- by assumption -}
  P(E) · P (F) / P (F)
= {- by computation -}
  P(E)
\end{spec}

Exercise: express the rest of the proof using our DSL

\subsection{Continuous spaces and equality}
TODO
\begin{spec}
Dirac :: REAL -> Space


integrator (Dirac x) f == f x

-- Not good enough because we can't relate variables. IE; how do we do:


uniform = do
  x <- RealLine
  isTrue (x >= 0 && x < 1)
  return x

exampleD0 = do
  x <- uniform
  y <- uniform
  equal x (2*y)  -- Dirac ???
  -- Dirac (x-y) does not work
  return (x <= 0.5)


integrator exampleD0 f =
  integrator uniform $ \x ->
  integrator uniform $ \y ->
  integrator ??? $  \x ->
  integrator (return (x <= 0.5))

\end{spec}


% Local Variables:
% dante-methods : (bare-ghci)
% mode: literate-haskell
% End:
