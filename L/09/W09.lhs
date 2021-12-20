\chapter{Probability Theory}
\label{ch:probability-theory}

%if False
\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module DSLsofMath.W09 where

import Control.Monad (ap)
import Data.List ((\\))

type REAL = Double -- pretend...

-- poorest man's equational reasoning.

(===) :: a -> a -> a
_ === y = y

infixl 1 ===

\end{code}
%endif

We have by now acquired a firm grip on DSL notions and several
mathematical domains.
%
In this chapter, will apply the DSL methodology once more, and now to
the domain of probability theory.
%
By building a DSL from scratch, we will not only clarify notations for
(conditional) probabilities, such as \(P(A ∣ B)\), but we will also be
able to describe and reason about problems such as those in the
following list.
%
For them we will even be able to compute the probabilities involved by
evaluating the DSL expressions.

\begin{enumerate}[label=\arabic*.]
\item Assume you throw two 6-faced dice, what is the probability that
  the product is greater than 10 if their sum is greater than 7?
\label{ex:dice}
\item Suppose that a test for using a particular drug is 99\%
  sensitive and 99\% specific.
  %
  That is, the test will produce 99\% true positive results for drug
  users and 99\% true negative results for non-drug users.
  %
  Suppose that 0.5\% of people are users of the drug.
  %
  What is the probability that a randomly selected individual with a
  positive test is a drug user?%
  %
  \footnote{Example found in
    \href{https://en.wikipedia.org/wiki/Bayes'_theorem}{Wikipedia
      article on Bayes' theorem}}
\label{ex:drugtest}

\item Suppose you’re on Monty Hall’s \emph{Let’s Make a Deal!}
  %
  You are given the choice of three doors, behind one door is a car,
  the others, goats.
  %
  You pick a door, say 1, Monty opens another door, say 3, which has a
  goat.
  %
  Monty says to you “Do you want to pick door 2?”
  %
  Assuming that the car is more desirable than the goat, is it to your
  advantage to switch your choice of doors?
\label{ex:monty}
\end{enumerate}

Our method will be to:\nopagebreak
\begin{itemize}
\item Describe the space of possible situations, or outcomes.
\item Define the events whose probabilities we will consider.
\item Evaluate such probabilities.
\end{itemize}


\section{Sample spaces}
\index{DSL!sample spaces}%
Generally, textbook problems involving probability involve the
description of some scenario or experiment, with an explicit
uncertainty, including the outcome of certain measurements.%
%
\footnote{Depending on the context, we use the word ``situation'' or
``outcome'' for the same mathematical objects.
%
The word ``outcome'' evokes some experiment, explicitly performed; and
the ``outcome'' is the situation after the experiment is over.
%
When we use the word ``situation'' there is not necessarily an
explicit experiment, but something happens according to a specific
scenario.
%
In this case we call the ``situation'' the state of affairs at end of
the scenario in question.}
%
Then the reader is asked to compute the probability of some event.

It is common to refer to a sample space by the labels \(S\), $\Omega$,
or \(U\), but in this chapter we will define many such spaces, and
therefore we will use descriptive names instead.
%
While the concept of a space of events underpins modern understandings
of probability theory, textbooks sometimes give a couple of examples
involving coin tosses and promptly forget the concept of sample space
in the body of the text.
%
Here we will instead develop this concept using our DSL methodology.
%
Once this is done, we will be able to see that an accurate model of
the sample space is an essential tool to solve probability problems.

Our first task is to describe the possible structure of sample spaces,
and model them as a DSL.
%
To this end we will use a data type to represent spaces.
%
This type is indexed by the underlying Haskell type of possible
outcomes.
%
Hence |Space| maps this underlying type to another type:
%
\begin{spec}
Space :: Type -> Type
\end{spec}
We then carry on and define the constructions which inhabit the above type.

\paragraph{Finite space}
In Example~\ref{ex:dice}, we consider dice with 6 faces.
%
For this we define a constructor |Finite| embedding a list of possible
outcomes into a space:
\begin{spec}
Finite :: [a] -> Space a
\end{spec}
The scenario (or ``experiment'') corresponding to throwing a general
|n|-sided die is then represented by the space |die n|:
\begin{code}
die :: Int -> Space Int
die n = Finite [1..n]
d6 = die 6
\end{code}
%
In particular the space |point x| is the space with a single point ---
only trivial probabilities (zero or one) are involved here:
\begin{code}
point :: a -> Space a
point x = Finite [x]
\end{code}

\paragraph{Scaling space}
%
If the die is well-balanced, then all cases have the same probability
(or probability mass) in the space, and this is what we have modelled
above.
%
But this is not always the case.
%
Hence we need a way to represent such imbalances.
%
We use the following combinator:%
\footnote{This is in fact \emph{scaling}, as defined in
\cref{sec:LinAlg}.
%
  Indeed, there is a vector space of measurable spaces, where each
  space is one vector.
  %
  However, we choose not to use this terminology, because we are
  generally not interested in the vector space structure of
  probability spaces.
  %
  There is also potential for confusion, because |Factor| does not
  scale the \emph{points} in the space.
  %
  What it does is to scale the \emph{probability mass} associated with
  each such points.
  %
}
\begin{spec}
Factor :: REAL -> Space ()
\end{spec}
Its underlying type is the unit type |()|, but its mass (or density)
is given by a real number.

On its own, |Factor| may appear useless, but we can setup the |Space|
type so that this mass or density can depend on (previously
introduced) spaces.

\paragraph{Product of spaces}
%
As a first instance of a dependency, we introduce the product of two
spaces, as follows:
\begin{code}
prod :: Space a -> Space b -> Space (a,b)
\end{code}

For example, the ``experiment'' of throwing two 6-faced dice is
represented as follows:%
\footnote{The use of a pair corresponds to the fact that the two dice
can be identified individually.}
%
\begin{code}
twoDice :: Space (Int,Int)
twoDice = prod d6 d6
\end{code}
%
But let's say now that we know that the sum is greater than 7.
%
We can then define the following parametric space, whose single point
has mass 1 if the condition is satisfied and 0 otherwise.
%
(This space is trivial in the sense that no uncertainty is involved.)
\begin{code}
sumAbove7 :: (Int,Int) -> Space ()
sumAbove7 (x,y) = Factor (if x+y > 7 then 1 else 0)
\end{code}

We now want to take the product of the |twoDice| space and the
|sumAbove7| space; but the issue is that |sumAbove7| \emph{depends} on
the outcome of |twoDice|.
%
To support this dependency we need a generalisation of the product
which we call ``Sigma'' (|Sigma|) because of its similarity of
structure with ``big sum''.
\begin{spec}
Sigma :: Space a -> (a -> Space b) -> Space (a,b)
\end{spec}
Hence:
\begin{code}
problem1 :: Space ((Int, Int), ())
problem1 = Sigma twoDice sumAbove7
\end{code}
The values of the dice are the same as in |twoDice|, but the density
of any sum less than 7 is brought down to zero.

We can check that the product of spaces is a special case of |Sigma|:
%*TODO: define |card| and |support| as alternative semantic functions
%
\begin{code}
prod a b = Sigma a (const b)
\end{code}
% If we call |card a| the cardinality of the support type of space |a|,
% then
% \begin{spec}
%   card (prod a b) == card a × card b
% \end{spec}
% and in the general case
% %{
% %format (support (a)) = "\lvert{}" a "\rvert{}"
% \begin{spec}
%   card (Sigma a f) == {-"\sum_{i\in"-} (support(a)) {-"}"-} card (f i)
% \end{spec}
% where |support a| is the set of points in the space |a|.
% %}  
\paragraph{Projections}
In the end we may not be interested in all values and hide some of
them.
%
For this purpose we use the following combinator:
\begin{spec}
Project :: (a -> b) -> Space a -> Space b
\end{spec}
A typical use is |Project fst :: Space (a,b) -> Space a|, ignoring the
second component of a pair.

\paragraph{Real line}
Before we continue, we may also add a way to represent real-valued
spaces, which assign an even probability density across all reals.
\begin{spec}
RealLine :: Space REAL
\end{spec}

\paragraph{Summary}
We have already completed the description of a DSL for spaces, whose
abstract syntax is captured by the following datatype:
%
\index{abstract syntax tree}%
%
\begin{code}
data Space a where
  Finite    :: [a] -> Space a
  Factor    :: REAL -> Space ()
  Sigma     :: Space a -> (a -> Space b) -> Space (a,b)
  Project   :: (a -> b) -> Space a -> Space b
  RealLine  :: Space REAL
\end{code}

\section{\extraMaterial Monad Interface}

Seasoned functional programmers will be aware of monadic interfaces.
%
For them, it may be useful to know that one can easily provide a
monadic interface for spaces.
%
The implementation is the following:

%format <*> = "\mathbin{" < "\!\!" * "\!\!" > "}"
\begin{code}
instance Functor Space where
  fmap    = Project
instance Applicative Space where
  pure    = point
  (<*>)   = Control.Monad.ap
instance Monad Space where
  a >>= f = Project snd (Sigma a f)
\end{code}

\begin{exercise}
  \label{ex:monad-laws}
  Prove the functor and monad laws for the above definitions.
  %
  (Use semantic equality, see \cref{def:space-equality}.)
%**TODO: semantic not defined yet!
\end{exercise}

\section{Distributions}
\index{probability distribution}%

So far we have defined several spaces, but we have not used them to
compute any probability.
%
We set out to do this in this section.
%
In section~\ref{sec:semanticsOfSpaces} we will see how to compute the
total mass (or |measure :: Space a -> REAL|) of a space.
%
But before that we will talk about another important notion in
probability theory: that of a distribution.
%
A distribution is a space whose |measure| is equal to 1.
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
First, we present the uniform distribution among a finite set of
elements.
%
It is essentially the same as the |Finite| space, but we scale it
so that the total measure comes down to |1|.
%
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

Scaling is a special case of the more general operation
|marginaliseWith| which applies a factor to every point, and ignores
the unit type from |Factor|.
%
This operation is often called ``marginalisation'', in the jargon of
Bayesian reasoning.
%
\begin{code}
marginaliseWith :: (a -> REAL) -> Space a -> Space a
marginaliseWith f s = Project fst (Sigma s (\x -> Factor (f x)))

scale :: REAL -> Space a -> Space a
scale c = marginaliseWith (const c)
\end{code}

If the scaling is all-or-nothing, we have the following version, which
will be useful later.
\begin{code}
observing :: (a -> Bool) -> Space a -> Space a
observing f = marginaliseWith (indicator . f)

indicator :: Bool -> REAL
indicator True   = 1
indicator False  = 0
\end{code}

Another useful discrete distribution is the Bernoulli distribution of
parameter~|p|.
%
It is a distribution whose value is |True| with probability |p| and
|False| with probability |1-p|.
%
Hence it can be used to represent a biased coin toss.
\begin{code}
bernoulli :: REAL -> Distr Bool
bernoulli p = marginaliseWith  (\b -> if b then p else 1-p)
                               (Finite [False,True])
\end{code}

%format mu = "\mu"
%format sigma = "\sigma"
Finally we can define the normal distribution with average |mu| and
standard deviation |sigma|.
\begin{code}
normal :: REAL -> REAL -> Distr REAL
normal mu sigma = marginaliseWith (normalMass mu sigma) RealLine

normalMass :: Floating r =>  r -> r -> r -> r
normalMass mu sigma x = exp(-y^2/2) / (sigma * sqrt (2*pi))
  where  y = (x - mu)/sigma
\end{code}

In scientific literature, distributions are sometimes called ``random
variables''.
%
However we consider this terminology to be misleading --- random
variables will be defined precisely later on.

We could try to define the probabilities or densities of possible
values of a distribution, say |distDensityAt :: Space a -> (a ->
REAL)|, and from there define the expected value (and other
statistical moments), but we'll take another route.

\section{Semantics of spaces}\label{sec:semanticsOfSpaces}
First, we come back to general probability spaces without restriction
on their |measure|: it does not need to be equal to one.
%
We define a function |integrator|, which generalises the notions of
weighted sum, and weighted integral.
%
When encountering |Finite| spaces, we sum; when encountering
|RealLine| we integrate.
%
When encountering |Factor| we will adjust the weights.
%
The integrator of a product (in general |Sigma|) is the nested
integration of spaces.
%
The weight is given as a second parameter to |integrator|, as a
function mapping elements of the space to a real value.

In code, we obtain the following:%
\footnote{You may want to come back to \cref{sec:big-operators} to see
  how to deal with the integration (or summation) variable and what it
  means for the type of the integrator.}
% \TODO{PJ: I'd prefer swapping the argument order.}
% JPB: It's generally a better idea to put the "continuation" last, because it's usually a much longer argument.
% Also this is the order of arguments in sum and integrals.
%*TODO: Can we replace REAL by a Ring? a Field?
\begin{code}
integrator :: Space a -> (a -> REAL) -> REAL
integrator (Finite a)     g =  bigsum a g
integrator (RealLine)     g =  integral g
integrator (Factor f)     g =  f * g ()
integrator (Sigma a f)    g =  integrator a      $ \x ->
                               integrator (f x)  $ \y ->
                               g (x,y)
integrator (Project p a)  g =  integrator a (g . p)
\end{code}
%
In calculuations we will often use the notation 
%{
%format integrator (s) = "\int\{" s "\}"
|integrator s g|
%}
for |integrator s g|.
%
This shows that we are dealing with a generalisation of integration to
more complicated domains (spaces).
%
For simplicity we use |REAL| here, but the definitions would work for
any field.

% integr :: (a -> REAL) -> Space a -> REAL
% integr g (Finite a)     = bigsum a g
% integr g (RealLine)     = integral g
% integr g (Factor f)     = f * g ()
% integr g (Sigma a f)    = integr (\x -> integr (\y -> g (x,y)) (f x)) a
% integr g (Project f a)  = integr (g . f) a
%
The above definition relies on the usual notions of sum (|bigsum|) and
|integral|.
%
We can define the sum of some terms, for finite lists, as follows:
\begin{code}
bigsum :: [a] -> (a -> REAL) -> REAL
bigsum xs f = sum (map f xs)
\end{code}

We use also the definite integral over the whole real line.
%
However, at the Haskell level, we will leave this concept undefined  ---
thus whenever using real-valued spaces, our defintions are not usable
for numerical computations, but for symbolic computations only.
%
(If we had a syntax for the function to integrate, we could do more,
but this would take us too far off track here.)
%
\begin{code}
integral :: (REAL -> REAL) -> REAL
integral = undefined
\end{code}

\index{semantic equality}
This semantics yields a notion of semantic equality for space, which we can define as follows:
%% \begin{definition}[Semantic equality]
\label{def:space-equality}
\begin{spec}
  s₁ === s₂ = Forall g (integrator s₁ g === integrator s₂ g)
\end{spec}
%% \end{definition}



The simplest useful quantity that we can compute using the integrator is the
measure of the space --- its total ``mass'' or ``volume''.
%
To compute the measure of a space, we can simply integrate the
constant |1| (so only the mass of the space matters).
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
expected value.%
\footnote{We reserve the name |expectedValue| for the expected value
  of a random variable, defined later.}
%
\begin{code}
expectedValueOfDistr :: Distr REAL -> REAL
expectedValueOfDistr d = integrator d id

-- |>>> expectedValueOfDistr dieDistr|
-- 3.5
\end{code}

\begin{exercise}
Compute symbolically the expected value of the |bernoulli|
distribution
\end{exercise}

\paragraph{Properties of |integrator|}

We can use calculational reasoning to show some useful properties
of spaces.


% \textbf{integrator/bind lemma}: |integrator (s >>= f) g == integrator s (\x -> integrator (f x) g)|
% \begin{spec}
%   integrator (s >>= f) g
% = {- by Def. of bind |(>>=)| -}
%   integrator (Project snd (Sigma s f)) g
% = {- by Def. of integrator/project -}
%   integrator (Sigma s f) (g . snd)
% = {- by Def. of integrator/Sigma -}
%   integrator s (\x -> integrator (f x) (\y -> (g . snd) (x,y)))
% = {- snd/pair -}
%   integrator s (\x -> integrator (f x) (\y -> g y))
% = {- composition -}
%   integrator s (\x -> integrator (f x) g)
% \end{spec}
% The same in |do| notation: |integrator (do x <- s; t) g == integrator s (\x -> integrator t g)|


% Note that using the definition of integrator/Project lemma with |g==id| we can always
% push the weight function of the integrator into the space: |integrator
% s f == integrator (Project f s) id|.

\begin{lemma}[Linearity of |integrator|]
  \label{lem:integrator-linear}
If |g| is a linear function (addition or multiplication by a
constant), then:
\begin{spec}
integrator s (g . f) == g (integrator s f)
\end{spec}
(or, equivalently,  |integrator s (\x -> g (f x)) = g (integrator s f)|)
\end{lemma}
\begin{proof}
  The proof proceeds by structural induction over |s|.
  % 
  The hypothesis of linearity is used in the base cases.
  % 
  Notably the linearity property of |Finite| and |RealLine| hinges on the
  linearity of sums and integrals.\footnote{Recall that integration is a linear operator in the space of functions (\cref{sec:functions-vector-space}).}
  % 
  The case of |Project| is immediate by definition.
  % 
  The case for |Sigma| is proven as follows:
  \begin{spec}
    integrator (Sigma a f) (g . h)
    = {- By definition -}
    integrator a $ \x -> integrator (f x) $ \y -> g (h (x,y))
    = {- By induction -}
    integrator a $ \x -> g (integrator (f x) $ \y -> h (x,y)
    = {- By induction -}
    g (integrator a $ \x -> (integrator (f x) $ \y -> h (x,y))
    = {- By definition -}
    g (integrator (Sigma a f) h)
  \end{spec}
\end{proof}

%**TODO this does not make sense type-wise in general, only for Num-values spaces
% As a corrolary, |Project| is linear as well:
% %
% if |f| is linear, then |integrator (Project f s) g = f (integrator s g)|

\begin{lemma}[Properties of |measure|]
\label{lem:measure-properties}  
~
\begin{itemize}
\item |measure (Finite xs) == length xs|
\item |measure (prod s t) == measure s * measure t|.
\item If |s| is a distribution and |f x| is a
  distribution for every |x|, then |Sigma s f| is a distribution
\item |s| is a distribution iff. |Project f s| is a distribution.
\end{itemize}
\end{lemma}

The proof of the second item is as follows:
\begin{spec}
   measure (prod s t)
== {- By definition of |prod| -}
   measure (Sigma s (const t))
== {- By definition of measure -}
   integrator (Sigma s (const t)) (const 1)
== {- By definition of integrator for |Sigma| -}
   integrator s $ \x -> integrator (const t x) (const 1)
== {- By definition of of measure, const -}
   integrator s $ \x -> measure t
== {- By linearity of integrator -}
   measure t * integrator s (const 1)
== {- By definition of measure -}
   measure t * measure s
\end{spec}

\begin{exercise}
  \label{ex:integrator-bernouilli}
  Using the above lemmas, prove:
\begin{spec}
  integrator (bernoulli p) f == p * f True + (1-p) * f False
\end{spec}
\end{exercise}

\section{Random Variables}
%
Even though we studied variables in detail (in \cref{sec:types}), it
is good to come back to them for a moment before returning to
\emph{random} variables proper.

According to
Wikipedia\footnote{\url{https://en.wikipedia.org/wiki/Variable},
  retrieved 2021-11-01.}, a variable has a different meaning in
computer science and in mathematics:
\begin{quote}
  Variable may refer to:
  \begin{itemize}
  \item Variable (computer science): a symbolic name associated with a
    value and whose associated value may be changed
  \item Variable (mathematics): a symbol that represents a quantity in
    a mathematical expression, as used in many sciences
  \end{itemize}
\end{quote}

At this stage of the \course{}, we have a pretty good grip on
variables in computer science.
%
In particular, in \cref{sec:types}, we have described a way to reduce
mathematical variables (position \(q\) and velocity \(v\) in
Lagrangian mechanics) to computer science variables.
%
This was done by expressing variables as \emph{functions} of a truly
free variable, time (\(t\)).
%
Time is a ``computer science'' variable in this context: it can be
substituted by any value without ``side effects'' on other variables
(unlike positions (\(q\)) and velocities (\(v\))).

In this light, let us return to random variables.
%
Wikipedia is not very helpful here, so we turn to
\citet{IntroProb_Grinstead_Snell_2003}, who give the following
definition:
\begin{quote}
  A random variable is simply an expression whose value is the outcome
  of a particular experiment.
\end{quote}

This may be quite confusing at this stage.
%
What are those expressions in our DSL? And where does the experiment influence the
variable?
%
Our answer is to use spaces to represent the ``experiments'' that
Grinstead and Snell mention.
%
More specifically, if |s : Space a|, then each possible situation at the
end of the experiment is representable in the type |a| and the space
will specify the mass of each of them (formally, \text{via} the integrator).

Then, a |b|-valued random variable (observed after an experiment
represented by a space |s : Space a|) is a function |f| of type |a ->
b|.
%
Then |f x| is the ``expression'' that Grinstead and Snell refer
to.
%
The (computer science) variable |x| is the outcome, and |s| represents the experiment
--- which is most often implicit in a random variable expressions as
written in a mathematics book.

We can finally define the expected value (and other statistical
moments) of a random variable.

In textbooks, one will often find the notation $E[t]$ for the expected
value of a real-valued random variable |t|.
%
As just mentioned, from our point of view, this notation can be
confusing because it leaves the space of situations completely
implicit.
%
That is, it is not clear how |t| depends on the outcome of
experiments.

With our DSL approach, we make this dependency completely explicit.
%
For example, the expected value of a real-valued random variable takes
the space of outcomes as its first argument:
\begin{code}
expectedValue :: Space a -> (a -> REAL) -> REAL
expectedValue s f = integrator s f / measure s
\end{code}

For instance, we can use the above function to compute the expected value
of the sum of two dice throws:
\begin{code}
expect2D6 = expectedValue twoDice (\ (x,y) -> fromIntegral (x+y))
\end{code}

\begin{exercise}
  Run the above code and check that you obtain the value 7.
\end{exercise}

Essentially, what the above definition of expected value does is to
compute the weighted sum/integral of |f x| for every point |x| in the
space.
%
And because |s| is a space (not necessarily a distribution), we must
normalise the result by dividing by its measure.

\begin{exercise}
Define various statistical moments (variance, skew, curtosis, etc.)
\end{exercise}


%**TODO: Perhaps come back to this with correct formulation: something like
% If (g1.f1) =.= (f2.g2) then |expectedValue (Project f1 s) g1 == f2 (expectedValue s g2)|
% with additional conditions.
% \paragraph{Theorem: Linearity of expected value}
% %
% Furthermore, if |f| is linear, then
% %
% |expectedValue (f <$> s) g| |==| |f (expectedValue s g)|
% % emacs $
%
% \begin{spec}
% expectedValue (f <$> s) g
%   == {- By definition of expected value -}
% integrator (f <$> s) g / measure (f <$> s)
%   == {- By linearity of |integrator| over |Project| -}
% integrator s (g . f) / measure s
%   == {- By definition of expected value -}
% expectedValue s (g . f)
% \end{spec}
% % emacs $
%
% %if False
% \begin{code}
% checkTypes f s g =
%   [
%     expectedValue (Project f s) g
%   , -- == {- By definition of expected value -}
%     integrator (Project f s) g / measure (f <$> s)
%   , -- == {- By linearity of |integrator| over |Project| -}
%     integrator s (g . f) / measure s
%   , --  == {- By definition of expected value -}
%     expectedValue s (g . f)
%   ]
% \end{code}
% %endif

% $ emacs

\section{Events and probability}

In textbooks, one typically finds the notation $P(e)$ for the
probability of an \emph{event} |e|.
%
Again, the space of situations |s| is implicit as well as the
dependency between |e| and |s|.

Here, we define events as \emph{boolean-valued} random variables.
%
Thus an event |e| can be defined as a boolean-valued function |e : a
-> Bool| over a space |s : Space a|.
%
Assuming that the space |s| accurately represents the relative mass of
all possible situations, there are two ways to define the probability
of |e|.

\index{probability notation}%

The first definition of the probability of |e| is the expected value
of |indicator . e|:
%
\begin{code}
probability1 :: Space a -> (a -> Bool) -> REAL
probability1 d e = expectedValue d (indicator . e)
\end{code}
%
The second definition of probability is the ratio between the measure
of the subspace where |e| holds, and the measure of the complete
space.
%
\begin{code}
probability2 :: Space a -> (a -> Bool) -> REAL
probability2 s e = measure (Sigma s (isTrue . e)) / measure s

isTrue :: Bool -> Space ()
isTrue = Factor . indicator
\end{code}
where |isTrue c| is the subspace which has measure 1 if |c| is true
and 0 otherwise.
%
The subspace of |s| where |e| holds is then the first projection of
|Sigma s (isTrue . e)|.
%
\begin{code}
subspace :: (a->Bool) -> Space a -> Space a
subspace e s = Project fst (Sigma s (isTrue . e))
\end{code}

We can show that if |s| has a non-zero measure, then the two
definitions are equivalent:
%
\begin{lemma}
  |measure s * probability s e = measure (Sigma s (isTrue . e))|
  \label{lem:probability-measure}
\end{lemma}
\begin{proof}
(where we shorten |indicator| to |ind| and |integrator s| to \(\int \{\Varid{s}\}\))
%{
%format indicator = ind
%format integrator (s) = "\int\{" s "\}"
\begin{code}
measure_sigma_equations s e = 
     probability1 s e
   === {- Def. of |probability1| -}
     expectedValue s (indicator . e)
   === {- Def. of |expectedValue| -}
     integrator s (indicator . e) / measure s
   === {- Def. of |(.)| -}
     integrator s (\x -> indicator (e x)) / measure s
   === {- mutiplication by 1 -}
     integrator s (\x -> indicator (e x) * const 1 (x,())) / measure s
   === {- Def. of |integrator| -}
     integrator s (\x -> integrator (Factor (indicator (e x))) (\y -> const 1 (x,y))) / measure s
   === {- Def. of |isTrue| -}
     integrator s (\x -> integrator (isTrue (e x)) (\y -> const 1 (x,y))) / measure s
   === {- Def. of |integrator| for |Sigma| -}
     measure (Sigma s (isTrue . e)) / measure s
\end{code}
%}

\end{proof}

We can now note that the space |observing e s| is the subspace of |s|
where the event |e| is observed to be true --- a kind of subspace
which is often used in textbook problems.

It will often be convenient to define a space whose underlying set is
a boolean value and compute the probability of the identity event:
\begin{code}
probability :: Space Bool -> REAL
probability d = expectedValue d indicator
\end{code}

Sometimes one even finds in the literature and folklore the notation
$P(v)$, where $v$ is a value, which stands for $P(t=v)$, for an
implicit random variable $t$.
%
Here even more imagination is required from the reader, who must not
only infer the space of outcomes, but also which random variable the
author means.

\section{Conditional probability}
\index{conditional probability}%
%
In \cref{sec:StocSys}, we encountered the notion of conditional
probability, traditionally written $P(F ∣ G)$ and read ``probability
of $F$ given $G$''.
%
As suggested in \cref{sec:StocSys} and brushed upon in
\cref{ex:prob-notation-naive}, it is not the case that the expression
$(F ∣ G)$ is an event.
%
Rather, a conditional probability is a separate concept, which takes
both |f| and |g| as arguments.
%
It is defined as the probability of |f| in the sub space where |g|
holds:
%
\begin{code}
condProb :: Space a -> (a -> Bool) -> (a -> Bool) -> REAL
condProb s f g = probability1 (subspace g s) f
\end{code}

We find the above definition more intuitive than the more usual
definition $P(F∣G) = P(F∩G) / P(G)$.
%
Why?
%
Because it makes clear that, in $P(F∣G)$, the event $G$ acts as the
subspace upon which the truth of $F$ is integrated.
%
(In fact, the $P(F∣G)$ notation is an improvement over the $P(F)$
notation, in the sense that the underlying space is more explicit.)

Regardless, the equivalence between the two definitions can be proven,
by symbolic calculation:
\begin{lemma}
|condProb s f g == probability s (\y -> f y && g y) / probability s g|
\label{lem:cond-prob}
\end{lemma}
%TODO: Possibly split into helper lemma(s) to avoid diving ``too deep''.
%{
%format indicator = ind
%format integrator (s) = "\int\{" s "\}"
\begin{proof}~
\begin{code}
cond_prob_equations :: Space a -> (a -> Bool) -> (a -> Bool) -> REAL
cond_prob_equations s f g = 
      condProb s f g
    === {- Def of condProb -}
      probability1 (subspace g s) f
    === {- Def of subspace -}
      probability1 (Sigma s (isTrue . g)) (f . fst)
    === {- Def of probability1 -}
      expectedValue (Sigma s (isTrue . g)) (indicator . f . fst)
    === {- Def of expectedValue -}
      (1/measure(Sigma s (isTrue . g))) * (integrator (Sigma s (isTrue . g)) (indicator . f . fst))
    === {- Def of |integrator| (Sigma) -}
      (1/measure s/probability1 s g) * (  integrator s               $ \x ->
                                          integrator (isTrue (g x))  $ \y ->
                                          indicator . f . fst $ (x,y))
    === {- Def of |fst| -}
      (1/measure s/probability1 s g) * (  integrator s               $ \x ->
                                          integrator (isTrue (g x))  $ \y ->
                                          indicator . f $ x)
    === {- Def of |isTrue| -}
      (1/measure s/probability1 s g) * (integrator s $ \x -> indicator (g x) * indicator (f x))
    === {- Property of |indicator| -}
      (1/measure s/probability1 s g) * (integrator s $ \x -> indicator (g x && f x) )
    === {- associativity of multiplication -}
      (1/probability1 s g) * (integrator s $ \x -> indicator (g x && f x)) / measure s
    === {- Definition of |probability1| -}
      (1/probability1 s g) * probability1 s (\x -> g x && f x)
\end{code}
% emacs wakeup $
%}
\end{proof}
\section{Examples: Dice, Drugs, Monty Hall}

We are now ready to solve all three problems motivating this chapter.

\subsection{Dice problem}

\begin{code}
diceSpace :: Space Bool
diceSpace =
     -- consider only the event ``product >= 10''
   Project (\(x,y) -> (x * y >= 10))  $
   -- observe that the sum is >= 7
   observing (\(x,y) -> (x + y >= 7)) $
   -- sample two balanced die
   twoDice
\end{code}
Then we can compute its probability:
\begin{code}
diceProblem :: REAL
diceProblem = probability diceSpace

-- |>>> diceProblem|
-- 0.9047619047619047
\end{code}

\begin{exercise}
Use the monadic interface to define the same experiment.
\end{exercise}

To illustrate the use of the various combinators from above to explore
a sample space we can compute a few partial results explaining the
|diceProblem|:
\begin{code}
p1 (x,y) = x+y >= 7
p2 (x,y) = x*y >= 10
test1     = measure (observing p1 twoDice)                       -- 21
test2     = measure (observing p2 twoDice)                       -- 19
testBoth  = measure (observing (\xy -> p1 xy && p2 xy) twoDice)  -- 19
prob21    = condProb (prod d6 d6) p2 p1                          -- 19/21
\end{code}
We can see that 21 possibities give a sum |>=7|, that 19 possibilities
give a product |>=10| and that all of those 19 satisfy both
requirements.

\subsection{Drug test}
The above drug test problem (\cref{ex:drugtest} at the start of
this chapter) is often used as an illustration for the Bayes
theorem.
%
We can solve it in exactly the same fashion as the Dice problem.

We begin by describing the space of situations.
%
To do so we make heavy use of the |bernoulli| distribution.
%
First we model the distribution of drug users.
%
Then we model the distribution of test outcomes --- depending on
whether we have a user or not.
%
Finally, we project out all variables, caring only about |isUser|.

\begin{code}
drugSpace :: Space Bool
drugSpace =
   -- we're interested the posterior distribution of |isUser|,
  -- (first component of the pair, ignoring the result of the test).
  Project fst  $
  -- we have ``a positive test'' by assumption
  -- (second component of the pair contains result of the test)
  observing snd $ 
  Sigma
    (bernoulli 0.005) -- model the distribution of drug users
    (\isUser -> bernoulli (if isUser then 0.99 else 0.01))
    -- model test results depending on whether we have a drug user

\end{code}
The probability is computed as usual:
\begin{code}
userProb :: REAL
userProb = probability drugSpace

-- |>>> userProb|
-- 0.33221476510067116
\end{code}
%
Thus a randomly selected individual with a positive test is a drug
user with probability around one third (and about two thirds are false
positives).

Perhaps surprisingly, we never needed the Bayes theorem to solve the
problem.
%
Indeed, the Bayes theorem is already incorporated in our defintion of
|probability|, so our methodology guarantees that we always respect
it.

\subsection{Monty Hall}

We can model the Monty Hall problem as follows.
%
For expository purposes, let us define the list of doors:
\begin{code}
type Door = Int
doors :: [Door]
doors = [1,2,3]
\end{code}

The event of ``winning'' depends on four variables:
\begin{itemize}
\item which door is the winning door (|winningDoor :: Door|)
\item the initial door choice (|initiallyPickedDoor :: Door|) 
\item the door which Monty opens (|montyPickedDoor :: Door|)
\item and finally, whether the player changes their mind after Monty
  shows that the door has a goat (|changing :: Bool|).
\end{itemize}

One way to do it is as follows:
\begin{code}
haveWon :: Bool -> ((Door, Door), Door) -> Bool
haveWon changing ((winningDoor,initiallyPickedDoor),montyPickedDoor)
   = finalChoice == winningDoor
  where finalChoice = case changing of
          False   ->  initiallyPickedDoor
          True    ->  head (doors \\ [initiallyPickedDoor, montyPickedDoor])
\end{code}
The player wins if their final choice is the right one.
%
If they do not change their mind, then the final choice is equal to
the initial one.
%
If they do change their mind, then the final choice is neither their
inital choice nor Monty's door.
%
(Because there are only three doors there is only one door left.)

Then, we need to describe the set of situations, as a triple:
\[|((winningDoor,initiallyPickedDoor),montyPickedDoor)|.\]
%
The first two variables are uniform, but the |montyPickedDoor| is
uniform in the set |(doors \\ [pickedDoor, winningDoor])|.
%
Note that if the |initiallyPickedDoor| is the same as the
|winningDoor|, then Monty has two choices.
%
We then imagine that Monty picks a door at random among those, even
though this might not be the case in a real game.%
\footnote{Exercise: check other strategies for Monty.}
\begin{code}
montySpace :: Space ((Door, Door), Door)
montySpace =
  (Sigma  (prod   (uniformDiscrete doors)
                  (uniformDiscrete doors))
          (\(winningDoor,pickedDoor) ->
              uniformDiscrete (doors \\ [pickedDoor, winningDoor])))
\end{code}

\begin{code}
montyProblem :: Bool -> Space Bool
montyProblem changing = Project (haveWon changing) montySpace

-- |>>> probability (montyProblem False)|
-- 0.3333333333333333

-- |>>> probability (montyProblem True)|
-- 0.6666666666666666
\end{code}
Thus, the ``changing door'' strategy is twice as good.

The Monty Hall is sometimes considered paradoxical: it is strange that
changing one's mind can change the outcome.
%
The crucial point to see this is that Monty can never show a door which contains
the prize.
%
To illustrate, an \emph{incorrect} way to model the Monty Hall
space of situations is the following:
\begin{code}
montySpaceIncorrect :: Space ((Door, Door), Door)
montySpaceIncorrect =
  (Sigma (prod (uniformDiscrete doors)
               (uniformDiscrete doors))
         (\(_,pickedDoor) -> uniformDiscrete (doors \\ [pickedDoor])))

-- |>>> probability (Project (haveWon False) montySpaceIncorrect)|
-- 0.5

\end{code}
The above does not correctly model the problem, because it allows
Monty to pick the door already chosen by the player.

\subsection{Solving a problem with equational reasoning}
Consider the following problem: how many times must one throw a coin
before one obtains 3 heads in a row?
%
We can model the problem as follows:
\begin{code}
coin :: Distr Bool
coin = bernoulli 0.5

coins :: Distr [Bool]
coins =  Project  (\(x,xs) -> x : xs)
                  (prod coin coins)

threeHeads :: [Bool] -> Int
threeHeads (True:True:True:_) = 3
threeHeads (_:xs) = 1 + threeHeads xs

example' :: Space Int
example' = Project threeHeads coins
\end{code}
% emacs $ $

Even though the problem is easy to \emph{model} using the DSL, it is
not easy to compute a solution.
%
Indeed, attempting to evaluate |probability1 threeHeads (< 5)| does
not terminate.
%
This is because we have an infinite list, which translates to
infinitely many cases to consider.
%
So the evaluator cannot solve this problem in finite time.
%
Hence, we have to resort to a symbolic method to solve it.
%
This will require extensive symbolic reasoning (perhaps not for the
faint of heart).
%
One may skip all the equational reasoning in first reading: the
important point is to realize that one is able to write down the kind
of proofs that one would do with pen and paper directly within a
program.
%
(We even use the Haskell type checker to verify that each line
typechecks.
%
Even though this does not guarantee that the proof is sound, we catch
most typos this way.)

The first step in our computation of the solution is a creative one,
which involves generalising the problem to computing the number of
throws to obtain \(3-m\) heads in a row.
%
For this purpose we define the function |tH|, such that |tH 3 ==
threeHeads|:

\begin{code}
tH :: Int -> [Bool] -> Int
tH 0 _ = 0
tH m (x:xs) = 1 + if x then tH (m-1) xs else tH 3 xs
\end{code}
We then define |helper m = Project (tH m) coins|.
%
Computing it symbolically will give our answer.
%
But first, we need a lemma which helps us push |Project| inside
|Sigma|:
\begin{lemma}
  |Project f (Sigma a g) == Project snd (Sigma a (\x -> Project (f . (x,)) (g x)))|
  \label{lem:project/sigma}
\end{lemma}
\begin{proof}
%{
%format indicator = ind
%format integrator (s) = "\int\{" s "\}"
We check the equivalence by using semantic equality:
  \begin{code}
project_sigma_equations f a g h = 
      integrator (Project f (Sigma a g)) h
  === -- by def
      integrator (Sigma a g) (h . f)
  ===  -- by def
      integrator a (\x -> integrator (g x) (\y -> (h . f) (x,y)))
  ===  -- rewriting in point-free style
      integrator a (\x -> integrator (g x) (h . f . (x,)))
  ===  -- by def of integrator of |Project|
      integrator a (\x -> integrator (Project (f . (x,)) (g x)) h)
  ===  -- |h == \y -> h y|
      integrator a (\x -> integrator (Project (f . (x,)) (g x)) $ \y -> h y)
  ===  -- taking an explicit (x,y) pair
      integrator a (\x -> integrator (Project (f . (x,)) (g x)) $ \y -> (h . snd) (x,y))
  ===  -- by def of integrator of |Sigma|
      integrator (Sigma a (\x -> Project (f . (x,)) (g x))) (h . snd)
  ===   -- by def of integrator of |Project|
      integrator (Project snd (Sigma a (\x -> Project (f . (x,)) (g x)))) h
\end{code}
\end{proof}


\begin{lemma}
  |Project (if c then a else b) s == if c then Project a s else Project b s|
\end{lemma}
\begin{proof}
We check the equivalence by using semantic equality:
  \begin{code}
project_if_equations c a b s g = 
      integrator (Project (if c then a else b) s) g
  === -- by def
      integrator s ((if c then a else b) . g)
  ===  -- by def
      integrator (if c then Project a s else Project b s) g
\end{code}
\end{proof}


We can proceed from |helper m| by equational reasoning, and see what we get:
\begin{code}
unfolding_helper_equations m = 
      helper m 
  === -- by def
      Project (tH m)  coins
  === -- by def
      Project (tH m) (Project (\(x,xs) -> x : xs) (prod coin coins))
  === -- property of |Project| (\cref{ex:monad-laws}, functoriality of |Project|)
      Project (tH m . \(x,xs) -> x : xs) (prod coin coins)
  === -- by def
      Project  (\(x,xs) -> 1 + if x then tH (m-1) xs else tH 3 xs)
               (prod coin coins)
  === -- by functoriality of |Project|
      Project (1+) (Project  (\(x,xs) -> if x   then  tH (m-1) xs
                                                else  tH 3 xs)
                             (prod coin coins))
  === -- by \cref{lem:project/sigma}
      Project (1+) (Project snd
                      (Sigma  coin (\x -> Project  (\xs -> if x  then  tH (m-1) xs
                                                                 else  tH 3 xs)
                                                   coins)))
  === -- by functoriality of |Project|
      Project  ((1+) . snd)
               (Sigma  coin (\x -> Project  (\xs -> if x  then  tH (m-1) xs
                                                          else  tH 3 xs)
                                            coins))
  === -- by semantics of |if| in Haskell
      Project  ((1+) . snd)
               (Sigma  coin (\x -> Project  (if x  then  (tH (m-1))
                                                   else  (tH 3))
                                            coins))
  === -- by semantics of |if| in Haskell
      Project   ((1+) . snd)
                (Sigma coin (\x -> if x  then  Project (tH (m-1))  coins
                                         else  Project (tH 3)      coins))
  === -- by definition of |helper|.
      Project ((1+) . snd) (Sigma coin (\x -> if x  then  helper (m-1)
                                                    else  helper 3))
\end{code}

To sum up:
\begin{code}
helper 0 = point 0
helper m =
  Project ((1 +) . snd)  
  (Sigma coin (\h -> if h
                then  helper (m-1)
                else  helper 3)) -- when we have a tail, we start from scratch
\end{code}
% emacs $ $
Evaluating the probability still does not terminate:
%
we no longer have an infinite list, but we still have infinitely many
possibilities to consider:
%
however small, there is always a probability to get a ``tail'' at the
wrong moment, and the evaluation must continue.

But we can keep performing our symbolic calculation.
%
We can start by showing that |helper m| is a distribution (its measure
is 1).
%
The proof is by induction on |m|:
\begin{itemize}
\item for the base case |measure (helper 0) = measure (pure 0) = 1|
\item for the step case: assume that |helper m| is a distribution.
  %
  Then, for every |h|, |if h then helper m else helper 3| is a
  distribution too.
  %
  The result is obtained by using the distribution property of |Sigma|
  (\cref{lem:measure-properties}).
\end{itemize}

Then, we can symbolically compute the integrator of |helper|.
%
The base case is |integrator (helper 0) id == 0|, and left as an exercise to the reader.
%
In the recursive case, we have:

\begin{spec}
integrator (helper (m+1)) id
== {- By above result regarding |helper| -}
integrator  (Project ((1 +) . snd)
            (Sigma coin (\h -> if h then helper (m-1) else  helper 3))) id
== {- By integrator def -}
integrator (Sigma coin (\h -> if h then  helper (m-1) else  helper 3)) ((1 +) . snd)
== {- By integrator def -}
integrator coin $ \h -> integrator (if h then helper m else helper 3) (1 +)
== {- By linearity of |integrator| -}
1 + integrator coin $ \h -> integrator (if h then helper m else helper 3) id
== {- By case analysis -}
1 + (integrator coin $ \h -> if h then integrator (helper m) id else integrator (helper 3) id)
== {- By integrator/bernouilli (\cref{ex:integrator-bernouilli}) -}
1 + 0.5 * integrator (helper m) id + 0.5 * integrator (helper 3) id
\end{spec}
% $ emacs

If we let |h m = expectedValueOfDistr (helper m)|, and using the above
lemma, then we find:

\begin{spec}
h (m+1)  == 1 + 0.5 * h m + 0.5 * h 3
\end{spec}
which we can rewrite as:
\begin{spec}
2 * h (m+1) = 2 + h m + h 3
\end{spec}
and expand for |m = 0| to |m = 2|:
\begin{spec}
2 * h 1 = 2 +        h 3
2 * h 2 = 2 + h 1 +  h 3
2 * h 3 = 2 + h 2 +  h 3
\end{spec}
This leaves us with a system of linear equations with three unknowns
|h 1|, |h 2| and |h 3|, which admits a single solution with |h 1 = 8|,
|h 2 = 12|, and finally |h 3 = 14|, giving the answer to the initial
problem: we can expect to need 14 coin flips to get three heads in a
row.
%}

%Simplify + eliminate |h 3 = 2*h 1 - 2 |:
% 2 * h 2 = 2 + h 1 +  2*h 1 - 2 = 3 * h 1
% 2 * h 1 - 2 = 2 + h 2
%Simplify
% 3 * h 1 = 2 * h 2
% 2 * h 1 = 4 + h 2
%Eliminate |h 2 = 2*h 1 - 4|
% 3 * h 1 = 2 * (2*h 1 - 4) = 4*h 1 - 8
%Simplify and substitute back
% h 1 =  8
% h 2 = 12
% h 3 = 14

\section{Independent events}
\index{independent events}%

Another important notion in probability theory is that of independent
events.
%
One way to define independent events is as follows.
%
$E$ is independent from $F$ iff $P(E ∣ F) = P(E)$.
%

According to \citet{IntroProb_Grinstead_Snell_2003}, two events
are independent iff.\ $P(E ∩ F) = P(E) · P(F)$.

The proof can be written in the traditional notation as follows:
\begin{proof}
In the left to right direction:
\begin{spec}
  P(E ∩ F)
= {- by def. of cond. prob -}
  P(E ∣ F) · P (F)
= {- by def. of independent events -}
  P(E) · P (F)
\end{spec}

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

\end{proof}

Let us now express the same definitions and the same theorem and proof
in our DSL.
%
The defintion for independent events is:
\begin{code}
independentEvents :: Space a -> (a -> Bool) -> (a -> Bool) -> Bool
independentEvents s e f = probability1 s e == condProb s e f
\end{code}

The equivalent formulation is:
\begin{spec}
independentEvents2 s e f =
  probability1 s (\x -> e x && f x) == probability1 s e * probability1 s f
\end{spec}

We can now state (and prove) the lemma using the DSL notation:
\begin{lemma}
  independentEvents s e f ⇔ independentEvents2 s e f 
\end{lemma}
\begin{proof}
  Left to right direction:
  \begin{spec}
        probability1 s (\x -> e x && f x)
      ===  {- by \cref{lem:cond-prob} -}
        condProb s e f    * probability1 s f
      ===  {- by assumption -}
        probability1 s e  * probability1 s f
  \end{spec}
\end{proof}

We note that at this level of abstraction, the proofs follow the same
structure as the textbook proofs --- the underlying space |s| is
constant.

\begin{exercise}
Express the rest of the proof using our DSL.
\end{exercise}


% \section{Continuous spaces and equality}
% TODO
% \begin{spec}
% Dirac :: REAL -> Space


% integrator (Dirac x) f == f x

% -- Not good enough because we can't relate variables. IE; how do we do:


% uniform = do
%   x <- RealLine
%   isTrue (x >= 0 && x < 1)
%   return x

% exampleD0 = do
%   x <- uniform
%   y <- uniform
%   equal x (2*y)  -- Dirac ???
%   -- Dirac (x-y) does not work
%   return (x <= 0.5)


% integrator exampleD0 f =
%   integrator uniform $ \x ->
%   integrator uniform $ \y ->
%   integrator ??? $  \x ->
%   integrator (return (x <= 0.5))

% \end{spec}



% Local Variables:
% dante-methods : (bare-ghci)
% mode: literate-haskell
% ispell-dictionary: "british"
% End:

% LocalWords:  TupleSections DSLsofMath ap equational infixl endif
% LocalWords:  DSL arabic href drugtest emph monty twoDice sumAbove
% LocalWords:  const fst RealLine monadic fmap Applicative snd Distr
% LocalWords:  isDistribution uniformDiscrete xs fromIntegral sqrt tH
% LocalWords:  marginaliseWith observing dieDistr bernoulli normalMass iff
% LocalWords:  distributionDensity bigsum defintions expectedValue
% LocalWords:  expectedValueOfDistr calculational IntroProb Grinstead
% LocalWords:  representable curtosis isTrue mutiplication condProb
% LocalWords:  cond diceSpace threeHeads cardinality
