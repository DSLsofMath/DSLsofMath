\section{Week 2}

Course learning outcomes:

\begin{itemize}
\item Knowledge and understanding
  \begin{itemize}
  \item design and implement a DSL (Domain Specific Language) for a new domain
  \item organize areas of mathematics in DSL terms
  \item explain main concepts of elementary real and complex analysis,
        algebra, and linear algebra
  \end{itemize}
\item Skills and abilities
  \begin{itemize}
  \item develop adequate notation for mathematical concepts
  \item perform calculational proofs
  \item use power series for solving differential equations
  \item use Laplace transforms for solving differential equations
  \end{itemize}
\item Judgement and approach
  \begin{itemize}
  \item discuss and compare different software implementations of
        mathematical concepts
  \end{itemize}
\end{itemize}

This week we focus on ``develop adequate notation for mathematical
concepts'' and ``perform calculational proofs'' (still in the context
of ``organize areas of mathematics in DSL terms'').


\subsection{A few words about pure set theory}

One way to build mathematics from the ground up is to start from pure
set theory and define all concepts by translation to sets.
%
We will only work with this as a mathematical domain to study, not as
``the right way'' of doing mathematics.
%
The core of the language of pure set theory has the Empty set, the
one-element set constructor Singleton, set Union, and Intersection.
%
There are no ``atoms'' or ``elements'' to start from except for the
empty set but it turns out that quite a large part of mathematics can
still be expressed.

\paragraph{Natural numbers} To talk about things like natural numbers
in pure set theory they need to be encoded. Here is one such encoding
(which is explored further in the first hand-in assignment).

\begin{spec}
vonNeumann 0        =  Empty
vonNeumann (n + 1)  =  Union  (vonNeumann n)
                              (Singleton (vonNeumann n))
\end{spec}

\paragraph{Pairs}

Definition:  A pair |(a,b)| is encoded as |{{a},{a,b}}|.

% (a,b) \coloneqq \big\{\,\{a\},\{a,b\}\,\big\} ;

\subsection{Propositional Calculus}

Swedish: Satslogik

False, True, And, Or, Implies

\subsection{First Order Logic (predicate logic)}

Swedish: Första ordningens logik = predikatlogik

Adds term variables and functions, predicate symbols and quantifiers (sv: kvantorer).


\subsection{Questions and answers from the exercise sessions week 2}

\paragraph{Variables, |Env| and |lookup|}

This was a frequently source of confusion already the first week so
there is already a question + answers earlier in this text.
%
But here is an additional example to help clarify the matter.
\begin{code}
data Rat v = RV v | FromI Integer | RPlus (Rat v) (Rat v) | RDiv (Rat v) (Rat v)

newtype RatSem = RSem (Integer, Integer)
\end{code}
We have a type |Rat v| for the syntax trees of rational number
expressions and a type |RatSem| for the semantics of those rational
number expressions as pairs of integers.
%
The constructor |RV :: v -> Rat v| is used to embed variables with
names of type |v| in |Rat v|.
%
We could use |String| instead of |v| but with a type parameter |v| we
get more flexibility at the same time as we get better feedback from
the type checker.
%
To evaluate some |e :: Rat v| we need to know how to evaluate the
variables we encounter.
%
What does ``evaluate'' mean for a variable?
%
Well, it just means that we must be able to translate a variable name
(of type |v|) to a semantic value (a rational number in this case).
%
Translate sounds like a good case for a function so we can give the
following implementation of eval:
%
\begin{code}
evalRat1 ::  (v -> RatSem) -> (Rat v -> RatSem)
evalRat1 ev (RV v)      = ev v
evalRat1 ev (FromI i)   = fromISem i
evalRat1 ev (RPlus l r) = plusSem (evalRat1 ev l) (evalRat1 ev r)
evalRat1 ev (RDiv  l r) = divSem  (evalRat1 ev l) (evalRat1 ev r)
\end{code}
Notice that we simply added a parameter |ev| for ``evaluate variable''
to the evaluator.
%
The rest of the definition follows a common pattern: recursively
translate each subexpression and apply the corresponding semantic
operation to combine the results: |RPlus| is replaced by |plusSem|,
etc.
%
\begin{code}
fromISem :: Integer -> RatSem
fromISem i = RSem (i, 1)

plusSem :: RatSem -> RatSem -> RatSem
plusSem = undefined -- TODO: exercise

-- Division of rational numbers
divSem :: RatSem -> RatSem -> RatSem
divSem (RSem (a, b)) (RSem (c, d)) = RSem (a*d, b*c)
\end{code}

Often the first argument |ev| to the eval function is constructed from
a list of pairs:
\begin{code}
type Env v s = [(v, s)]
envToFun :: (Show v, Eq v) => Env v s -> (v -> s)
envToFun [] v = error ("envToFun: variable "++ show v ++" not found")
envToFun ((w,s):env) v
  | w == v     = s
  | otherwise  = envToFun env v
\end{code}
Thus, |Env v s| can be seen as an implementation of a ``lookup
table''.
%
It could also be implemented using hash tables or binary search trees,
but efficiency is not the point here.
%
Finally, with |envToFun| in our hands we can implement a second
version of the evaluator:
\begin{code}
evalRat2 :: (Show v, Eq v) => (Env v RatSem) -> (Rat v -> RatSem)
evalRat2 env e = evalRat1 (envToFun env) e
\end{code}

\paragraph{The law of the excluded middle}

Many had problems with implementing the ``law of the excluded middle''
in the exercises and it is indeed a tricky property to prove.
%
They key to implementing it lies in double negation and as that is
encoded with higher order functions it gets a bit hairy.

\paragraph{SET and PRED}

Several groups have had trouble grasping the difference between |SET|
and |PRED|.
%
This is understandable, beacuse we have so far in the lectures mostly
talked about term syntax + semantics, and not so much about predicate
syntax and semantics.
%
The one example of terms + predicates covered in the lectures is
Predicate Logic and I never actually showed how eval (for the
expressions) and check (for the predicates) is implemented.

TODO: fill in an example
