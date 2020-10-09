

\subsection{Recap of syntax trees with variables, |Env| and |lookup|}

% removed this because it's already covered in similar ways before and after, several times.
% Alternatively: convert to exercise.

\section{Logic and calculational proofs}
\label{sec:logic}


In this chapter, we continue to exercise our skill of organize areas
of mathematics in DSL terms. We apply our methodology to the languages
of logic: propositions and proofs. Additionally, at the same time, we
will develop adequate notions and notations for mathematical
foundations and learn to perform calculational proofs.
%
There will be a fair bit of theory: introducing propositional and
first order logic, but also applications to mathematics: prime
numbers, (ir)rationals, limit points, limits, etc. and some
Haskell concepts.

%if False
\begin{code}
module DSLsofMath.W02 where
\end{code}
%endif

%include SetTheory.lhs
%include PropositionalLogic.lhs
%include FOL.lhs
%include Examples.lhs
%include CalculusConcepts.lhs



\subsection{SET and PRED}
\jp{reformulate in book form (or move earlier, or remove)}
%
\label{par:SETandPRED}

Several groups have had trouble grasping the difference between |SET|
and |PRED|.
%
This is understandable, because we have so far in the lectures mostly
talked about term syntax + semantics, and not so much about predicate
syntax and semantics.
%
The one example of terms + predicates covered in the lectures is
Predicate Logic and I never actually showed how eval (for the
expressions) and check (for the predicates) is implemented.

As an example we can we take our terms to be the rational number
expressions defined above and define a type of predicates over those
terms:
\begin{code}
type Term v = Rat v

data RPred v  =  Equal     (Term v) (Term v)
              |  LessThan  (Term v) (Term v)
              |  Positive  (Term v)

              |  AND  (RPred v) (RPred v)
              |  NOT  (RPred v)
  deriving (Eq, Show)
\end{code}
%
Note that the first three constructors, |Eq|, |LessThan|, and
|Positive|, describe predicates or relations between terms (which can contain term
variables)
%
while the two last constructors, |AND| and |NOT|, just combine such
relations together.
%
(Terminology: I often mix the words ``predicate'' and ``relation''.)

We have already defined the evaluator for the |Term v| type but we
need to add a corresponding ``evaluator'' (called |check|) for the
|RPred v| type.
%
Given values for all term variables the predicate checker should just
determine if the predicate is true or false.
\begin{code}
checkRP :: (Eq v, Show v) => Env v RatSem -> RPred v -> Bool
checkRP env (Equal     t1 t2)  = eqSem        (evalRat2 env t1) (evalRat2 env t2)
checkRP env (LessThan  t1 t2)  = lessThanSem  (evalRat2 env t1) (evalRat2 env t2)
checkRP env (Positive  t1)     = positiveSem  (evalRat2 env t1)

checkRP env (AND p q)  = (checkRP env p) && (checkRP env q)
checkRP env (NOT p)    = not (checkRP env p)
\end{code}
Given this recursive definition of |checkRP|, the semantic functions
|eqSem|, |lessThanSem|, and |positiveSem| can be defined by just
working with the rational number representation:
\begin{code}
eqSem        :: RatSem -> RatSem -> Bool
lessThanSem  :: RatSem -> RatSem -> Bool
positiveSem  :: RatSem -> Bool
eqSem        = error "TODO"
lessThanSem  = error "TODO"
positiveSem  = error "TODO"
\end{code}


%include E2.lhs
