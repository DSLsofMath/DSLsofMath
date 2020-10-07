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


\subsection{Proof by contradiction}

%if false
What would it take to do this proof in Haskell? A sketch.
\begin{code}
data ZZ where
  GCD,(:*:) :: ZZ -> ZZ -> ZZ
  One :: ZZ
  Two :: ZZ
type Pos = ZZ
data (==) (a::ZZ) (b::ZZ) where
data GCD  (a::ZZ) (b::ZZ)  where
data R x where
  Decomposition :: (forall (a::ZZ). forall (b::Pos). Not ((b ':*: x) == a `And` ('GCD a b == 'One))) -> R x
data Sqrt2Rational where
  ConstructionS2 :: forall r. ((r ':*: r) == 'Two) `And` R r -> Sqrt2Rational

-- squareEq :: a == b -> a :*: a == b :*: b
-- squareEq = error "assumed"

proof :: Sqrt2Rational -> False
proof (ConstructionS2 (p,q)) = error "tedious"
\end{code}
%endif

Let us express and prove the irrationality of the square
root of 2.
%
We have two main concepts involved: the predicate ``irrational'' and the
function ``square root of''.
%
The square root function (for positive real numbers) can be specified
by $r = \sqrt{s}$ iff |r^2 == s| and |r : REAL|.
%
The formula ``x is irrational'' is just |not(R x)| where |R| is the
predicate ``is rational''.
%
\begin{spec}
  R x = Exists (a:ZZ) (Exists (b:Pos) (b*x==a & GCD(a,b)==1))
\end{spec}
\jp{Why the GCD bit? This is strong version of rational which is easier to negate, but it should be justified at least a bit.}
The classical way to prove a negation |not P| is to assume |P| and
derive something absurd (some |Q| and |not Q|, for example).
%
Lets take |P = R r| and |Q = GCD(a,b)==1|.\jp{Do we have r=2 at this point? From which point?}
%
Assuming |P| we immediately get |Q| so what we need is to prove |not
Q|, that is |GCD(a,b)/=1|.
%
We can use the equations |b*r==a| and |r^2 == 2|.
%
Squaring the first equation and using the second we get |b^2*2==a^2|.
%
Thus |a^2| is even, which means that |a| is even, thus |a==2*c| for
some |c|.
%
But then |b^2*2==a^2==4*c^2| which means that |b^2==2*c^2|.
%
By the same reasoning again we have that also |b| is even.
%
But then |GCD(a,b)>=2| which implies |not Q|.

To sum up: by assuming |P| we can prove both |Q| and |not Q|.
%
Thus, by contradiction |not P| must hold.
\jp{With the build of proof terms, I was fully expecting to see the above formalised as such. We probably need quite a bit of stuff about GCD.}

\subsection{Proof by cases}
%
As another example, let's prove that there are two irrational numbers
|p| and |q| such that |p^q| is rational.

\begin{spec}
  S = Exists p (Exists q (not (R p) & not (R q) & R (p^q)))
\end{spec}

We know from above that |r = sqrt 2| is irrational, so as a first
attempt we could set |p=q=r|.
%
Then we have satisfied two of the three clauses (|not (R p)| and |not
(R q)|).
%
What about the third clause: is |x=p^q==r^r| rational?
%
We can reason about two possible cases, one of which has to hold: |R
x| or |not (R x)|.

Case 1: |R x| holds.
%
Then we have a proof of |S| with |p=q=r=sqrt 2|.

Case 2: |not (R x)| holds.
%
Then we have another irrational number |x| to play with.
%
Let's try |p=x| and |q=r|.
%
Then |p^q == x^r == (r^r)^r == r^(r*r) == r^2 == 2| which is clearly
rational.
%
Thus, also in this case we have a proof of |S|, but now with |p=r^r|
and |q=r|.

To sum up: yes, there are irrational numbers such that their power is
rational.
%
We can prove the existence without knowing what numbers |p| and |q|
actually are!
%

\subsection{Case study: there is always another prime}

As an example of combining quantification (forall, exists) and implication let us turn
to one statement of the fact that there are infinitely many primes.
%
If we assume that we have a unary predicate expressing that a number is prime
and a binary (infix) predicate ordering the natural numbers we can
define a formula |IP| for ``Infinitely many Primes'' as follows:
%
\begin{spec}
 IP = Forall n (Prime n => Exists m (Prime m & m > n))
\end{spec}
%TODO: perhaps introduce Prime and < in L01
%
Combined with the fact that there is at least one prime (like |2|) we
can repeatedly refer to this statement to produce a never-ending
stream of primes.

To prove this formula we first translate from logic to programs as
described above.
%
We can translate step by step, starting from the top level.
%
The forall-quantifier translates to a (dependent) function type |(n :
Term) -> | and the implication to a normal function type |Prime n ->|.
%
The exists-quantifier translates to a (dependent) pair type |((m :
Term), ...)| and finally the |&| translates into a pair type.
%
Putting all this together we get a type signature for any |proof| of
the theorem:
%
\begin{spec}
proof : (n : Term) -> Prime n -> ((m : Term), (Prime m, m>n))
\end{spec}
%
Now we can start filling in the definition of |proof| as a
2-argument function returning a triple: % nested pair
%
\begin{spec}
proof n np = (m, (pm, gt))
  where  m' = 1 + factorial n
         m  = {- some non-trivial prime factor of |m'| -}
         pm = {- a proof that |m| is prime -}
         gt = {- a proof that |m>n| -}
\end{spec}
%
The proof |pm| is the core of the theorem.
%
First, we note that for any |2<=p<=n| we have
%
\begin{spec}
 mod m' p                        ==  {- Def. of |m'| -}
 mod (1 + n!) p                  ==  {- modulo distributes over |+| -}
 mod (mod 1 p  +  mod (n!) p) p  ==  {- modulo comp.: |n!| has |p| as a factor -}
 mod (1        +  0) p           ==
 1
\end{spec}
where |mod x y| is the remainder after integer division of |x| by |y|.
%
Thus |m'| is not divisible by any number from |2| to |n|.
%
But is it a prime?
%
If |m'| is prime then |m=m'| and the proof is done (because |1+n! >= 1
+ n > n|).
%
Otherwise, let |m| be a prime factor of |m'| (thus |m'=m*q|, |q>1|).
%
Then |1 == mod m' p == (mod m p)*(mod q p)| which means that neither
|m| nor |q| are divisible by |p| (otherwise the product would be
zero).
%
Thus they must both be |>n|.
%
QED.

Note that the proof can be used to define a somewhat useful function
which takes any prime number to some larger prime number.
%
We can compute a few example values:

\begin{tabular}{r@@{ $\mapsto$ }rl}
     2 &  3 &( 1+2! )
  \\ 3 &  7 &( 1+3! )
  \\ 5 & 11 &( 1+5! = 121 = 11*11 )
  \\ 7 & 71 &\ldots
\end{tabular}


\subsection{Basic concepts of calculus}
\jp{This is marked as a subjection but after reading the text it feels more like a new chapter is starting.}

Now we have built up quite a bit of machinery to express logic
formulas and proofs.\jp{But are we using it here? Except writing down formulas?}
%
It is time time to apply it to some concepts in calculus.
%
We start with the concept of ``limit point'' which is used in the
formulation of different properties of limits of functions.

% TODO: Perhaps start with the ``expression'' $lim_{x\to x_0} f(x)$ and
% explain that not all |x_0| make sense, etc. [For context and
% motivation.]
%
% TODO: Or talk a bit about open and closed sets. (Open set = every
% point is internal = there is some wiggle-room around each point in the
% set. Closed set contains all its limit points.)

\paragraph{Limit point}\label{sec:LimPoint}

\emph{Definition} (adapted from \cite{rudin1964principles}, page 28):
Let |X| be a subset of |ℝ|.
%
A point |p ∈ ℝ| is a limit point of |X| iff for every |ε > 0|, there
exists |q ∈ X| such that |q ≠ p| and |absBar(q - p) < ε|.
%

To express ``Let |X| be a subset of |ℝ|'' we write |X : 𝒫 ℝ|.
%
In general, the operator |𝒫| takes a set (here |REAL|) to the set of
all its subsets.
%
\begin{spec}
Limp : ℝ → 𝒫 ℝ → Prop
Limp p X = ∀ ε > 0? ∃ q ∈ X - {p}? absBar (q-p) < ε
\end{spec}
%
Notice that |q| depends on |ε|.
%
Thus by introducing a function |getq| we can move the |∃| out.

\begin{spec}
type Q = {-"ℝ_{> 0}"-} → (X - {p})
Limp p X = ∃ getq : Q? ∀ ε > 0? absBar (getq ε - p) < ε
\end{spec}

Next: introduce the ``open ball'' function |B|.
%
\begin{spec}
B : ℝ → {-"ℝ_{> 0}"-} → 𝒫 ℝ
B c r = {x | absBar (x - c) < r}
\end{spec}
%
|B c r| is often called an ``open ball'' around |c| of radius |r|.
%
On the real line this ``open ball'' is just an open interval, but with
complex |c| or in more dimensions the term feels more natural.
%
In every case |B c r| is an open set of values (points) of distance
less than |r| from |c|.
%
The open balls around |c| are special cases of \emph{neighbourhoods of
  |c|} which can have other shapes but must contain some open ball.

Using |B| we get
\begin{spec}
Limp p X = ∃ getq : Q? ∀ ε > 0? getq ε ∈ B p ε
\end{spec}

Example 1: Is |p=1| a limit point of |X={1}|?
%
No! |X - {p} = {}| (there is no |q/=p| in |X|), thus there cannot
exist a function |getq| because it would have to return elements in
the empty set!

Example 2: Is |p=1| a limit point of the open interval |X = (0,1)|?
%
First note that |p ∉ X|, but it is ``very close'' to |X|.
%
A proof needs a function |getq| which from any |ε| computes a point |q
= getq ε| which is in both |X| and |B 1 ε|.
%
We need a point |q| which is in |X| and \emph{closer} than |ε| from |1|.
%
We can try with |q = 1-ε/2| because |absBar (1-(1-ε/2)) = absBar (ε/2) = ε/2
< ε| which means |q ∈ B 1 ε|.
%
We also see that |q/=1| because |ε > 0|.
%
The only remaining thing to check is that |q ∈ X|.
%
This is true for sufficiently small |ε| but the function |getq| must
work for all positive reals.
%
We can use any value in |X| (for example |17/38|) for |ε| which are
``too big'' (|ε >= 2|).
%
Thus our function can be
%
\begin{spec}
  getq ε  | ε < 2      = 1 - ε/2
          | otherwise  = 17/38
\end{spec}
%
A slight variation which is often useful would be to use |max| to
define |getq ε = max (17/38,1-ε/2)|.
%
Similarly, we can show that any internal point (like |1/2|) is a limit
point.

Example 3: limit of an infinite discrete set |X|

\begin{spec}
X = {1/n | n ∈ Pos }
\end{spec}

Show that |0| is a limit point of |X|.
%
Note (as above) that |0 ∉ X|.

We want to prove |Limp 0 X| which is the same as |∃ getq : Q? ∀ ε > 0?
getq ε ∈ B 0 ε|.
%
Thus, we need a function |getq| which takes any |ε > 0| to an element
of |X - {0} = X| which is less than |ε| away from |0|.
%
Or, equivalently, we need a function |getn : {-"ℝ_{> 0}"-} → Pos| such
that |1/n < ε|.
%
Thus, we need to find an |n| such that |1/ε < n|.
%
If |1/ε| would be an integer we could use the next integer (|1 + 1/ε|),
so the only step remaining is to round up:
%
\begin{spec}
getq ε = 1/getn ε
getn ε = 1 + ceiling (1/ε)
\end{spec}
%
\begin{exercise}
prove that |0| is the \emph{only} limit point of |X|.
\end{exercise}
\emph{Proposition}: If |X| is finite, then it has no limit points.

\begin{spec}
∀ p ∈ ℝ? not (Limp p X)
\end{spec}
%
This is a good exercise in quantifier negation!
%
\begin{spec}
  not (Limp p X)                                   = {- Def. of |Limp| -}
  not (∃ getq : Q? ∀ ε > 0? getq ε ∈ B p ε)       = {- Negation of existential -}
  ∀ getq : Q? not (∀ ε > 0? getq ε ∈ B p ε)       = {- Negation of universal -}
  ∀ getq : Q? ∃ ε > 0? not (getq ε ∈ B p ε)       = {- Simplification -}
  ∀ getq : Q? ∃ ε > 0? absBar (getq ε - p) >= ε
\end{spec}
%
Thus, using the ``functional interpretation'' of this type we see that
a proof needs a function |noLim|
%
\begin{spec}
noLim : (getq : Q) → RPos
\end{spec}
%
such that |let ε = noLim getq in absBar (getq ε - p) >= ε|.

Note that |noLim| is a \emph{higher-order} function: it takes a
function |getq| as an argument.
%
How can we analyse this function to find a suitable |ε|?
%
The key here is that the range of |getq| is |X - {p}| which is a
finite set (not containing |p|).
%
Thus we can enumerate all the possible results in a list |xs = [x1,
x2, {-"\ldots"-} xn]|, and measure their distances to |p|: |ds = map
(\x-> absBar (x - p)) xs|.
%
Now, if we let |ε = minimum ds| we can be certain that
%
|absBar (getq ε - p) >= ε| just as required (and |ε /= 0| because |p
`notElem` xs|).

\begin{exercise}
If |Limp p X| we now know that |X| is infinite.
\end{exercise}
%
Show how to construct an infinite sequence |a : Nat -> REAL| of points
in |X - {p}| which gets arbitrarily close to |p|.
%
Note that this construction can be seen as a proof of |Limp p X =>
Infinite X|.

\subsection{The limit of a sequence}
% TODO: transcribe more of the 2016 notes
\label{par:LimitOfSequence}

Now we can move from limit points to the more familiar limit of a
sequence.
%
At the core of DSLsofMath\jp{should this be a macro?} is the ability to analyse definitions from
mathematical texts, and here we will use the definition of the limit
of a sequence from \citet[page 498]{adams2010calculus}:

\begin{quote}
  We say that sequence ${a_n}$ converges to the limit $L$, and we
  write $\lim_{n→∞} a_n = L$, if for every positive real number |ε|
  there exists an integer $N$ (which may depend on |ε|) such that if
  $n > N$, then |absBar (an - L) < ε|.
\end{quote}
%

The first step is to type the variables introduced.
%
A sequence |a| is a function from |Nat| to |REAL|, thus |a : Nat ->
REAL| where |an| is special syntax for normal function application of
|a| to |n : Nat|.
%
Then we have |L : REAL|, |ε : RPos|, and |N : Nat| (or |N : RPos ->
Nat|).
%

In the next step we analyse the new concept introduced:
%
the syntactic form $\lim_{n→∞} a_n = L$ which we could express as an
infix binary predicate |haslim| where |a haslim L| is well-typed if |a
: Nat -> REAL| and |L : REAL|.
%

The third step is to formalise the definition using logic: we define
|haslim| using a ternary helper predicate |P|:
%
\begin{spec}
  a haslim L  =  ∀ ε > 0? P a L ε   -- ``for every positive real number |ε| \ldots''

  P a L ε  = ∃ N : Nat? ∀ n ≥ N? absBar (an - L) < ε
           = ∃ N : Nat? ∀ n ≥ N? an ∈ B L ε
           = ∃ N : Nat? I a N ⊆ B L ε
\end{spec}
%
where we have introduced an ``image function'' for sequences ``from |N| onward'':
%
\begin{spec}
  I : (Nat → X) → Nat → PS X
  I a N = {a n | n ≥ N}
\end{spec}

The ``forall-exists''-pattern is very common and it is often useful to
transform such formulas into another form.
%
In general |Forall (x : X) (Exists (y : Y) (Q x y))| is equivalent to
|Exists (gety : X -> Y) (Forall (x : X) (Q x (gety x)))|.
%
In the new form we more clearly see the function |gety| which shows
how the choice of |y| depends on |x|.
%
For our case with |haslim| we can thus write
%
\begin{spec}
  a haslim L  =  ∃ getN : RPos -> Nat? ∀ ε > 0? I a (getN ε)  ⊆  B L ε
\end{spec}
%
where we have made the function |getN| more visible.
%
The core evidence of |a haslim L| is the existence of such a function
(with suitable properties).

% TODO: perhaps swap the argument order in the definition of |Limp| to
% make it fit better with |haslim|.

\begin{exercise}
Prove that the limit of a sequence is unique.
\end{exercise}

%TODO: add the proof from file:~/src/DSLM/DSLsofMath/2016/Lectures/BasicConcepts.lhs

\begin{exercise}
prove that |(a1 haslim L1) & (a2 haslim L2)| implies
|(a1+a2) haslim (L1+L2)|.
\end{exercise}

% TODO: Perhaps include something about the relation between |haslim| and |Limp|

% TODO: Perhaps continue with one example: the limit of |invSeq| is |0|. See ../blackboard/W1/20170116_165408.jpg and 20170116_165412.jpg

When we are not interested in the exact limit, just that it exists, we
say that a sequence |a| is \emph{convergent} when |∃ L ? a haslim L|.

\subsection{Case study: The limit of a function}
\label{sec:LimitOfFunction}
%
As our next mathematical text book quote we take the definition of the
limit of a function of type |REAL -> REAL| from
\citet{adams2010calculus}:
%
\label{sec:FunLimit}
%
\begin{quote}
  \textbf{A formal definition of limit}

  We say that \(f(x)\) \textbf{approaches the limit} \(L\) as \(x\) \textbf{approaches} \(a\), and we write

  \[\lim_{x\to a} f(x) = L,\]

  if the following condition is satisfied:\\
  for every number \(\epsilon > 0\) there exists a number
  \(\delta > 0\), possibly depending on \(\epsilon\), such that if
  |0 < absBar (x - a) < delta|, then \(x\) belongs to the domain of \(f\)
  and
  \begin{spec}
    absBar (f(x) - L) < epsilon {-"."-}
  \end{spec}

\end{quote}
%
The |lim| notation has four components: a variable name |x|, a point
|a|, an expression \(f(x)\) and the limit |L|.
%
The variable name and the expression can be combined into just the
function |f| and this leaves us with three essential components: |f|,
|a|, and |L|.
%
Thus, |lim| can be seen as a ternary (3-argument) predicate which is
satisfied if the limit of |f| exists at |a| and equals |L|.
%
If we apply our logic toolbox we can define |lim| starting something like this:
%
\begin{spec}
lim f a L  =  Forall (epsilon > 0) (Exists (delta > 0) (P epsilon delta))
\end{spec}
%
when |P| is a predicate yet to define.
Indeed, it is often useful to introduce a local name (like |P| here) to help
break the definition down into more manageable parts.
%
If we now naively translate the last part we get this ``definition''
for |P|:
%
\begin{spec}
{-"\quad"-}  where  P epsilon delta = (0 < absBar (x - a) < delta) => (x `elem` Dom f  && absBar (f x - L) < epsilon))
\end{spec}
%
Note that there is a scoping problem: we have |f|, |a|, and |L| from
the ``call'' to |lim| and we have |epsilon| and |delta| from the two
quantifiers, but where did |x| come from?
%
It turns out that the formulation ``if \ldots then \ldots'' hides a
quantifier that binds |x|.
%
Thus we get this definition:
%
\begin{spec}
lim a f L  =  Forall (epsilon > 0) (Exists (delta > 0) (Forall x (P epsilon delta x)))
  where  P epsilon delta x = (0 < absBar (x - a) < delta) => (x `elem` Dom f  && absBar (f x - L) < epsilon))
\end{spec}
%
The predicate |lim| can be shown to be a partial function of two
arguments, |f| and |a|.
%
This means that each function |f| can have \emph{at most} one limit
|L| at a point |a|.
%
(This is not evident from the definition and proving it is a good
exercise.)
\jp{Exercise: what does Adams means by ``delta possibly depends on epsilon''? How did we express that in our formal defintion? Hint: how whould you express that delta cannot depend on epsilon?}

\subsection{Recap of syntax trees with variables, |Env| and |lookup|}

\jp{reformulate in book form (or move earlier, or remove)}

This was frequently a source of confusion already the first week so
there is already a question + answers earlier in this text.
%
But here is an additional example to help clarify the matter.
%
\begin{code}
data Rat v = RV v | FromI Integer | RPlus (Rat v) (Rat v) | RDiv (Rat v) (Rat v)
  deriving (Eq, Show)

newtype RatSem = RSem (Integer, Integer)
\end{code}
%
We have a type |Rat v| for the syntax trees of rational number
expressions (with variables of type |v|) and a type |RatSem| for the
semantics of those rational number expressions as pairs of integers.
%
The constructor |RV :: v -> Rat v| is used to embed variables with
names of type |v| in |Rat v|.
%
We could use |String| instead of |v| but with a type parameter |v| we
get more flexibility at the same time as we get better feedback from
the type checker.
%
Note that this type parameter serves a different purpose from the type
parameter in~\ref{sec:toComplexSyn}.

%
To evaluate some |e :: Rat v| we need to know how to evaluate the
variables we encounter.
%
What does ``evaluate'' mean for a variable?
%
Well, it just means that we must be able to translate a variable name
(of type |v|) to a semantic value (a rational number in this case).
%
To ``translate a name to a value'' we can use a function (of type |v
-> RatSem|) so we can give the following implementation of the
evaluator:
%
\begin{code}
evalRat1 ::  (v -> RatSem) -> (Rat v -> RatSem)
evalRat1 ev (RV v)       = ev v
evalRat1 ev (FromI i)    = fromISem i
evalRat1 ev (RPlus l r)  = plusSem  (evalRat1 ev l) (evalRat1 ev r)
evalRat1 ev (RDiv  l r)  = divSem   (evalRat1 ev l) (evalRat1 ev r)
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
%
\begin{code}
type Env v s = [(v, s)]

envToFun :: (Show v, Eq v) => Env v s -> (v -> s)
envToFun [] v = error ("envToFun: variable "++ show v ++" not found")
envToFun ((w,s):env) v
  | w == v     = s
  | otherwise  = envToFun env v
\end{code}
%
Thus, |Env v s| can be seen as an implementation of a ``lookup
table''.
%
It could also be implemented using hash tables or binary search trees,
but efficiency is not the point here.
%
Finally, with |envToFun| in our hands we can implement a second
version of the evaluator:
%
\begin{code}
evalRat2 :: (Show v, Eq v) => (Env v RatSem) -> (Rat v -> RatSem)
evalRat2 env e = evalRat1 (envToFun env) e
\end{code}

% \paragraph{The law of the excluded middle}
%
% Many had problems with implementing the ``law of the excluded middle''
% in the exercises and it is indeed a tricky property to prove.
% %
% They key to implementing it lies in double negation and as that is
% encoded with higher order functions it gets a bit hairy.
%
% TODO[Daniel]: more explanation

\paragraph{SET and PRED}
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
