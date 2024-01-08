\section{Example proofs: contradiction, cases, primes}
\subsection{Proof by contradiction}

% See also ProofByContradiction.hs
Let us express and prove the irrationality of the square
root of 2.
%
We have two main concepts involved: the predicate ``irrational'' and the
function ``square root of''.
%
The square root function can be specified by the relation between two
positive real numbers $r$ and $s$ as $r = \sqrt{s}$ iff |r^2 == s|.
%
The formula ``x is irrational'' is just |not(R x)| where |R| is the
predicate ``is rational''.\footnote{In fact we additionally require the rational to be normalised (no common factor between the denominator and numerator) to simplify the proof.}
%
\begin{spec}
  R x = Exists (a:ZZ) (Exists (b:Pos) (b*x==a & GCD#(a,b)==1))
\end{spec}
The pattern ``proof by contradiction'' says that a way to prove some
statement |P| is to assume |not P| and derive something absurd.
%
%This pattern was formalised in \cref{sec:intuitionistic-logic} as |Not
%a = a `Implies` False|.
%
The traditional ``absurd'' statement is to prove simultaneously some
|Q| and |not Q|, for example.

Let us take |P = not (R r)| so that |not P = not (not (R r)) = R r|
and try with |Q = GCD#(a,b)==1|.
%
Assuming |not P| we immediately get |Q| so what we need is to prove
|not Q|, that is |GCD(a,b)/=1|.
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
But then |2| is a factor of both |a| and |b|, which means that
|2| is a factor of |GCD#(a,b)|, which in turn implies |not Q|.

To sum up: by assuming |not P| we can prove both |Q| and |not Q|.
%
Thus, by contradiction, |P| must hold (the square root of two is
irrational).

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
What about the third clause: is |x=p^q=r^r| rational?
%
By the principle of the excluded middle (\cref{sec:excluded-middle}),
we know that either |R x| or |not (R x)| must hold.
%
Then, we apply ∨-elimination, and thus we have to deal with the two
possible cases separately.

Case 1: |R x| holds.
%
Then we have a proof of |S| with |p=q=r=sqrt 2|.

Case 2: |not (R x)| holds.
%
Then we have another irrational number |x| to play with.
%
Let's try |p=x| and |q=r|.
%
Then |p^q = x^r = (r^r)^r = r^(r*r) = r^2 = 2| which is clearly
rational.
%
Thus, also in this case we have a proof of |S|, but now with |p=r^r|
and |q=r|.

To sum up: yes, there are irrational numbers such that their power is
rational.
%
We can prove the existence without knowing what numbers |p| and |q|
actually are: this is because negation-elimination is a \emph{non-constructive principle}.
The best we could do in an intuitionistic logic, which is constructive, is to show that, if they were not to exist,
then we come to a contradiction.
%

\subsection{There is always another prime}

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

To prove this formula we are going to translate from logic to programs as
described in \cref{sec:curry-howard}.
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
This time the proof is going to be constructive: we have to find a concrete bigger prime, |m|.
We can start filling in the definition of |proof| as a
2-argument function returning a triple. % nested pair
The key idea is to consider |1+factorial n| as a candidate new prime:
%
\begin{spec}
proof n pn = (m, (pm, gt))
  where  m'  = 1 + factorial n
         m   = {- some non-trivial prime factor of |m'| -}
         pm  = {- a proof that |m| is prime -}
         gt  = {- a proof that |m>n| -}
\end{spec}
%
The proof |pm| is the core of the theorem.
%
Let |mod x y| be the remainder after integer division of |x| by |y| and 
%
%format ==# = == "_{p}"
let |(==#)| denote ``equality modulo p'': |x ==# y = mod x p == mod y p|.
%
Then note that for any number |p| where |2<=p<=n| we have |n! ==# 0|.
%
We then calculate
%
\begin{spec}
 m'           ==#  {- Def. of |m'| -}
 1 + n!       ==#  {- modulo distributes over |+|, and |n! ==# 0| -}
 1
\end{spec}
%
Thus |m'| is not divisible by any number from |2| to |n|.
%
But is it a prime?
%
Here we could, as before, use the law of excluded middle to progress.
%
But we don't have to, because primality is a \emph{decidable
property}: we can write a terminating function which checks if |m'| is
prime.
%
We can then proceed by case analysis again:
%
If |m'| is prime then |m=m'| and the proof is done (because |1+n! >= 1
+ n > n|).
%
Otherwise, let |m| be a prime factor of |m'| (thus |m'=m*q|, |q>1|).
%
Then |1 ==# m' ==# (mod m p)*(mod q p)| which means that neither
|m| nor |q| are divisible by |p| (otherwise the product would be
zero).
%
As |m| is thus not divisible by any number from |2| to |n|, it has to
be bigger than |n|.
%
QED.

The constructive character of this proofs means that it can be used to
define a (mildly useful) function which takes any prime number to
some larger prime number.
%
We can compute a few example values:

\begin{tabular}{r@@{ $\mapsto$ }rl}
     2 &  3 &( 1+2! )
  \\ 3 &  7 &( 1+3! )
  \\ 5 & 11 &( 1+5! = 121 = 11*11 )
  \\ 7 & 71 &\ldots
\end{tabular}
