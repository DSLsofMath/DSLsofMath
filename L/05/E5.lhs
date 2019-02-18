\newpage
\subsection{Exercises}

The first few exercises are about filling in the gaps in the chapter
above.

\begin{exercise}
  Polynomial multiplication.
  %
  To get a feeling for the definition it can be useful to take it step
  by step, starting with some easy cases.
  %
  \begin{spec}
    mulP [] p = -- TODO
    mulP p [] = -- TODO
  \end{spec}
  % Answer: [] because 0*p=0
  \begin{spec}
    mulP [a] p = -- TODO
    mulP p [b] = -- TODO
  \end{spec}
  % Answer: map (a*) p and map (*b) p because scaling a polynomial by a constant is done by scaling all coefficients.
  \begin{spec}
    mulP (0:as) p = -- TODO
    mulP p (0:bs) = -- TODO
  \end{spec}
  % Answer: 0:(mulP as p) and 0:(mulP p bs) because (x*p)*q = x*(p*q)
  Finally we reach the main case
  \begin{spec}
    mulP (a:as) q@(b:bs) = -- TODO
  \end{spec}
  % Answer: (a*b):(map (a*) bs `addP`  (as  `mulP`  q)  because  we can first split (a:as) into [a] + (0:as), etc.

% TODO: format and move into solutions appendix
% (a:as) * q =
% ([a] + (0:as)) * q =
% ([a]*q) + (0:as)*q =
% (map (a*) q) + 0:(as*q) =
% (map (a*) (b:bs)) + 0:(as*q) =
% ((a*b):map (a*) bs)) + 0:(as*q) =
% ((a*b)+0) : (map (a*) bs) + as*q) =
% a*b  :  (map (a*) bs) + as*q) =
\end{exercise}

\begin{exercise}

  Show (by induction) that the evaluation function |evalL| gives the
  same result as the formula \[P(x) = a_n x^n + a_{n-1} x^{n - 1} +
  \cdots + a_1 x + a_0\].

\end{exercise}

\begin{exercise}
  Prove that, with the definition of |x = [0, 1]| we really have
%
  \begin{spec}
    as = a0 + a1 * x + a2 * x^2 + ... + an * x^n
  \end{spec}
\end{exercise}

\begin{exercise}\label{ex:chebyshev}\textbf{Chebyshev polynomials.}
Let
\(T_n(x) = \cos (n*\arccos(x))\).
%
Compute \(T_0\), \(T_1\), and \(T_2\) by hand to get a
feeling for how it works.
%
Note that they all turn out to be (simple) polynomial functions.
%
In fact, \(T_n\) is a polynomial function of degree |n| for all |n|.
%
To prove this, here are a few hints:

\begin{itemize}
\item \(cos(\alpha)+cos(\beta)=2\cos((\alpha+\beta)/2)\cos((\alpha-\beta)/2)\)
\item let \(\alpha = (n+1)*\arccos(x)\) and \(\beta = (n-1)*\arccos(x)\)
\item Simplify \(T_{n+1}(x)+T_{n-1}(x)\) to relate it to \(T_n(x)\).
\item Note that the relation can be seen as an inductive definition of \(T_{n+1}(x)\).
\item Use induction on |n|.
\end{itemize}
\end{exercise}

\begin{exercise}
  Another view of |Tn| from Exercise~\ref{ex:chebyshev} is as a
  homomorphism.
%
  Let |H1(h,F,f) = Forall x (h(F x) == f (h x))| be the predicate
  that states ``|h : A -> B| is a homomorphism from |F : A -> A| to |f : B -> B|''.
%
  Show that |H1(cos,(n*),Tn)| holds, where |cos : RPosz -> [-1,1]|,
  |(n*) : RPosz -> RPosz|, and |Tn : [-1,1] -> [-1,1]|.
\end{exercise}

\begin{exercise}
  Complete the following definition for polynomials represented as a
  plain list of coefficients:
  \begin{spec}
    instance Num a => Num [a] where
      (+)  = addP
      (*)  = mulP
      -- ... TODO

    addP  :: Num a => [a] -> [a] -> [a]
    addP  = zipWith' (+)
    mulP  :: Num a => [a] -> [a] -> [a]
    mulP  = -- TODO
  \end{spec}
  %
  Note that |zipWith'| is almost, but not quite, the definition of
  |zipWith| from the standard Haskell prelude.
  % Hint: |zipWith as [] = []| but |zipWith' as [] = as|
  % blackboard/W5/20170213_114418.jpg
\end{exercise}

\begin{exercise}
  What are the ring operations on |Poly' A| where
%
  \begin{spec}
    Poly' A = { a : ℕ → A | {- |a| has only a finite number of non-zero values -} }
  \end{spec}
\end{exercise}

\begin{exercise}
Prove the |degree| law
\begin{spec}
 ∀ x, y? degree (x `op` y) = degree x  +  degree y
\end{spec}
for polynomials.
\end{exercise}
\begin{exercise}
Check all the |Monoid| and homomorphism properties in this claim:
``|degree| is a monoid homomorphism from |(Poly a, 1, *)| to |(Maybe Nat, Just 0, opMaybe)|''.
\end{exercise}
\begin{exercise}

  The helper function |mapPoly :: (a->b) -> (Poly a -> Poly b)| that
  was used in the implementation of |polyNeg| is a close relative of
  the usual |map :: (a->b) -> ([a] -> [b])|.
%
  Both these are members of a typeclass called |Functor|:
%
\begin{spec}
class Functor f where
  fmap :: (a->b) -> (f a -> f b)
\end{spec}
%
  Implement an instance of |Functor| for |Maybe| and |ComplexSyn| from
  Chapter 1 and for |Rat| from Chapter 2.

  Is |fmap f| a homomorphism?

\end{exercise}

%TODO: more exercises!
