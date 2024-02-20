\newpage
\section{Exercises}

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

\begin{exercise}\label{exc:evalLSpec}

  Show (by induction) that the evaluation function |evalL| (from page
  \pageref{def:evalL}) gives the same result as the formula
  \[P(x) = a_n x^n + a_{n-1} x^{n - 1} + \cdots + a_1 x + a_0\] from
  the quote on that same page.
\end{exercise}

\begin{exercise}\label{exc:polySpecList}
  Prove that, with |x = Poly [0, 1]| and |as = Poly [a0, a1,
  ..., an]|, we really have
%
%{
%format .* = "\mathbin{\cdot}"
  \begin{spec}
    as = a0 .* x^0 + a1 .* x^1 + a2 .* x^2 + ... + an .* x^n
  \end{spec}
  where |(+)| is addition of coefficient lists and |(.*)| is an infix version of
  |scaleList|.
%}
\end{exercise}

\begin{exercise}\label{ex:chebyshev}\textbf{\addtoindex{Chebyshev polynomials}.}
%
Let
\(T_n(x) = \cos (n*\arccos(x))\) for |x| in the interval |[-1,1]|.
%
First, compute \(T_0\), \(T_1\), and \(T_2\) by hand to get a
feeling for how it works.
%
Note that they all turn out to be polynomial functions.
%
In fact, \(T_n\) is a polynomial function of degree |n| for all |n|.
%
Your task is to to prove this, using these five hints:
%
\begin{itemize}
\item \(cos(\alpha)+cos(\beta)=2*\cos(\frac{\alpha+\beta}{2})*\cos(\frac{\alpha-\beta}{2})\)
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

\begin{exercise}\label{exc:Poly'}
  What are the ring operations on |Poly' A| where
%
  \begin{spec}
    Poly' A = { a : ℕ → A | {- |a| has a finite number of non-zero values -} }
  \end{spec}
\end{exercise}

\begin{exercise}
\index{degree}%
Prove the |degree| law
\begin{spec}
 ∀ x, y? degree (x `op` y) = degree x  +  degree y
\end{spec}
for polynomials.
\end{exercise}

\begin{exercise}\label{exc:degreeMonoidHomomorphism}
Is |degree| a homomorphism from the monoid |(Poly a, 1, *)| to the
monoid |(Maybe Nat, Just 0, opMaybe)|?
%
\index{opMaybe@@|opMaybe|}%
%
Check all the |Monoid| and homomorphism properties in the claim.
\end{exercise}

\begin{exercise}
  The helper function |polyMap :: (a->b) -> (Poly a -> Poly b)| that
  was used in the implementation of |negPoly| is a close relative of
  the usual |map :: (a->b) -> ([a] -> [b])|.
\index{Functor@@|Functor| (type class)}%
  Both these are members of a type class called |Functor|:
%
\begin{spec}
class Functor f where
  fmap :: (a->b) -> (f a -> f b)
\end{spec}
%
  Implement an instance of |Functor| for |Maybe| and |ComplexSyn| from
  Chapter 1.
%
  Is |fmap f| a homomorphism?
\end{exercise}

\begin{exercise}
  \label{ex:maximum-homo}
  Can the function |maximum :: [ℤ] -> ℤ| be defined as a homomorphism?
\end{exercise}

\begin{solution}
  Some information is lacking, but we assume a monoid homomorphism is
  requested.
  %
  We write |maxi| instead of |maximum| below to keep it short.
  %{
  %format maximum = maxi
  %
  The source type (lists) is a monoid with |op=(++)| and |unit=[]| and
  we are looking for a monoid structure on |ℤ|.
  %
  The homomorphism conditions are then |maximum (xs ++ ys)| |=| |op
  (maximum xs) (maximum ys)| and |maximum [] = unit|, for some
  operation |op : ℤ -> ℤ -> ℤ| and a constant |unit : ℤ|, forming a
  monoid.

  Because of what maximum does, we must pick |op = max|.
  %
  The |unit| must act like an identity for |max|:
  %
  |max unit x = x|.
  %
  This is possible only if |unit ≤ x| for every |x|.
  %
  But, this is not possible: there is no lower bound in |ℤ|.
  %
  Thus, |maximum| is \emph{not} a (monoid) homomorphism from |[ℤ]| to |ℤ|.
  %
  (If we create another type |ℤ'| with |-∞| added to |ℤ| we could
  define another |maxi' : [ℤ] -> ℤ'|).
  %}
\end{solution}
%TODO: more exercises!
