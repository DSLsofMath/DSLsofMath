%if False
\begin{code}
module UnusualStream where
import DSLsofMath.FunNumInst
type REAL = Double
(deriv, integ) = undefined
\end{code}
%endif
\paragraph{An unusual stream}

(Credits: \citep{Pavlovic:1998:CCF:788020.788885})

Now consider |X = REAL -> REAL|, and |A = REAL| with the following definitions:

\begin{code}
type X = REAL -> REAL
type A = REAL
deriv :: X -> X
integ :: X -> X
head  f    = f 0                 -- value of |f| at |0|
tail  f    = deriv f             -- derivative of |f|
cons a f   = const a + integ f   -- start at |a|, integrate |f| from |0|
\end{code}

Then the first law becomes

\begin{spec}
law1c f =
  f  ==  cons (head f) (tail f)
     ==  (head f) + integ (tail f)
     ==  f 0  +  integ (deriv f)
\end{spec}
or, in traditional notation:

\[
  f(x) = f(0) + \int_0^x f'(t) \text{d}t
\]

which we recognize as the fundamental law of calculus!
%
There is much more to discover in this direction and we present some
of it in the next few chapters.
%

\paragraph{For the curious.}
%
Here are the other two stream laws, in case you wondered.
%
\begin{spec}
law2c a f  =
  f  ==  tail (cons a f)
     ==  deriv (const a + integ f)
     ==  deriv (integ f)
   \end{spec}
\jp{The last line is actually obscuring what the law is saying. IMO no calculus-dependent rewriting should take place here.}
%
\begin{spec}
law3c a f  =
  a  ==  head (cons a f)
  a  ==  head (const a + integ f)
  a  ==  (const a + integ f) 0
  a  ==  a + (integ f) 0
  0  ==  integ f 0
\end{spec}
\jp{Likewise}
