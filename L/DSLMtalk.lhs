\documentclass{beamer}
\usetheme{Madrid}
% Hide navigation symbols
\setbeamertemplate{navigation symbols}{}
\usepackage{amsmath}
\usepackage{amsthm}
\usepackage{mathrsfs}
%\usepackage{textgreek}
%include polycode.fmt
%include ../L/dslm.format
%%% Somewhat updated version of polycode.fmt from the lhs2TeX dist.
% %include dslmagda.fmt
%%% Our own formatting directives
%include dslm.format
\usepackage{natbib}
\usepackage{wrapfig}
\usepackage{graphicx}
\usepackage{hyperref}
\hypersetup{pdfpagemode={FullScreen}}
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8x]{inputenc}
\RequirePackage{ucs}
\RequirePackage{amsfonts}
\usepackage{tikz}
\usepackage{tikz-cd}
\usetikzlibrary{trees,graphs,quotes}
\usepackage{lineno}
\usepackage{lmodern}

\providecommand\mathbbm{\mathbb}
\providecommand{\paragraph}{}

%TODO: Define more of these ...
\DeclareUnicodeCharacter{737}{\textsuperscript{l}}
\DeclareUnicodeCharacter{8718}{\ensuremath{\blacksquare}}
\DeclareUnicodeCharacter{8759}{::}
\DeclareUnicodeCharacter{9669}{\ensuremath{\triangleleft}}
\DeclareUnicodeCharacter{8799}{\ensuremath{\stackrel{\scriptscriptstyle ?}{=}}}
\DeclareUnicodeCharacter{10214}{\ensuremath{\llbracket}}
\DeclareUnicodeCharacter{10215}{\ensuremath{\rrbracket}}

%let submit = False
%if submit
\newcommand{\todo}[2][?]{}
%else
\newcommand{\todo}[2][?]{\marginpar{\raggedright\tiny{}TODO: #2}}
%endif

\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt plus 2pt minus 1pt}

\theoremstyle{definition}
\newtheorem{exercise}{Exercise}[section]
% Old exercise style (until 2017-11-08): \renewcommand*{\theenumi}{\textbf{E\thesection.\arabic{enumi}}}
\newcommand{\TODO}[1]{\todo{#1}}
\newcommand{\refSec}[1]{Sec. \ref{#1}}
\newcommand{\refSecs}[1]{Secs. \ref{#1}}
\newcommand{\refSecI}[1]{Section \ref{#1}}
\newcommand{\refSecsI}[1]{Sections \ref{#1}}
\newcommand{\refTab}[1]{Tab. \ref{#1}}
\newcommand{\tyconsym}[1]{\mathrel{{:}{#1}{:}}}
% the `doubleequals' macro is due to Jeremy Gibbons
\def\doubleequals{\mathrel{\unitlength 0.01em
  \begin{picture}(78,40)
    \put(7,34){\line(1,0){25}} \put(45,34){\line(1,0){25}}
    \put(7,14){\line(1,0){25}} \put(45,14){\line(1,0){25}}
  \end{picture}}}
%% If you remove the %format == command the lhs2TeX default yields ≡, which can be a problem
\def\tripleequals{\mathrel{\unitlength 0.01em
  \begin{picture}(116,40)
    \put(7,34){\line(1,0){25}} \put(45,34){\line(1,0){25}} \put(83,34){\line(1,0){25}}
    \put(7,14){\line(1,0){25}} \put(45,14){\line(1,0){25}} \put(83,14){\line(1,0){25}}
  \end{picture}}}
\providecommand{\cpp}{C\kern-0.05em\texttt{+\kern-0.03em+}}
\newcommand{\colvec}[1]{\colvecc{#1_0}{#1_n}}
\newcommand{\colvecc}[2]{\colveccc{#1 \\ \vdots \\ #2}}
\newcommand{\colveccc}[1]{\begin{bmatrix} #1 \end{bmatrix}}
\newcommand{\rowvec}[1]{\rowvecc{#1_0}{#1_n}}
\newcommand{\rowvecc}[2]{\rowveccc{#1 &  \cdots &  #2}}
\newcommand{\rowveccc}[1]{\begin{bmatrix} #1 \end{bmatrix}}

\title{DSLsofMath: Typing Mathematics}
\author[Jansson \& Ionescu]{Patrik Jansson \and Cezar Ionescu}
\institute[FP div., Chalmers]{Functional Programming division, Chalmers University of Technology}
\date{2018-01-10}
% {Chalmers Univeristy of Technology, Sweden}
%           {\texttt{patrikj@@chalmers.se}}
%           {\texttt{cezar@@chalmers.se}}
\begin{document}
\begin{frame}
\maketitle
\end{frame}
\begin{frame}{What is ``DSLs of Math''?}

  ``Domain Specific Languages of Mathematics''

  \begin{itemize}
  \item A BSc-level course (2016-01 CeIo, 2017 onwards: PaJa, DaSc)
  \item A pedagogical project to develop the course (DaHe, SoEi)
  \item A BSc thesis project ``DSLsofMath for other courses''
  \end{itemize}

  Aim: ``\ldots improve the mathematical education of computer
  scientists and the computer science education of mathematicians.''

  Focus on types \& specifications, syntax \& semantics

  DSL examples: Power series, Differential equations, Linear Algebra
\end{frame}

\begin{frame}{DSLsofMath learning outcomes}

\begin{itemize}
\item Knowledge and understanding
  \begin{itemize}
  \item design and implement a DSL for a new domain
  \item organize areas of mathematics in DSL terms
  \item explain main concepts of analysis, algebra, and lin.\ alg.
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
\end{frame}

\section{Types in Mathematics}

\begin{frame}{Case 1: limits \citep{adams2010calculus}}

\begin{quote}
  We say that \(f(x)\) \textbf{approaches the limit} \(L\) as \(x\)
  \textbf{approaches} \(a\), and we write

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
\vspace{-0.5cm}
Four parts: name |x|, point |a|, expr.\ \(f(x)\), limit |L|.

Name + expr. combines to just |f|: thus three parts: |a|, |f|, and |L|.

%format Dom f = "\mathcal{D}" f
\only<1>{
\begin{spec}
lim a f L  =  Forall (epsilon > 0) (Exists (delta > 0) (P epsilon delta))
  where  P epsilon delta = (0 < absBar (x - a) < delta) => (x `elem` Dom f  && absBar (f x - L) < epsilon))
\end{spec}
}
\only<2>{
\begin{spec}
lim a f L  =  Forall (epsilon > 0) (Exists (delta > 0) (Forall x (P epsilon delta x)))
  where  P epsilon delta x = (0 < absBar (x - a) < delta) => (x `elem` Dom f  && absBar (f x - L) < epsilon))
\end{spec}
}
\end{frame}

\begin{frame}{Case 2: derivative}

\begin{quote}
  The \textbf{derivative} of a function |f| is another function |f'| defined by
%
  \[
    f'(x) = \lim_{h \to 0} \frac{f(x+h) - f(x)}{h}
  \]
%
  at all points |x| for which the limit exists (i.e., is a finite real
  number). If \(f'(x)\) exists, we say that |f| is \textbf{differentiable}
  at |x|.
\end{quote}

We can write

\savecolumns
\begin{spec}
  D f x  = lim 0 g        where            g  h = frac (f(x+h) - f x) h
\end{spec}
\pause
\restorecolumns
\begin{spec}
  D f x  = lim 0 (phi x)  where       phi  x  h = frac (f(x+h) - f x) h
\end{spec}
\pause
\restorecolumns
\begin{spec}
  D f    = lim 0 . psi f  where  psi  f    x  h = frac (f(x+h) - f x) h
\end{spec}
\end{frame}

\begin{frame}{Derivatives, cont.}

Examples:

\begin{spec}
  D : (REAL->REAL) -> (REAL->REAL)

  sq x      =  x^2
  double x  =  2*x
  c2 x      =  2
  sq'   =  D sq   = D (\x -> x^2) = D ({-"{}"-}^2) = (2*) = double
  sq''  =  D sq'  = D double = c2 = const 2
\end{spec}

Note: we cannot \emph{implement} |D| in Haskell.
%

Given only |f : REAL -> REAL| as a ``black box'' we
cannot compute the actual derivative |f' : REAL -> REAL|.

We need the ``source code'' of |f| to apply rules from calculus.

\end{frame}

\subsection{Type inference and understanding: Lagrangian case study}
\begin{frame}{Case 3: Lagrangian}

  From \citep{sussman2013functional}:

\begin{quote}
  A mechanical system is described by a Lagrangian function of the
  system state (time, coordinates, and velocities).
%
  A motion of the system is described by a path that gives the
  coordinates for each moment of time.
%
  A path is allowed if and only if it satisfies the Lagrange
  equations.
%
  Traditionally, the Lagrange equations are written

\[\frac{d}{dt} \frac{∂L}{∂\dot{q}} - \frac{∂L}{∂q} = 0\]

What could this expression possibly mean?

\end{quote}

\end{frame}

\begin{frame}{Lagrangian, cont.}
\[\frac{d}{dt} \frac{∂L}{∂\dot{q}} - \frac{∂L}{∂q} = 0\]

\begin{itemize}
\item The use of notation for ``partial derivative'', \(∂L / ∂q\), suggests
that |L| is a function of at least a pair of arguments:
\begin{spec}
  L : ℝⁱ → ℝ,    i ≥ 2
\end{spec}

This is consistent with the description: ``Lagrangian function of the
system state (time, coordinates, and velocities)''.
%
So, if we let ``coordinates'' be just one coordinate, we can take |i =
3|:
%
\begin{spec}
  L : ℝ³ → ℝ
\end{spec}
%
The ``system state'' here is a triple, of type |S = (T, Q, V)|,
and we can call the the three components |t : T| for time, |q : Q| for
coordinate, and |v : V| for velocity.
%
(|T = Q = V = ℝ|.)
\end{itemize}
\end{frame}

\begin{frame}{Lagrangian, cont.}
\[\frac{d}{dt} \frac{∂L}{∂\dot{q}} - \frac{∂L}{∂q} = 0\]

\begin{itemize}
\item Looking again at \(∂L / ∂q\), \(q\) is the name of a variable,
  one of the 3 args to \(L\).
%
  In the context, which we do not have, we would expect to find
  somewhere the definition of the Lagrangian as
  %
  \begin{spec}
    L  :  (T, Q, V)  ->  ℝ
    L     (t, q, v)  =   ...
  \end{spec}

\item therefore, \(∂L / ∂q\) should also be a function of the same
  triple:

  \begin{spec}
    (∂L / ∂q) : (T, Q, V) -> ℝ
  \end{spec}

  It follows that the equation expresses a relation between
  \emph{functions}, therefore the \(0\) on the right-hand side is
  \emph{not} the real number \(0\), but rather the constant function
  |const 0|:

  \begin{spec}
    const 0  :  (T, Q, V)  →  ℝ
    const 0     (t, q, v)  =   0
  \end{spec}
\end{itemize}
\end{frame}

\begin{frame}{Lagrangian, cont.}
\[\frac{d}{dt} \frac{∂L}{∂\dot{q}} - \frac{∂L}{∂q} = 0\]
\begin{itemize}
\item We now have a problem: |d / dt| can only be applied to functions
  of \emph{one} real argument |t|, and the result is a function of one
  real argument:

%format dotq = "\dot{q}"
%format ddotq =  ∂ dotq
%format juxtapose f x = f "\," x
\begin{spec}
    juxtapose (d / dt) (∂L / ∂dotq)  :  T → ℝ
\end{spec}

Since we subtract from this the function \(∂L / ∂q\), it follows that
this, too, must be of type |T -> ℝ|.
%
But we already typed it as |(T, Q, V) → ℝ|, contradiction!
%
\label{item:L:contra}

\item The expression \(∂L / ∂\dot{q}\) appears to also be malformed.
%
  We would expect a variable name where we find \(\dot{q}\), but
  \(\dot{q}\) is the same as \(dq / dt\), a function.
\end{itemize}
\end{frame}
\begin{frame}{Lagrangian, cont.}
\[\frac{d}{dt} \frac{∂L}{∂\dot{q}} - \frac{∂L}{∂q} = 0\]
\begin{itemize}
\item The only immediate candidate for an application of \(d/dt\) is
  ``a path that gives the coordinates for each moment of time''.
%
  Thus, the path is a function of time, let us say
%
  \begin{spec}
    w  :  T → Q  -- with |T| for time and |Q| for coords (|q : Q|)
  \end{spec}

  We can now guess that the use of the plural form ``equations'' might
  have something to do with the use of ``coordinates''.
%
  In an |n|-dim.\ space, a position is given by |n| coordinates.
%
  A path would then be
%
  \begin{spec}
    w  :  T → Q  -- with |Q = ℝⁿ|
  \end{spec}
%
  which is equivalent to |n| functions of type |T → ℝ|, each computing
  one coordinate as a function of time.
%
  We would then have an equation for each of them.
%
  We will use |n=1| for the rest of this example.

\end{itemize}

\end{frame}
\begin{frame}{Lagrangian, cont.}
\[\frac{d}{dt} \frac{∂L}{∂\dot{q}} - \frac{∂L}{∂q} = 0\]
\begin{itemize}
\item Now that we have a path, the coordinates at any time are given
  by the path.
  %
  And as the time derivative of a coordinate is a velocity, we can
  actually compute the trajectory of the full system state |(T, Q, V)|
  starting from just the path.
%
  \begin{spec}
    q  :  T → Q
    q t  =  w t        -- or, equivalently, |q = w|

    dotq : T → V
    dotq t = dw /dt    -- or, equivalently, |dotq = D w|
  \end{spec}
%
  We combine these in the ``combinator'' |expand|, given by
  %
  \begin{spec}
    expand : (T → Q) → (T → (T, Q, V))
    expand w t  =  (t, w t, D w t)
  \end{spec}
\end{itemize}
\end{frame}

\begin{frame}{Lagrangian, cont.}
\[\frac{d}{dt} \frac{∂L}{∂\dot{q}} - \frac{∂L}{∂q} = 0\]
\vspace{-0.5cm}
\begin{itemize}
\item With |expand| in our toolbox we can fix the typing problem.
  %
  \begin{spec}
    (∂L / ∂q) . (expand w)  :  T -> ℝ
  \end{spec}


\item We now move to using |D| for |d / dt|, |D₂| for |∂ / ∂q|, and
  |D₃| for |∂ / ∂dotq|.
  %
  In combination with |expand w| we find these type correct
  combinations for the two terms in the equation:
  %
  \begin{spec}
    D ((D₂ L)  ∘  (expand w))  :  T → ℝ
       (D₃ L)  ∘  (expand w )  :  T → ℝ
  \end{spec}

  The equation becomes
  %
  \begin{spec}
    D ((D₃ L) ∘ (expand w))  -  (D₂ L) ∘ (expand w)  =  const 0
  \end{spec}
  or, after simplification:
  \begin{spec}
    D (D₃ L ∘ expand w)  =  D₂ L ∘ expand w
  \end{spec}

\end{itemize}
\end{frame}

\begin{frame}{Case 3: Lagrangian, summary}

  ``A path is allowed if and only if it satisfies the Lagrange
  equations'' means that this equation is a predicate on paths:
  %
  \begin{spec}
    Lagrange(L, w) =  D (D₃ L ∘ expand w) == D₂ L ∘ expand w
  \end{spec}
  %

  Thus: If we can describe a mechanical system in terms of ``a
  Lagrangian'' (|L : S -> ℝ|), then we can use the predicate to check
  if a particular candidate path |w : T → ℝ| qualifies as a ``motion
  of the system'' or not.
%
  The unknown of the equation is the path |w|, and the equation is an
  example of a partial differential equation (a PDE).

\end{frame}

\begin{frame}{DSLsofMath: Typing Mathematics}

  \begin{itemize}
  \item Mathematical concepts like |lim|, |D|, |Lagrangian| can be
    explored and explained using typed functional programming.
  \item Sometimes new insights arise: Stream calculus, for example.
  \item Aim: ``\ldots improve the mathematical education of computer
    scientists and the computer science education of mathematicians.''
  \item Focus on types \& specifications, syntax \& semantics
  \item DSL examples: Power series, Differential equations, Linear
    Algebra
  \end{itemize}

\end{frame}
\bibliographystyle{abbrvnat}
\begin{frame}{Bibliography}
\bibliography{ref}
\end{frame}
\end{document}




%TODO (by DaHe) There's two more things I think should be added in this
% chapter:
% * Typing the conditional probability notation. The notation P(A | B) is
%   something that I and others were confused during the statistics course. In one
%   assignment during that course, my solution claimed somthing along the
%   lines of that {P | A} was an event, that had a certain probability. So I
%   think many would agree that this is indeed a very confusing notation, so it
%   is a great idea to cover it in this book. Cezar had a very good rant about
%   this during his guest lecture last year.
%TODO: [Include the problem from E3.lhs and the solution from ../../Exam/2016-03/Ex2.hs]
% * Using Haskell to type mathematical expressions. I leared a lot about typing
%   maths by playing around with mathematical expressions in Haskell. A good
%   example is Exam/2016-03/Ex2.hs, where the solution is a haskell program
%   which compiles and type checks. This has a similar advantage to the 'typed
%   hole' method in the previous chapter, in that it encourages students to
%   take the trial-end-error approach that can often be used when solving
%   programming problems, and apply it to a mathematical context. I think this
%   chapter should include at least one example of using this method to type a
%   mathematical expression.

\subsection{Type in Mathematics (Part II)}

\subsubsection{Type classes}

The kind of type inference we presented in the last lecture becomes
automatic with experience in a domain, but is very useful in the
beginning.

The ``trick'' of looking for an appropriate combinator with which to
pre- or post-compose a function in order to makes types match is often
useful.
%
It is similar to the casts one does automatically in expressions such
as \(4 + 2.5\).

One way to understand such casts from the point of view of functional
programming is via \emph{type classes}.
%
As a reminder, the reason \(4 + 2.5\) works is because floating point
values are members of the class |Num|, which includes the member
function

\begin{spec}
  fromInteger   ::  Integer  ->  a
\end{spec}

which converts integers to the actual type |a|.

Type classes are related to mathematical structures which, in turn,
are related to DSLs.
%
The structuralist point of view in mathematics is that each
mathematical domain has its own fundamental structures.
%
Once these have been identified, one tries to push their study as far
as possible \emph{on their own terms}, i.e., without introducing other
structures.
%
For example, in group theory, one starts by exploring the consequences
of just the group structure, before one introduces, say, an order
structure and monotonicity.

The type classes of Haskell seem to have been introduced without
relation to their mathematical counterparts, perhaps because of
pragmatic considerations.
%
For now, we examine the numerical type classes |Num|, |Fractional|, and
|Floating|.

\begin{spec}
class  (Eq a, Show a) => Num a  where
    (+), (-), (*)  :: a -> a -> a
    negate         :: a -> a
    abs, signum    :: a -> a
    fromInteger    :: Integer -> a
\end{spec}

This is taken from the Haskell documentation\footnote{Fig. 6.2 in
  \href{https://www.haskell.org/onlinereport/haskell2010/haskellch6.html}{section
    6.4 of the Haskell 2010 report}: \cite[Sect.~6.4]{haskell2010}.}
but it appears that |Eq| and |Show| are not necessary, because there
are meaningful instances of |Num| which don't support them:
%
\begin{code}
instance Num a => Num (x -> a) where
  f + g        =  \x -> f x + g x
  f - g        =  \x -> f x - g x
  f * g        =  \x -> f x * g x
  negate f     =  negate . f
  abs f        =  abs . f
  signum f     =  signum . f
  fromInteger  =  const . fromInteger
\end{code}

This instance for functions allows us to write expressions like |sin +
cos :: Double -> Double| or |sq * double :: Integer -> Integer|.
%
As another example:
\begin{spec}
  sin^2 = \x -> (sin x)^(const 2 x) = \x -> (sin x)^2
\end{spec}
%
thus the typical math notation \(\sin^2\) works fine in Haskell.
%
(Note that there is a clash with another use of superscript for functions: sometimes |f^n| means \emph{composition} of |f| with itself |n| times.
%
With that reading \(sin^2\) would mean |\x->sin (sin x)|.)

%
Exercise: play around with this a bit in ghci.


\subsubsection{Overloaded integers literals}

As an aside, we will spend some time explaining a convenient syntactic
shorthand which is very useful but which can be confusing: overloaded
integers.
%
In Haskell, every use of an integer literal like |2|, |1738|, etc., is
actually implicitly an application of |fromInteger| to the literal.
%
This means that the same program text can have different meaning
depending on the type of the context.
%
The literal |three = 3|, for example, can be used as an integer, a real
number, a complex number, or even as a (constant) function (by the
instance |Num (x -> a)|).

The instance declaration of the method |fromInteger| above looks
recursive, but is not.
%
The same pattern appeared already in section
\ref{sec:firstFromInteger}, which near the end included roughly the
following lines:

\begin{spec}
instance Num r => Num (ComplexSyn r) where
  -- ... several other methods and then
  fromInteger = toComplexSyn . fromInteger
\end{spec}

To see why this is not a recursive definition we need to expand the
type and to do this I will introduce a name for the right hand side
(RHS): |fromIntC|.

\begin{verbatim}
--          ComplexSyn r <---------- r <---------- Integer
fromIntC =              toComplexSyn . fromInteger
\end{verbatim}

I have placed the types in the comment, with ``backwards-pointing''
arrows indicating that
%
|fromInteger :: Integer -> r| and
%
|toComplexSyn :: r -> ComplexSyn r|
%
while the resulting function is
%
|fromIntC :: Integer -> ComplexSyn r|.
%
The use of |fromInteger| at type |r| means that the full type of
|fromIntC| must refer to the |Num| class.
%
Thus we arrive at the full type:
%
\begin{spec}
fromIntC :: Num r =>   Integer -> ComplexSyn r
\end{spec}

As an example we have that
\begin{spec}
  3 :: ComplexSyn Double                 ==  {- |Integer| literals have an implicit |fromInteger| -}
  (fromInteger 3) :: ComplexSyn Double   ==  {- |Num| instance for |ComplexSyn| -}
  toComplexSyn   (fromInteger 3)         ==  {- |Num| instance for |Double| -}
  toComplexSyn   3.0                     ==  {- Def. of |toComplexSyn| from Section \ref{sec:toComplexSyn} -}
  FromCartesian  3.0  0                  ==  {- |Integer| literals have an implicit |fromInteger| -}
  FromCartesian  3.0  (fromInteger 0)    ==  {- |Num| instance for |Double|, again -}
  FromCartesian  3.0  0.0
\end{spec}

\subsubsection{Back to the numeric hierarchy instances for functions}

Back to the main track: defining numeric operations on functions.
%
We have already defined the operations of the |Num| class, but we can
move on to the neighbouring classes |Fractional| and |Floating|.

The class |Fractional| is for types which in addition to the |Num|
operations also supports division:
%
\begin{spec}
class  Num a => Fractional a  where
    (/)           :: a -> a -> a
    recip         :: a -> a             -- |\x -> 1/x|
    fromRational  :: Rational -> a      -- similar to |fromInteger|
\end{spec}
and the |Floating| class collects the ``standard'' functions from
calculus:
\begin{spec}
class  Fractional a => Floating a  where
    pi                   :: a
    exp, log, sqrt       :: a -> a
    (**), logBase        :: a -> a -> a
    sin, cos, tan        :: a -> a
    asin, acos, atan     :: a -> a
    sinh, cosh, tanh     :: a -> a
    asinh, acosh, atanh  :: a -> a
\end{spec}

We can instantiate these type classes for functions in the same way we
did for |Num|:

\begin{code}
instance Fractional a => Fractional (x -> a) where
  recip  f         =  recip . f
  fromRational     =  const . fromRational
\end{code}

\begin{code}
instance Floating a => Floating (x -> a) where
  pi       =  const pi
  exp f    =  exp . f
  f ** g   =  \ x -> (f x)**(g x)
  -- and so on
\end{code}

Exercise: complete the instance declarations.

These type classes represent an abstract language of algebraic and
standard operations, abstract in the sense that the exact nature of
the elements involved is not important from the point of view of the
type class, only from that of its implementation.


\subsection{Type classes in Haskell}

We now abstract from |Num| and look at what a type class is and how it
is used.
%
One view of a type class is as a set of types.
%
For |Num| that is the set of ``numeric types'', for |Eq| the set of
``types with computable equality'', etc.
%
The types in this set are called instances and are declared by
|instance| declarations.
%
When a class |C| is defined, there are no types in this set (no
instances).
%
In each Haskell module where |C| is in scope there is a certain
collection of instance declarations.
%
Here is an example of a class with just two instances:
%
\begin{code}
class C a where
  foo :: a -> a
instance C Integer where
  foo = (1+)
instance C Char where
  foo = toUpper
\end{code}
%
Here we see the second view of a type class: as a collection of
overloaded methods (here just |foo|).
%
Overloaded here means that the same symbol can be used with different
meaning at different types.
%
If we use |foo| with an integer it will add one, but if we use it with
a character it will convert it to upper case.
%
The full type of |foo| is |C a => a -> a| and this means that it can
be used at any type |a| for which there is an instance of |C| in
scope.

Instance declarations can also be parameterised:
\begin{code}
instance C a => C [a] where
  foo xs = map foo xs
\end{code}
This means that for any type |a| which is already an instance of |C|
we also make the type |[a]| an instance (recursively).
%
Thus, we now have an infinite collection of instances of |C|: |Char|,
|[Char]|, |[[Char]]|, etc.
%
Similarly, with the function instance for |Num| above, we immediately
make the types |x->Double|, |x->(y->Double)|, etc.\ into instances
(for all |x|, |y|, \ldots).

%TODO: parhaps make the ``looks recursive'' |fromInteger| example talk
% about |foo| instead? (And then just mention |fromInteger| shortly.)

\subsection{Computing derivatives}
\label{sec:computingDerivatives}

An important part of calculus is the collection of laws, or rules, for
computing derivatives.
%
Using the notation |D f| for the derivative of |f| and lifting the
numeric operations to functions we can fill in a nice table of
examples which can be followed to compute derivatives of many
functions:
%
\begin{spec}
    D (f + g)         =  D f + D g
    D (f * g)         =  D f * g + f * D g

    D (f ∘ g) x       =  D f (g x) * D g x     -- the chain rule

    D (const a)       =  const 0
    D id              =  const 1
    D (powTo n)  x    =  (n - 1) * (x^(n-1))
    D sin   x         =  cos x
    D cos   x         =  - (sin x)
    D exp   x         =  exp x
\end{spec}
%
and so on.
%

If we want to get a bit closer to actually implementing |D| we quickly
notice a problem: if |D| has type |(REAL -> REAL) -> (REAL -> REAL)|
we have no way of telling which of these rules we should apply.
%
Given a real (semantic) function |f| as an argument, |D| cannot know
if this function was written using a |+|, or |sin| or |exp| as
outermost operation.
%
The only thing |D| could do would be to numerically approximate the
derivative, and that is not what we are exploring in this course.
%
Thus we need to take a step back and change the type that we work on.
%
All the rules in the table seem to work on \emph{syntactic} functions:
abstract syntax trees \emph{representing} the real (semantic)
functions.

We observe that we can compute derivatives for any expressions made
out of arithmetical functions, standard functions, and their
compositions.
%
In other words, the computation of derivatives is based on a domain
specific langauge (a DSL) of expressions (representing functions in
one variable).
%
Here is the start of a grammar for this little language:
%
\begin{spec}
   expression  ∷=  const ℝ
               |   id
               |   expression + expression
               |   expression * expression
               |   exp expression
               |   ...
\end{spec}

We can implement this in a datatype:
\label{sec:FunExp}
\begin{code}
data FunExp  =  Const Double
             |  Id
             |  FunExp  :+:  FunExp
             |  FunExp  :*:  FunExp
             |  Exp FunExp
             -- and so on
             deriving Show
\end{code}

The intended meaning of elements of the |FunExp| type is functions:

\begin{code}
eval  ::  FunExp         ->  (Double -> Double)
eval      (Const alpha)  =   const alpha
eval      Id             =   id
eval      (e1 :+: e2)    =   eval e1  +  eval e2    -- note the use of ``lifted |+|'',
eval      (e1 :*: e2)    =   eval e1  *  eval e2    -- ``lifted |*|'',
eval      (Exp e1)       =   exp (eval e1)          -- and ``lifted |exp|''.
-- and so on
\end{code}
%
An example:
\begin{code}
f1 :: Double -> Double
f1 x = exp (x^2)
e1 :: FunExp
e1 = Exp (Id :*: Id)
\end{code}

We can implement the derivative of |FunExp| expressions using the
rules of derivatives.
%
We want to implement a function |derive :: FunExp -> FunExp| which
makes the following diagram commute:

\quad%
\begin{tikzcd}
  |FunExp| \arrow[r, "|eval|"] \arrow[d, "|derive|"]  & |Func| \arrow[d, "D"] \\
  |FunExp| \arrow[r, "|eval|"]                        & |Func|
\end{tikzcd}

In other words we want
%
\begin{spec}
     eval . derive e  =  D . eval
\end{spec}
or, in other words, for any expression |e|, we want
%
\begin{spec}
     eval (derive e)  =  D (eval e)
\end{spec}

For example, let us derive the |derive| function for |Exp e|:
%
\begin{spec}
     eval (derive (Exp e))                          =  {- specification of |derive| above -}

     D (eval (Exp e))                               =  {- def. |eval| -}

     D (exp (eval e))                               =  {- def. |exp| for functions -}

     D (exp . eval e)                               =  {- chain rule -}

     (D exp . eval e) * D (eval e)                  =  {- |D| rule for |exp| -}

     (exp . eval e) * D (eval e)                    =  {- specification of |derive| -}

     (exp . eval e) * (eval (derive e))             =  {- def. of |eval| for |Exp| -}

     (eval (Exp e)) * (eval (derive e))             =  {- def. of |eval| for |:*:| -}

     eval (Exp e  :*:  derive e)
\end{spec}

Therefore, the specification is fulfilled by taking
%
\begin{spec}
derive (Exp e) = Exp e :*: derive e
\end{spec}

Similarly, we obtain
%
\begin{code}
derive     (Const alpha)  =  Const 0
derive     Id             =  Const 1
derive     (e1 :+: e2)    =  derive e1  :+:  derive e2
derive     (e1 :*: e2)    =  (derive e1  :*:  e2)  :+:  (e1  :*:  derive e2)
derive     (Exp e)        =  Exp e :*: derive e
\end{code}

Exercise: complete the |FunExp| type and the |eval| and |derive|
functions.

\subsection{Shallow embeddings}
\label{sec:evalD}

The DSL of expressions, whose syntax is given by the type |FunExp|,
turns out to be almost identical to the DSL defined via type classes
in the first part of this lecture.
%
The correspondence between them is given by the |eval| function.

The difference between the two implementations is that the first one
separates more cleanly from the semantical one.
%
For example, |:+:| \emph{stands for} a function, while |+| \emph{is}
that function.

The second approach is called ``shallow embedding'' or ``almost
abstract syntax''.
%
It can be more economical, since it needs no |eval|.
%
The question is: can we implement |derive| in the shallow embedding?

Note that the reason the shallow embedding is possible is that the
|eval| function is a \emph{fold}: first evaluate the sub-expressions
of |e|, then put the evaluations together without reference to the
sub-expressions.
%
This is sometimes referred to as ``compositionality''.

We check whether the semantics of derivatives is compositional.
%
The evaluation function for derivatives is

\begin{code}
eval'  ::  FunExp -> Double -> Double
eval'  =   eval . derive
\end{code}

For example:
%
\begin{spec}
     eval' (Exp e)                      =  {- def. |eval'|, function composition -}

     eval (derive (Exp e))		=  {- def. |derive| for |Exp| -}

     eval (Exp e :*: derive e)		=  {- def. |eval| for |:*:| -}

     eval (Exp e) * eval (derive e)	=  {- def. |eval| for |Exp| -}

     exp (eval e) * eval (derive e)	=  {- def. |eval'| -}

     exp (eval e) * eval' e             =  {- let |f = eval e|, |f' = eval' e| -}

     exp f * f'
\end{spec}
%
Thus, given only the derivative |f' = eval' e|, it is impossible to
compute |eval' (Exp e)|.
%
(There is no way to implement |eval'Exp :: (REAL -> REAL) -> (REAL ->
REAL)|.)
%
Thus, it is not possible to directly implement |derive| using shallow
embedding; the semantics of derivatives is not compositional.
%
Or rather, \emph{this} semantics is not compositional.
%
It is quite clear that the derivatives cannot be evaluated without, at
the same time, being able to evaluate the functions.
%
So we can try to do both evaluations simultaneously:

\begin{code}
type FD a = (a -> a, a -> a)

evalD ::  FunExp  ->  FD Double
evalD     e       =   (eval e, eval' e)
\end{code}
%
(Note: At this point, you are adviced to look up and solve exercise
\ref{exc:tuplingE1} on the ``tupling transform'' in case you have not
done so already.)

Is |evalD| compositional?

We compute, for example:
%
\begin{spec}
     evalD (Exp e)                           =  {- specification of |evalD| -}

     (eval (Exp e), eval' (Exp e))	     =  {- def. |eval| for |Exp| and reusing the computation above -}

     (exp (eval e), exp (eval e) * eval' e)  =  {- introduce names for subexpressions -}

     let  f   = eval e
          f'  = eval' e
     in (exp f, exp f * f')		     =  {- def. |evalD| -}

     let (f, f') = evalD e
     in (exp f, exp f * f')
\end{spec}

This semantics \emph{is} compositional and the |Exp| case is:
%
\begin{code}
evalDExp ::  FD Double  ->  FD Double
evalDExp     (f, f')  =   (exp f, exp f * f')
\end{code}
%
We can now define a shallow embedding for the computation of
derivatives, using the numerical type classes.

\begin{code}
instance Num a => Num (a -> a, a -> a) where
  (f, f')  +  (g, g')  =  (f  +  g,  f'      +  g'      )
  (f, f')  *  (g, g')  =  (f  *  g,  f' * g  +  f * g'  )
  fromInteger n        =  (fromInteger n, const 0)
\end{code}

Exercise: implement the rest

\subsection{Exercises}

% %include E3.lhs
