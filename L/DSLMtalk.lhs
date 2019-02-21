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
  \url{https://github.com/DSLsofMath/}

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

{}\hspace{\fill} \tiny [\url{https://github.com/DSLsofMath/DSLsofMath/blob/master/Course2018.md}]
\end{frame}

\section{Types in Mathematics}

\begin{frame}{Case 1: limits \citep{adams2010calculus}}

\begin{quote}
  We say that \(f(x)\) \textbf{approaches the limit} \(L\) as \(x\)
  \textbf{approaches} \(a\), and we write
\vspace{-0.5cm}

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
Where did |x| come from?
}
\only<2>{
\begin{spec}
lim a f L  =  Forall (epsilon > 0) (Exists (delta > 0) (Forall x (P epsilon delta x)))
  where  P epsilon delta x = (0 < absBar (x - a) < delta) => (x `elem` Dom f  && absBar (f x - L) < epsilon))
\end{spec}
}
\end{frame}

\begin{frame}{Case 2: derivative}

{\small [We now assume limits exist and use |lim| as a function from |a| and |f| to |L|.]}

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
\vspace{-0.5cm}
\restorecolumns
\begin{spec}
  D f x  = lim 0 (phi x)  where       phi  x  h = frac (f(x+h) - f x) h
\end{spec}
\pause
\vspace{-0.5cm}
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
and we can call the three components |t : T| for time, |q : Q| for
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
    Lagrange(L, w) =  {-"\qquad"-} D (D₃ L ∘ expand w) == D₂ L ∘ expand w
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
Domain Specific Languages of Mathematics, BSc level course at Chalmers and GU,
\url{https://github.com/DSLsofMath/}
\end{frame}
\end{document}
