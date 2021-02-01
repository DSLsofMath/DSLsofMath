%-*-Latex-*-
\documentclass[aspectratio=1610]{beamer}
\usetheme{Madrid}
% Hide navigation symbols
\setbeamertemplate{navigation symbols}{}
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8x]{inputenc}
\usepackage{xcolor}
\usepackage{tabu}
\usepackage{hyperref}
\hypersetup{pdfpagemode={FullScreen}}
\RequirePackage{ucs}
\RequirePackage{amsfonts}
\usepackage{tikz}
\usepackage{tikz-cd}
\usetikzlibrary{trees,graphs,quotes}
%include dslmagda.fmt
%include tfpie2018slides.format
% the `doubleequals' macro is due to Jeremy Gibbons
\def\doubleequals{\mathrel{\unitlength 0.01em
  \begin{picture}(78,40)
    \put(7,34){\line(1,0){25}} \put(45,34){\line(1,0){25}}
    \put(7,14){\line(1,0){25}} \put(45,14){\line(1,0){25}}
  \end{picture}}}

\title[DSLM.Ch3.LimFun]{DSLs of Mathematics: limit of functions}
\date{Lecture 3.1, 2021-02-02}
\author{Patrik Jansson}
\institute[FP, Chalmers]{Functional Programming, Chalmers University of Technology}
\begin{document}
\begin{frame}
\maketitle
\end{frame}
% Introduce course - show example

%% -------------------------------------------------------------------

\begin{frame}
% TODO: update title & contents!
\frametitle{Course goal and focus}

\begin{block}{Goal}
  Encourage students to approach mathematical domains \\
  from a functional programming perspective.
\end{block}

\begin{block}{Course focus}
\begin{itemize}
\item Make functions and types explicit

\item Explicit distinction between syntax and semantics

\item Types as carriers of semantic information

\item Organize the types and functions in DSLs

\item [Now] Make variable binding and scope explicit
\end{itemize}
\end{block}

Lecture notes and more available at:
  \url{https://github.com/DSLsofMath/DSLsofMath}
\end{frame}



%% -------------------------------------------------------------------

\begin{frame}
\frametitle{Example: The limit of a function}
\begin{quote}
  We say that \(f(x)\) \textbf{approaches the limit} \(L\) as \(x\)
  \textbf{approaches} \(a\), and we write
%
  \[\lim_{x\to a} f(x) = L,\]
%
  if the following condition is satisfied:\\
  for every number \(\varepsilon > 0\) there exists a number
  \(\delta > 0\), possibly depending on \(\varepsilon\), such that if
  $0 < \lvert x - a\rvert < \delta$, then \(x\) belongs to the domain of \(f\)
  and
  \[
    \lvert f(x) - L\rvert  < \varepsilon
  \]
\end{quote}
\begin{flushright}
  - Adams \& Essex, Calculus - A Complete Course
\end{flushright}
\end{frame}
%% -------------------------------------------------------------------

\begin{frame}
  \frametitle{Limit of a function -- continued}
\begin{quote}
%
  \[\lim_{x\to a} f(x) = L,\]
%
  if \[
    \forall \varepsilon > 0\]
    \[\exists \delta > 0\]
  such that
  if \[0 < \lvert x - a\rvert < \delta,\] then
  \[
    x\in Dom\, f \wedge \lvert f(x) - L\rvert  < \varepsilon
  \]
\end{quote}
\end{frame}

%% -------------------------------------------------------------------

\begin{frame}
  \frametitle{Limit of a function -- continued}
First attempt at translation:
\begin{spec}
lim a f L  =  Forall (epsilon > 0) (Exists (delta > 0) (P epsilon delta))

  where  P epsilon delta =  (0 < absBar (x - a) < delta) =>
                            (x `elem` Dom f  && absBar (f x - L) < epsilon)
\end{spec}
\end{frame}

%% -------------------------------------------------------------------

\begin{frame}
  \frametitle{Limit of a function -- continued}
Finally (after adding a binding for |x|):
\begin{spec}
lim a f L  =  Forall (epsilon > 0) (Exists (delta > 0) (P epsilon delta))

  where  P epsilon delta =    Forall x (Q epsilon delta x)

         Q epsilon delta x =  (0 < absBar (x - a) < delta) =>
                              (x `elem` Dom f  && absBar (f x - L) < epsilon)
\end{spec}

\pause
Lesson learned: be careful with scope and binding (of |x| in this case).

\pause
\vspace{1cm}
{\small [We will now assume limits exist and use |lim| as a function from |a| and |f| to |L|.]}

\end{frame}

%% -------------------------------------------------------------------

\newsavebox{\diagramD}
\savebox{\diagramD}{%
\begin{tikzcd}
         \pgfmatrixnextcell \arrow[dl, "|D f|", swap] \arrow[d, "|psi f|"] |REAL| \\
  |REAL| \pgfmatrixnextcell \arrow[l, "|lim 0|"] |(REAL->REAL)|
\end{tikzcd}%
}
\begin{frame}[fragile]{Example 2: derivative}

%  Lecture 3.1

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
  D f    = lim 0 . psi f  where  psi  f    x  h = frac (f(x+h) - f x) h   {-"\usebox{\diagramD}"-}
\end{spec}
\end{frame}

\begin{frame}{Derivatives, cont.}

Examples:

\begin{spec}
  D : (REAL->REAL) -> (REAL->REAL)

  sq x       =  x^2
  double x   =  2*x
  c2 x       =  2

  sq'   ==  D sq   == D (\x -> x^2) == D ({-"{}"-}^2) == (2*) == double
  sq''  ==  D sq'  == D double == c2 == const 2
\end{spec}

Note: we cannot \emph{implement} |D| (of this type) in Haskell.
%

Given only |f : REAL -> REAL| as a ``black box'' we
cannot compute the actual derivative |f' : REAL -> REAL|.

We need the ``source code'' of |f| to apply rules from calculus.

\end{frame}
%% -------------------------------------------------------------------
\end{document}