%-*-Latex-*-
\documentclass[aspectratio=169]{beamer}
\usetheme{Madrid}
% Hide navigation symbols
\setbeamertemplate{navigation symbols}{}
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8]{inputenc}
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
%include ../L/03/tfpie2018slides.format
% the `doubleequals' macro is due to Jeremy Gibbons
\def\doubleequals{\mathrel{\unitlength 0.01em
  \begin{picture}(78,40)
    \put(7,34){\line(1,0){25}} \put(45,34){\line(1,0){25}}
    \put(7,14){\line(1,0){25}} \put(45,14){\line(1,0){25}}
  \end{picture}}}
\usepackage{newunicodechar}
\newunicodechar{⊆}{\ensuremath{\subseteq}} %
\newunicodechar{ℝ}{\ensuremath{\mathbb{R}}} %
\newunicodechar{⁺}{\ensuremath{^+}} %% \newunicodechar{⁺}{^+}

\title[DSLM.Ch3.LimFun]{DSLs of Mathematics: limit of functions}
%\date{Lecture 3.1, 2023-01-31}
\date{2023-08-31}
\author{Patrik Jansson}
\institute[FP, Chalmers]{Functional Programming, Chalmers University of Technology}
\begin{document}
\begin{frame}
\maketitle
\end{frame}
% Introduce course - show example
%% -------------------------------------------------------------------
%\begin{frame}
%\end{frame}
% Dependencies:
% TODO "predicate", FOL = First Order Logic,
% DONE Haskell notation for function application
% DONE Maybe type (data Maybe a = Nothing | Just a)
% DONE lambda expressions: (f = \ x -> e  same as f(x) = e but without the name f)
% DONE operator sections (^2) for squaring, (2*) for doubling


\begin{frame}
\frametitle{Math book quote: The limit of a function}
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

\pause
Scope check:
\begin{itemize}
\item |a|, |f|, |L| bound in the def. of |lim|. \pause
\item Forall binds |epsilon|, exists binds |delta| (and then again in |P epsilon delta|).\pause
\item Anything missing?
\end{itemize}
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

\note{Go through the types of all the symbols:
  --    a     f        L    lim a F L
  lim : X -> (X->Y) -> Y -> FOL
  f : X -> Y
  X `subsetOf` REAL
  Y `subsetOf` REAL
  REAL+ = ``all reals except zero''
  epsilon, delta  : REAL+
  P : REAL+ -> REAL+ -> FOL
  Q : REAL+ -> REAL+ -> X -> FOL
}
\pause
Lesson learned: be careful with scope and binding (of |x| in this case).

\note{The original notation was not quite honest about the use of ``='': \(lim_{x \to a} f(x) = L\) is expressed by by our 3-place predicate lim.}

\end{frame}

%format limProp = "\ensuremath{\mathcolor{red}{\underline{" lim "}}}"
%format limMaybe = "\ensuremath{\mathcolor{blue}{\underline{" lim "}}}"
%format limSloppy  = lim
% %format limSloppy  = "\ensuremath{\mathcolor{blue}{\underrightarrow{" lim "}}}"
\begin{frame}{Variants of |lim| and some properties}
\begin{itemize}
\item Typing: let |X ⊆ ℝ|; |Y ⊆ ℝ|; |a : X|; |L : Y|; and |f : X -> Y|\\
  \begin{tabular}{llcl}
  Version ``limProp''  & |limProp|    & |:| & |X -> (X->Y) -> Y -> Prop| \\
  Version ``limMaybe'' & |limMaybe|   & |:| & |X -> (X->Y) -> Maybe Y|\\
  Version ``limSloppy''& |limSloppy|  & |:| & |X -> (X->Y) -> Y|
  \end{tabular}
  \pause
\item |limProp| can be used as a partial function |limMaybe|, or |limSloppy|:
\begin{spec}
  Forall (a, f, L1, L2) ((limProp a f L1 && limProp a f L2) => L1 == L2)
  Forall (a, f, L) ((limProp a f L) => (limMaybe a f = Just L) && (limSloppy a f = L))
\end{spec}
\pause
\vspace{-0.5cm}
%format oplus f g = f "\oplus" g
%format oplusOp = "(\oplus)"
%format scale c f = c "\triangleleft" f
\item The function |limSloppy a : (X -> Y) -> Y| is linear:
  \begin{code}
    limSloppy a (oplus f g)  = limSloppy a f + limSloppy a g
    limSloppy a (scale c f)  = c * (limSloppy a f)
  \end{code}
\pause
\vspace{-0.5cm}
\item What is |oplusOp : (X->Y)->(X->Y)->(X->Y)|?\pause
  \begin{code}
    oplus f g = \x -> f x + g x
  \end{code}
\end{itemize}

% \pause
% \vspace{1cm}
% {\small [We will now assume limits exist and use |lim| as a function from |a| and |f| to |L|.]}

\end{frame}

%% -------------------------------------------------------------------

\newsavebox{\diagramD}
\savebox{\diagramD}{%
\begin{tikzcd}
         \pgfmatrixnextcell \arrow[dl, "|D f|", swap] \arrow[d, "|psi f|"] |X| \\
  |Y| \pgfmatrixnextcell \arrow[l, "|lim 0|"] |(H -> Y)|
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
  number).
  %If \(f'(x)\) exists, we say that |f| is \textbf{differentiable} at |x|.
\end{quote}


We can write
%format RPlus = "\ensuremath{\mathbb{R}_{\neq 0}}"
%{-"= \mathbb{R} \setminus \{ 0 \}"-}
\savecolumns
\begin{spec}
  D f x  = lim 0 g        where            g  h = frac (f(x+h) - f x) h;                      g ::  H -> Y; type H = RPlus 
\end{spec}
\pause
\vspace{-0.5cm}
\restorecolumns
\begin{spec}
  D f x  = lim 0 (phi x)  where       phi  x  h = frac (f(x+h) - f x) h;{-"\quad"-}  phi  ::  X ->  (H -> Y)
  D f    = lim 0 . phi    where       phi  x  h = frac (f(x+h) - f x) h;{-"\quad"-}  phi  ::  X ->  (H -> Y)
\end{spec}
\pause
\vspace{-0.5cm}
\restorecolumns
\begin{spec}
  D f    = lim 0 . psi f  where  psi  f    x  h = frac (f(x+h) - f x) h;             {-"\usebox{\diagramD}"-}
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

Next: We implement a DSL (embedded in Haskell) to solve this and then
\emph{we implement} a ``symbolic derivative'' function.

\end{frame}
%% -------------------------------------------------------------------
\end{document}