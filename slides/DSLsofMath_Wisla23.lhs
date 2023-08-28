%-*-Latex-*-
\documentclass[aspectratio=1610]{beamer}
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

\title[DSLsofMath Intro]{Domain-Specific Languages of Mathematics}
\date{2023-08-30}
\author{Patrik Jansson}
\institute[FP, Chalmers]{Functional Programming unit, Chalmers University of Technology}
\begin{document}
\begin{frame}
\maketitle

\begin{itemize}
\item Computer scientist, \href{https://www.haskell.org/}{Haskell} hacker, catalyst of research ideas, likes to connect the big picture with formal details, software \& language technology advocate.
\item Have worked on proofs in \href{https://wiki.portal.chalmers.se/agda/pmwiki.php}{Agda}, Climate Impact Research, Parametricity for Dependent Types, Testing, Parsing, polytypic programming, and \textbf{Domain-Specific Languages of Mathematics}

\end{itemize}
\end{frame}
\begin{frame}
\begin{columns}
\begin{column}{0.4\textwidth}
  \includegraphics[height=\textheight]{DSLsofMath_book_front_cover.jpg}
\end{column}
\begin{column}{0.59\textwidth}
  \begin{itemize}
  \item The book (on the left) is course literature for a 7.5hec
    course at Chalmers and UGOT.
  \item The main idea behind the course is to encourage the students
    to approach mathematical domains from a functional programming
    perspective. \pause
  \item Students will learn about the language Haskell; identify the
    main functions and types involved; introduce calculational proofs;
    pay attention to the syntax of mathematical expressions; and,
    finally, to organize the resulting functions and types in
    domain-specific languages. \pause
  \item The minicourse today and tomorrow gives a teaser: some
    functional programming in Haskell and a few examples of lectures
    from the course.
  \end{itemize}
\end{column}
\end{columns}
\end{frame}
\begin{frame}
\frametitle{What is a Domain-Specific Language (DSL)}
A DSL has four components:
\begin{itemize}
\item Surface syntax (mostly ignored in this course): a set of strings defined by a grammar
\item Abstract syntax: usually a recursive Haskell datatype of syntax trees (|AbsSyn|)
\item Semantic type: a type |Sem| of values (meanings) for the syntax
\item Semantics: a function from |eval : AbsSyn -> Sem|
\end{itemize}

Examples:
\begin{itemize}
\item date expressions (``2023-08-30'', ``last wed. in Aug.'', ``today'', \ldots)
\item excel-sheet formula (``SUM(A1:A9)'', ``RIGHT(LEFT(C7,4),2)'', \ldots)
\item first-order logic (|P => Q|, |Forall (epsilon > 0) (Exists (delta > 0) (epsilon > delta))|, \ldots)
\end{itemize}

%    * Syntax: "2011-11-11", "tredje onsdagen i oktober", "nästa lördag"
%    * Semantik: Date, eller kanske Date -> Date
%    * Syntax: 
%    * Semantik: [ [ Cell ] ] -> Value


\end{frame}
\begin{frame}
\frametitle{Mini-course Overview}
\begin{itemize}
\item Haskell --- a short introduction to ``Type-Driven Development (TDD)''
  \begin{itemize}
  \item Typed functional programming
  \item Function types, Polymorphism, function composition
  \item Pair types, TDD, \ldots
  \item Sum types, |Either|, |Maybe|, \ldots
  \end{itemize}
  \pause
\item Text-book examples:
  \begin{itemize}
  \item scoping, limit of function, derivative in terms of limits, typing
  \item Lagrangian: give type and formal predicate from a text-book quote
  \end{itemize}
  \pause
\item Interaction welcome: ask questions at any time and I will try to explain.
\end{itemize}
\end{frame}
% Dependencies:
% DONE "predicate", FOL = First Order Logic,
% DONE Haskell notation for function application
% DONE Maybe type (data Maybe a = Nothing | Just a)
% DONE lambda expressions: (f = \ x -> e  same as f(x) = e but without the name f)
% DONE operator sections (^2) for squaring, (2*) for doubling

\end{document}

\begin{code}
f x = x^2
\end{code}
