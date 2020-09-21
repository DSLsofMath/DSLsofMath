\documentclass[twoside]{article}
\usepackage{a4wide}
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
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8x]{inputenc}
\RequirePackage{ucs}
\RequirePackage{amsfonts}
\usepackage{tikz}
\usepackage{tikz-cd}
\usetikzlibrary{trees,graphs,quotes,fit,shapes}
\usepackage{lineno}
\usepackage{enumitem}
\usepackage{pdfpages}
\usepackage{tabu}
\newtheorem{theorem}{Theorem}
\newenvironment{exercise}% environment name
{% begin code
  \textbf{Exercise}\begin{itshape}%
}%
{\end{itshape}}% end code
\newenvironment{solution}% environment name
{% begin code
  \textbf{Solution:}%
}%
{}% end code
\newcommand\jp[1]{\todo{JP: #1}}
\newcommand\pedantic[1]{\footnote{Pedantic remark: #1}}

\providecommand\mathbbm{\mathbb}

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
\newcommand{\reviseForBook}[1]{} % don't show anything for now
\newcommand\bookOnly[1]{#1} % show only in the book version (not the lecture notes)
\newcommand\lnOnly[1]{} % show only in the lecture notes (not the book version)
\newcommand\course{\lnOnly{course}\bookOnly{book}}
\newcommand{\TODO}[1]{\todo{#1}}
\newcommand{\refFig}[1]{Fig.~\ref{#1}}
\newcommand{\refSec}[1]{Sec.~\ref{#1}}
\newcommand{\refSecs}[1]{Secs.~\ref{#1}}
\newcommand{\refSecI}[1]{Section~\ref{#1}}
\newcommand{\refSecsI}[1]{Sections~\ref{#1}}
\newcommand{\refTab}[1]{Tab.~\ref{#1}}
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

\title{Domain-Specific Languages of Mathematics: Lecture Notes}
\author{Patrik Jansson \and Cezar Ionescu}
%           {Chalmers University of Technology, Sweden}
%           {\texttt{patrikj@@chalmers.se}}
%           {\texttt{cezar@@chalmers.se}}
\begin{document}
\maketitle

\begin{abstract}
  These notes aim to cover the lectures and exercises of the recently
  introduced course ``Domain-Specific Languages of Mathematics'' (at
  Chalmers and University of Gothenburg).
%
  The course was developed in response to difficulties faced by
  third-year computer science students in learning and applying
  classical mathematics (mainly real and complex analysis).
%
  The main idea is to encourage the students to approach mathematical
  domains from a functional programming perspective:
%
  to identify the main functions and types involved and, when
  necessary, to introduce new abstractions;
%
  to give calculational proofs;
%
  to pay attention to the syntax of the mathematical expressions;
%
  and, finally, to organize the resulting functions and types in
  domain-specific languages.

\end{abstract}

\tableofcontents

\cleardoublepage
%include 00/Intro.lhs
\cleardoublepage
%include 01/W01.lhs
\cleardoublepage
%include 02/W02.lhs

\cleardoublepage
%include 03/W03.lhs
\cleardoublepage
%include 04/W04.lhs
\cleardoublepage
%include 05/W05.lhs
\cleardoublepage
%include 06/W06.lhs
\cleardoublepage
%include 07/W07.lhs
\cleardoublepage
%include 08/W08.lhs
\cleardoublepage
%include 09/W09.lhs
\cleardoublepage
%include End.lhs

\clearpage
\appendix

\newcommand{\includeexam}[1]{\includeexaminner{Exam #1}{app:Exam#1}{../Exam/#1/Exam-#1.pdf}}

\newcommand{\includeexaminner}[3]{%
\section{#1}
\label{#2}
\includegraphics[page=1,trim={2cm 3.5cm 2cm 3.5cm},clip]{#3}
\includepdf[pages=2-3]{#3}
}

\includeexam{2016-Practice}
\includeexam{2016-03}
\includeexam{2016-08}
\includeexam{2017-03}
\includeexam{2017-08}

%include 01/CSem.lhs

\cleardoublepage
\bibliographystyle{abbrvnat}
\bibliography{ref}
\end{document}
