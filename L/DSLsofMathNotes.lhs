\documentclass{article}
\usepackage{a4wide}
\usepackage{amsmath}
%include polycode.fmt
%%% Somewhat updated version of polycode.fmt from the lhs2TeX dist.
% %include dslmagda.fmt
%%% Our own formatting directives
%include dslm.format
\usepackage{natbib}
\usepackage{graphicx}
\usepackage{url}
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8x]{inputenc}
\RequirePackage{ucs}
\RequirePackage{amsfonts}

\providecommand\mathbbm{\mathbb}

% TODO: Define more of these ...
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
\newcommand{\todo}[2][?]{\marginpar{\raggedright \tiny TODO: #2}}
%endif

\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt plus 2pt minus 1pt}

\newcommand{\TODO}[1]{\todo{#1}}
\newcommand{\refSec}[1]{Sec. \ref{#1}}
\newcommand{\refSecs}[1]{Secs. \ref{#1}}
\newcommand{\refSecI}[1]{Section \ref{#1}}
\newcommand{\refSecsI}[1]{Sections \ref{#1}}
\newcommand{\refTab}[1]{Tab. \ref{#1}}
\title{Domain Specific Languages of Mathematics: Lecture Notes}
\author{Patrik Jansson \and Cezar Ionescu}
%           {Chalmers Univeristy of Technology, Sweden}
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

%include 01/W01.lhs
%include 02/W02.lhs
%include 03/W03.lhs

\bibliographystyle{abbrvnat}
\bibliography{ref}
\end{document}
