\documentclass{article}
\usepackage{a4wide}
\usepackage{amsmath}
\usepackage{mathrsfs}
%\usepackage{textgreek}
%include polycode.fmt
%include ../L/dslm.format
%%% Somewhat updated version of polycode.fmt from the lhs2TeX dist.
% %include dslmagda.fmt
%%% Our own formatting directives
%include dslm.format
\usepackage{natbib}
\usepackage{graphicx}
\usepackage{hyperref}
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8x]{inputenc}
\RequirePackage{ucs}
\RequirePackage{amsfonts}
\usepackage{tikz-cd}

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

\renewcommand*{\theenumi}{\textbf{E\thesection.\arabic{enumi}}}
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
\providecommand{\cpp}{C\kern-0.05em\texttt{+\kern-0.03em+}}

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

\tableofcontents

%include 00/Intro.lhs
\newpage
%include 01/W01.lhs
\newpage
%include 02/W02.lhs
\newpage
%include 03/W03.lhs
\newpage
%include 04/W04.lhs
\newpage
%include 05/W05.lhs
\newpage
%include 06/W06.lhs
\newpage
%include 07/W07.lhs
\newpage
%include 08/W08.lhs
\newpage
%include End.lhs

\bibliographystyle{abbrvnat}
\bibliography{ref}
\end{document}
