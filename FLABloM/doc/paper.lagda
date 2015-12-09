% -*- latex -*-

%let submit = False
%if submit
\documentclass[times,authoryear]{sigplanconf}
%else
\documentclass[preprint,times]{sigplanconf}
%endif

%%% Standard definitions from the lhs2TeX installation
%include polycode.fmt
%%% Put your own formatting directives in a separate file
%include paper.format

\usepackage{url}
\usepackage{ucs}
\usepackage[utf8x]{inputenc}
\usepackage{autofe}
%if techreport
\usepackage{TRtitlepage}
%endif

%%% Some useful macros
%if submit
\newcommand{\todo}[2][?]{}
%else
\newcommand{\todo}[2][?]{\marginpar{\raggedright \tiny TODO: #2}}
%endif

\newcommand{\TODO}[1]{\todo{#1}}
\newcommand{\refSec}[1]{Sec. \ref{#1}}
\newcommand{\refSecs}[1]{Secs. \ref{#1}}
\newcommand{\refSecI}[1]{Section \ref{#1}}
\newcommand{\refSecsI}[1]{Sections \ref{#1}}
\newcommand{\refTab}[1]{Tab. \ref{#1}}

\toappear{}

% \usepackage[style=abbrvnat]{biblatex}
% %%% Keep references in a separate bib-file
% \addbibresource{paper.bib}



\begin{document}

%if submit
\conferenceinfo{Haskell'12,} {September 13, 2012, Copenhagen, Denmark.}
\CopyrightYear{2012}
\copyrightdata{978-1-4503-1574-6/12/09}
%elif not techreport
\titlebanner{Preprint}
\preprintfooter{Preprint}
%endif

%if techreport
\TRtitlepage
  {The title}             % argument 1 <= the title
  {Tess Ting \\[1em] Test Ing} % argument 2 <= authors
  {}                                     % argument 3 <= report number
%else
\title{Functional linear algebra with block matrices}

\authorinfo{Adam Sandberg Eriksson}
           {Chalmers Univeristy of Technology, Sweden}
           {\texttt{saadam@@chalmers.se}}

\maketitle
%endif


%-------------------------------------------------------------------------------

\begin{abstract}
  The abstract should describe in a short and catch way what the paper
  is about etc.
  \begin{itemize}
  \item block matrices
  \item linear algebra
  \item rings (seminearrings, semirings, \dots)
  \item closure of matrices
  \end{itemize}
\end{abstract}

%%% Some venues require ACM classification categories - here is an example
\category{D.1.1}%
  {Programming Techniques}%
  {Applicative (Functional) Programming}%

\terms
design, languages, verification

\keywords
some, important, concepts, not already, mentioned, in the title

\section{Introduction}
\label{sec:intro}

In \cite{bernardy2015certified} a formulation of matrices was
used to certify the Valiant parsing algorithm. The matrix formulation
used was restricted to matrices of size $2^n \times 2^n$.
%
This work extends the matrix to allow for all sizes of matrices and
applies the techniques to other algorithms that can be described as
semirings or seminearrings with inspiration from \cite{dolan???fun}.
\section{Matrices}

%include ../Shape.lagda

%include ../Matrix.lagda

\section{Structures}

We define a heirarchy of rings as records in Agda. Using algebraic
structures from the Agda standard library a record for seminnearrings
is built and then extended to a record for semirings.

\paragraph{Seminearrings}

The weakest structure used in this work are seminnearrings.


\section{Conclusions and related work}
\label{sec:conc}

There is very often a conclusion section.
%
Not so much in this skeleton!

\paragraph{Acknowledgements.} This research has been partially funded
by the (some project title + granting agency).
%
Somebody helped with something.
%
The reviewers suggested many improvements to the paper.

%------------------------------------------------------------------------------

\end{document}
