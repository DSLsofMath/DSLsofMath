\documentclass{article}
%%% Somewhat updated version of polycode.fm from the lhs2TeX dist.
%include dslmagda.fmt
%%% Our own formatting directives
%include dslm.format
\usepackage{natbib}
\usepackage{url}
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
  \TODO{Fill in abstract}
\end{abstract}


\section{Lecture 01}

This lecture is partly based on the paper
\cite{TFPIE15_DSLsofMath_IonescuJansson} from the International
Workshop on Trends in Functional Programming in Education 2015.

%include 01/L01.lhs

\bibliographystyle{abbrvnat}
\bibliography{../comp/ref}
\end{document}
