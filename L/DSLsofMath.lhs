%let lectureNotes = not book

\documentclass[oneside]{book} % use one-side as long as we edit the book (easier to read in pdf reader).
%if book
\usepackage{mathpazo}
%endif
%if book
% let's use the default size to avoid luring ourselves into huge lines.
%else
\usepackage[margin=2cm,a4paper]{geometry}
% \usepackage{a4wide} (the above is even more compact and easier to control)
%endif
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
\usepackage[dvipsnames]{xcolor}
%if book
\usepackage[colorlinks=true,allcolors=blue]{hyperref}
%else
\usepackage{hyperref}
%endif
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8]{inputenc}
\usepackage{newunicodechar}
%include newunicodedefs.tex
\usepackage[type={CC},modifier={by-nc-sa},version={4.0}]{doclicense}
\RequirePackage{amsfonts}
\usepackage[capitalize]{cleveref} % \cref
\usepackage{tikz}
%\usepackage[labelfont=bf]{caption}
\usepackage{caption}
\usepackage{subcaption}
\usepackage{tikz-cd}
\usetikzlibrary{trees,graphs,quotes,fit,shapes,matrix}
\usepackage{lineno}
\usepackage{enumitem}
\usepackage{pdfpages}
\usepackage{tabu}
\newtheorem{theorem}{Theorem}
\newtheorem{lemma}{Lemma}
\newtheorem{proposition}{Proposition}
\newcommand\jp[1]{\todo{JP: #1}}
\newcommand\ci[1]{\todo{CI: #1}}
\newcommand\pj[1]{\todo{PJ: #1}}
\newcommand\extraMaterial{\(\ast\)}
\newcommand\pedantic[1]{\footnote{Pedantic remark: #1}}

\providecommand\mathbbm{\mathbb}


%if submit
\newcommand{\todo}[2][?]{}
%else
\newcommand{\todo}[2][?]{\marginpar{\raggedright\tiny{}TODO: #2}}
%endif

\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt plus 2pt minus 1pt}

\theoremstyle{definition}
\newtheorem{exercise}{Exercise}[chapter]
% Old exercise style (until 2017-11-08): \renewcommand*{\theenumi}{\textbf{E\thesection.\arabic{enumi}}}
\newenvironment{solution}% environment name
{% begin code
  \textbf{Solution:}%
}%
{}% end code
\newenvironment{example}% environment name
{% begin code
  \paragraph{Example}%
}%
{}% end code
\newcommand{\reviseForBook}[1]{} % don't show anything for now
%if book
\newcommand\bookOnly[1]{#1} % show only in the book version (not the lecture notes)
%else
\newcommand\bookOnly[1]{} % show only in the book version (not the lecture notes)
%endif
%if lectureNotes
\newcommand\lnOnly[1]{#1} % show only in the lecture notes (not the book version)
%else
\newcommand\lnOnly[1]{} % show only in the lecture notes (not the book version)
%endif
\newcommand\course{\lnOnly{course}\bookOnly{book}}
\newcommand{\TODO}[1]{\todo{#1}}
\newcommand\refFig{\cref}
\newcommand\refSec{\cref}
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
\newcommand{\fromExam}[1]{\lnOnly{\emph{From exam #1}}}
\newcommand\crefatpage[1]{\cref{#1}, on page \pageref{#1}}

\title{Domain-Specific Languages of Mathematics
%if lectureNotes
: Lecture Notes
%endif lectureNotes
}
\author{Patrik Jansson \and Cezar Ionescu  \and Jean-Philippe Bernardy}
%           {Chalmers University of Technology, Sweden}
%           {\texttt{patrikj@@chalmers.se}}
%           {\texttt{cezar@@chalmers.se}}
\date{WORK IN PROGRESS: DRAFT OF \today}
\begin{document}
\frontmatter
\maketitle
%\begin{abstract}
\paragraph{Abstract}
%if lectureNotes

  These notes aim to cover the lectures and exercises of the recently
  introduced course ``Domain-Specific Languages of Mathematics'' (at
  Chalmers and University of Gothenburg).
%
  The course was developed in response to difficulties faced by
  third-year computer science students in learning and applying
  classical mathematics (mainly real and complex analysis).
%endif
%
  The main idea behind this textbook is to encourage the students to
  approach mathematical domains from a functional programming
  perspective:
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
\pj{Remove abstract or replace by text from reviews}

\vfill

\paragraph{License} \doclicenseThis


\tableofcontents
\mainmatter

%include 00/Intro.lhs
%include 01/W01.lhs
%include 02/W02.lhs
%include 03/W03.lhs
%include 04/W04.lhs
%include 05/W05.lhs
%include 06/W06.lhs
%include 07/W07.lhs
%include 08/W08.lhs
%include 09/W09.lhs
%include End.lhs


\appendix

%if lectureNotes
\chapter{Exam Practice}

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
%endif

%include 01/CSem.lhs

\bibliographystyle{abbrvnat}
\bibliography{ref}
\end{document}
