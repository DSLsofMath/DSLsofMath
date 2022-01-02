\chapter{The course ``DSLs of Mathematics''}
\label{sec:DSLMcourse}

From 2016 there has been a BSc-level university course on
``Domain-Specific Languages of Mathematics (DSLM)'' at the Computer Science
and Engineering (CSE) Department, joint between Chalmers University of
Technology and University of Gothenburg, Sweden.
%
The learning outcomes of the course are presented in
Figure~\ref{fig:LearningOutcomes}.
%
\begin{wrapfigure}[26]{R}{0.5\textwidth}
\small
\vspace{-2em}
\begin{itemize}
\item Knowledge and understanding
  \begin{itemize}
  \item design and implement a DSL
    % (Domain-Specific Language)
    for a new domain
  \item organize areas of mathematics in DSL terms
  \item explain main concepts of elementary real and complex analysis,
        algebra, and linear algebra
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
  \caption{Learning outcomes}
  \label{fig:LearningOutcomes}
\end{wrapfigure}

In the first instances, the course is an elective course for the
second year within programmes such as CSE\footnote{CSE = Computer
Science \& Engineering = Datateknik = D}, SE, and Math.
%
The potential students will have all taken first-year mathematics
courses, and the only prerequisite which some of them will not satisfy
will be familiarity with functional programming.
%
However, as some of the current data structures course (common to the Math and
CSE programmes) shows, math students are usually able to catch up fairly
quickly, and in any case we aim to keep to a restricted subset of
Haskell (no ``advanced'' features are required).

The formal course prerequisites say that the student should have successfully completed
\begin{itemize}
\item a course in discrete mathematics as for example Introductory Discrete Mathematics.
\item 15 hec in mathematics, for example Linear Algebra and Calculus
\item 15 hec in computer science, for example (Introduction to Programming or Programming with MATLAB) and Object-oriented Software Development
\item an additional 22.5 hec of any mathematics or computer science courses.
\end{itemize}
%
The unit here is ``higher education credit (hec)'' where 60 hec
corresponds to one full-time year of study.


%note: partial evidence that the course "works"
To assess the impact in terms of increased quality of education, we
planned to measure how well the students do in ulterior courses that
require mathematical competence (in the case of engineering students)
or software competence (in the case of math students).
%
For math students, we would like to measure their performance in
ulterior scientific computing courses, but we have taught too few math
students so far to make good statistics.
%
But for CSE students we have measured the percentage of
students who, having taken DSLM, pass the third-year courses
\emph{\href{https://www.student.chalmers.se/sp/course?course_id=21865}{Transforms,
    signals and systems (TSS)}} and
\emph{\href{https://www.student.chalmers.se/sp/course?course_id=21303}{Control
    Theory (sv: Reglerteknik)}}, which are current major stumbling blocks.
%
We have compared the results with those of a control group (students
who have not taken the course).
%
The evaluation of the student results shows improvements in the pass
rates and grades in later courses.
%
This is very briefly summarised in Table~\ref{tab:res} and more
details are explained by
\citet{DBLP:journals/corr/abs-1908-01572}.

%% -------------------------------------------------------------------
% Subsequent results
\begin{table}[htb]
  \centering
  \begin{tabu}{l*{3}{c}}
                       & PASS  & IN   & OUT  \\
    \hline
    TSS pass rate   & 77\%  & 57\% & 36\% \\
    \rowfont{\scriptsize}
    TSS mean grade  & 4.23  & 4.10 & 3.58 \\
    Control pass rate   & 68\%  & 45\% & 40\% \\
    \rowfont{\scriptsize}
    Control mean grade  & 3.91  & 3.88 & 3.35 \\

  \end{tabu}
\caption{Pass rate and mean grade in third year courses for students
  who took and passed DSLsofMath and those who did not.
  %
  Group sizes: PASS 34, IN 53, OUT 92 (145 in all).}
  \label{tab:res}
\end{table}
%TODO: perhaps add later statistics

The work that leads up to the current \course{} started in 2014 with
an assessment of what prerequisites we can reasonably assume and what
mathematical fields the targeted students are likely to encounter in
later studies.
%
In 2015 we submitted a course plan so that the first instance of the
course could start early 2016.
%
We also surveyed similar courses being offered at other universities,
but did not find any close matches.
%
(``The Haskell road to Logic, Maths and Programming'' by
\citet{doets-haskellroadto-2004} is perhaps the closest, but it is
mainly aimed at discrete mathematics.)

\reviseForBook{What follows probably needs thorough revision}
%
While preparing course materials for use within the first instance we
wrote a paper \citep{DBLP:journals/corr/IonescuJ16} about the
course and presented the pedagogical ideas at several events
(TFPIE'15, DSLDI'15, IFIP WG 2.1 \#73 in Göteborg, LiVe4CS in
Glasgow).
%
In the following years we used the feedback from students following
the standard course evaluation in order to improve and further develop
the course material into complete lecture notes, and now a book.

In the first few years, the enrolment and results of the DSLsofMath
course itself was as follows:

\begin{center}
\begin{tabular}{lrrrrrr}
Year           & '16 & '17 & '18 & '19 & '20 & '21\\\hline
Student count  &  28 &  43 &  39 &  59 &  50 &  67\\
Pass rate (\%) &  68 &  58 &  89 &  73 &  68 &  72\\
\end{tabular}
\end{center}

Note that this also counts students from other programmes (mainly SE
and Math) while Table~\ref{tab:res} only deals with the CSE
programme students.


% \paragraph{Future work} includes involving faculty from CSE and
% mathematics in the development of other mathematics courses with the
% aim to incorporate these ideas also there.
% %
% A major concern will be to work together with our colleagues in the
% mathematics department in order to distill the essential principles
% that can be ``back-ported'' to the other mathematics courses, such as
% Calculus or Linear Algebra.
% %
% Ideally, the mathematical areas used in DSLM will become increasingly
% challenging, the more the effective aspects of the computer science
% perspective are adopted in the first-year mathematics courses.
