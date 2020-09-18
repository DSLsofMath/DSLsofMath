\setcounter{section}{-1}
\section{Introduction}
\label{sec:intro}


These lecture notes\reviseForBook{This book stems from ....} aim to cover the lectures and exercises of the
recently introduced BSc-level course ``Domain-Specific Languages of
Mathematics'' (at Chalmers Univeristy of Technology and University of
Gothenburg).
%
The immediate aim of the \course{} is to improve the mathematical
education of computer scientists and the computer science education of
mathematicians.
%
We believe the \course{} can be the starting point for far-reaching
changes, leading to a restructuring of the mathematical training
of engineers in particular, but perhaps also for mathematicians
themselves.

Computer science, viewed as a mathematical discipline, has certain
features that set it apart from mainstream mathematics.
%
It places much more emphasis on syntax, tends to prefer formal proofs
to informal ones, and views logic as a tool rather than (just) as an
object of study.
%
It has long been advocated, both by mathematicians
\citep{wells1995communicating, kraft2004functions} and computer
scientists \citep{gries1995teaching, boute2009decibel}, that the
computer science perspective could be valuable in general mathematical
education.
%
Until today, as far as we can judge, this perspective has been convincingly demonstrated (at least since
the classical textbook of \citet{gries1993logical}) only in the field
of discrete mathematics.
%
In fact, this demonstration has been so successful, that we
increasingly see the discrete mathematics courses being taken over by
computer science departments.\jp{evidence?}
%
This is a quite unsatisfactory state of affairs, for at least two
reasons.

First, any benefits of the computer science perspective remain within
the computer science department and the synergy with the wider
mathematical landscape is lost.
%
The mathematics department also misses the opportunity to see more in
computer science than just a provider of tools for numerical
computations.
%
Considering the increasing dependence of mathematics on software, this
can be a considerable loss.

Second, computer science (and other) students are exposed to two quite
different approaches to teaching mathematics.
%
For many of them, the formal, tool-oriented style of the discrete
mathematics course is easier to follow than the traditional
mathematical style.
%
Moreover, because discrete mathematics tends to be immediately useful
to them, the added difficulty of continuous mathematics
makes it even less palatable.
%
As a result, their mathematical competence tends to suffer in areas
such as real and complex analysis, or linear algebra.

This is a serious problem, because this lack of competence tends to
infect the design of the entire curriculum.
%
\lnOnly{
For example, a course in
``\href{https://www.student.chalmers.se/sp/course?course_id=22612}
{Modeling of sustainable energy systems}'' for Chalmers'
CSE\footnote{CSE = Computer Science \& Engineering = Datateknik = D}
students has to be tailored around this limitation, meaning that the
models, methods, and tools that can be presented need to be
drastically simplified, to the point where contact with mainstream
research becomes impossible.}


We propose that a focus on \emph{domain-specific languages} (DSLs) can
be used to repair this unsatisfactory state of affairs.
%
In computer science, a DSL ``is a computer language specialized to a
particular application domain'' (Wikipedia), and building DSLs is
increasingly becoming a standard industry practice.
%
Empirical studies show that DSLs lead to fundamental increases in
productivity, above alternative modelling approaches such as UML
\citep{tolvanen2011industrial}.
%
Moreover, building DSLs also offers the opportunity for
interdisciplinary activity and can assist in reaching a shared
understanding of intuitive or vague notions. This is supported by our experience: an example is the
work done at Chalmers in cooperation with the Potsdam Institute for
Climate Impact Research in the context of
\href{http://www.chalmers.se/en/departments/cse/news/Pages/Global-Systems-Science.aspx}{Global
  Systems Science}, \cite{LinckeJanssonetalDSLWC2009,
  ionescujansson2013DTPinSciComp, jaeger13:GSSshort,
  ionescujansson:LIPIcs:2013:3899, DBLP:journals/corr/BottaJICB16,
  botta_jansson_ionescu_2017_avoidability}.


Thus, a course on designing and implementing DSLs can be an important
addition to an engineering curriculum.
%
Our key idea is to apply the DSL approach to a rich source of domains and
applications: mathematics.
%
Indeed, mathematics offers countless examples of DSLs: the language of
group theory, say, or the language of probability theory, embedded in
that of measure theory\jp{forward references to chapters}.
%
The idea that the various branches of mathematics are in fact DSLs
embedded in the ``general purpose language'' of set theory was (even
if not expressed in these words) the driving idea of the Bourbaki
project\jp{citations}, which exerted an enormous influence on present day
mathematics.

Hence, the topic of this \course{} is \emph{DSLs of Mathematics (DSLM)}. It presents
classical mathematical topics in a way which builds on the experience
of discrete mathematics: giving specifications of the concepts
introduced, paying attention to syntax and types, and so on.
%
For the mathematics students, the style of this \course{} will be more
formal than usual, as least from a linguistic perspective. The
increased formality is justified by the need to implement (fragments
of) the language.
\jp{There seem to be a paragraph missing about the benefits for the CS student}
%
We provide a wide range of applications of the DSLs introduced,
so that the new concepts can be seen ``in action'' as soon as
possible.

In our view a course based on this textbook should have two major learning outcomes.
%
First, the students should be able to design and implement a DSL in a
new domain.
%
Second, they should be able to handle new mathematical areas using the
computer science perspective.
%
(For the detailed learning outcomes, see Figure~\ref{fig:LearningOutcomes}.)

\begin{wrapfigure}{R}{0.5\textwidth}
\begin{itemize}
\item Knowledge and understanding
  \begin{itemize}
  \item design and implement a DSL (Domain-Specific Language) for a new domain
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
  \caption{Learning outcomes for DSLsofMath}
  \label{fig:LearningOutcomes}
\end{wrapfigure}

To achieve these objectives, the course consists of a sequence of case
studies in which a mathematical area is first presented (for example,
a fragment of linear algebra, probability theory, interval analysis,
or differential equations), followed by a careful analysis that
reveals the domain elements needed to build a language for that
domain.
%
The DSL is first used informally, in order to ensure that it is
sufficient to account for intended applications (for example, solving
equations, or specifying a certain kind of mathematical object).
%
It is in this step that the computer science perspective proves
valuable for improving the students' understanding of the mathematical
area.
%
The DSL is then implemented in Haskell.
%**TODO: adjust to real areas covered.
%**TODO: add these comparisons
The resulting implementation can be compared with existing ones, such
as Matlab in the case of linear algebra, or R in the case of
statistical computations.
%
Finally, limitations of the DSL are assessed and the possibility for
further improvements discussed.

In the first instances, the course is an elective course for the
second year within programmes such as CSE, SE, and Math.
%
The potential students will have all taken first-year mathematics
courses, and the only prerequisite which some of them will not satisfy
will be familiarity with functional programming.
%
However, as some of the current data structures course (common to the Math and
CSE programmes) shows, math students are usually able to catch up fairly
quickly, and in any case we aim to keep to a restricted subset of
Haskell (no ``advanced'' features are required).

To assess the impact in terms of increased quality of education, we
planned to measure how well the students do in ulterior courses that
require mathematical competence (in the case of engineering students)
or software compentence (in the case of math students).
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
\lnOnly{
Since the course is, at least initially, an elective one, we have also
used the possibility to compare the results with those of a control
group (students who have not taken the course).}
%
The evaluation of the student results shows improvements in the pass
rates and grades in later courses.
%
This is very briefly summarised in Table~\ref{tab:res} and more
details are explained by
\citet{TFPIE18_DSLMResults_JanssonEinarsdottirIonescu}.

%% -------------------------------------------------------------------
% Subsequent results
\begin{table}[h]
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
\caption{Pass rate and mean grade in third year courses for students who took and passed DSLsofMath and those who did not. Group sizes: PASS 34, IN 53, OUT 92 (145 in all)}
  \label{tab:res}
\end{table}




The work that leads up to the current \course{} is as follows:

\paragraph{2014:} in interaction with our colleagues from the various
study programmes, we performed an assessment of the current status of
potential students for the course in terms of their training (what
prerequisites we can reasonably assume) and future path (what
mathematical fields they are likely to encounter in later studies),
and we worked out a course plan (which we submitted in February 2015,
so that the first instance of the course could start in January 2016).
%
We also made a survey of similar courses being offered at other
universities, but did not find any close matches.\reviseForBook{What about Doets and van Eijck ``The Haskell road to Logic Math and Programming''}

\reviseForBook{What follows probably needs thorough revision}
\paragraph{2015:} we developed course materials for use within the
first instance, wrote a paper
\citep{TFPIE15_DSLsofMath_IonescuJansson} about the course and
pressented the pedagogical ideas at several events (TFPIE'15,
DSLDI'15, IFIP WG 2.1 \#73 in Göteborg, LiVe4CS in Glasgow).

\paragraph{2016:} we ran the first instance of DSLM (partly paid by
the regular course budget, partly by the pedagogical project) with
Cezar Ionescu as main lecturer.

\paragraph{2017:} we ran the second instance of DSLM (paid fully by
the regular course budget), now with Patrik Jansson as main lecturer.

\paragraph{2016, 2017, and 2018:} we used the feedback from students
following the standard Chalmers course evaluation in order to improve
and further develop the course material.

\paragraph{2018:} we wrote a paper presenting three examples from the
course material, and an evaluation of the student results showing
improvements in the pass rates and grades in later courses.


\paragraph{Future work} includes involving faculty from CSE and
mathematics in the development of other mathematics courses with the
aim to incorporate these ideas also there.
%
A major concern will be to work together with our colleagues in the
mathematics department in order to distill the essential principles
that can be ``back-ported'' to the other mathematics courses, such as
Calculus or Linear Algebra.
%
Ideally, the mathematical areas used in DSLM will become increasingly
challenging, the more the effective aspects of the computer science
perspective are adopted in the first-year mathematics courses.

\subsection{About this course}
\jp{The whole chapter could have this title. It seems that what follows should be folded seamlessly into the rest}

Software engineering involves modelling very different domains (e.g.,
business processes, typesetting, natural language, etc.) as software
systems.
%
The main idea of this course is that this kind of modelling is also
important when tackling classical mathematics.
%
In particular, it is useful to introduce abstract datatypes to
represent mathematical objects, to specify the mathematical operations
performed on these objects, to pay attention to the ambiguities of
mathematical notation and understand when they express overloading,
overriding, or other forms of generic programming.
%
We shall emphasise the dividing line between syntax (what mathematical
expressions look like) and semantics (what they mean).
%
This emphasis leads us to naturally organise the software abstractions
that we develop in the form of domain-specific languages, and we will see
how each mathematical theory gives rise to one or more such languages,
and appreciate that many important theorems establish ``translations''
between them.

Mathematical objects are immutable, and, as such, functional
programming languages are a very good fit for describing them.
%
We shall use Haskell as our main vehicle, but only at a basic level,
and we shall introduce the elements of the language as they are
needed.
%
The mathematical topics treated have been chosen either because we
expect all students to be familiar with them (for example, limits of
sequences, continuous functions, derivatives) or because they can be
useful in many applications (e.g., Laplace transforms, linear algebra).

In the first three years, the enrolment and results of the DSLsofMath
course itself was as follows:
\begin{itemize}
\item 2016: 28 students, pass rate: 68\%
\item 2017: 43 students, pass rate: 58\%
\item 2018: 39 students, pass rate: 89\%
\end{itemize}
Note that this also counts students from other programmes (mainly SE
and Math) while Table~\ref{tab:res} only deals with the CSE
programme students.

\subsection{Who should read this textbook?}

The prerequisites of the underlying course may give a hint about what
is expected of the reader.
\jp{This was stated in different terms above, restructuring needed}
%
But feel free to keep going and fill in missing concepts as you go
along.

The student should have successfully completed
\begin{itemize}
\item a course in discrete mathematics as for example Introductory Discrete Mathematics.
\item 15 hec in mathematics, for example Linear Algebra and Calculus
\item 15 hec in computer science, for example (Introduction to Programming or Programming with Matlab) and Object-oriented Software Development
\item an additional 22.5 hec of any mathematics or computer science courses.
\end{itemize}

Informally: One full time year (60 hec) of university level study
consisting of a mix of mathematics and computer science.

Working knowledge of functional programming is helpful, but it should
be possible to pick up quite a bit of Haskell along the way.


% \subsection{Roadmap}

%TODO: write about the lecture plan and internal dependencies



\subsection{Notation and code convention}

Each chapter ends with exercises to help the reader practice the
concepts just taught.
%
Most exam questions from the first five exams of the DSLsofMath course
have been included as exercises, so for those of you taking the
course, you can check your progress towards the final examination.
%
Sometimes the chapter text contains short, inlined questions, like
``Exercice~\ref{exc:fmap}: what does function composition do to a sequence?''.
%
In such cases there is some more explanation in the exercises section
at the end of the chapter.

In several places the book contains an indented quote of a definition
or paragraph from a mathematical textbook, followed by detailed
analysis of that quote.
%
The aim is to improve the reader's skills in understanding, modelling,
and implementing mathematical text.


%TODO: fill in more about notation

\subsection*{Acknowledgments}

The support from Chalmers Quality Funding 2015 (Dnr C 2014-1712, based
on Swedish Higher Education Authority evaluation results) is
gratefully acknowledged.
%
Thanks also to Roger Johansson (as Head of Programme in CSE) and Peter
Ljunglöf (as Vice Head of the CSE Department for BSc and MSc
education) who provided continued financial support when the national
political winds changed.

Thanks to Daniel Heurlin who provided many helpful comments during his
work as a student research assistant in 2017.

This work was partially supported by the projects GRACeFUL (grant
agreement No 640954) and CoeGSS (grant agreement No 676547), which
have received funding from the European Union’s Horizon 2020 research
and innovation programme.
