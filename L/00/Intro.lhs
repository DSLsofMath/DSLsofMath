\setcounter{section}{-1}
\section{Introduction}

These lecture notes aim to cover the lectures and exercises of the
recently introduced BSc-level course ``Domain Specific Languages of
Mathematics'' (at Chalmers Univeristy of Technology and University of
Gothenburg).
%
The immediate aim of the course is to improve the mathematical
education of computer scientists and the computer science education of
mathematicians.
%
We believe the course can be the starting point for far-reaching
changes, leading to a restructuring of the mathematical training
especially for engineers, but perhaps also for mathematicians
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
Until today, this has been convincingly demonstrated (at least since
the classical textbook of \citet{gries1993logical}) only in the field
of discrete mathematics.
%
In fact, this demonstration has been so successful, that we
increasingly see the discrete mathematics courses being taken over by
computer science departments.
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
Since, moreover, discrete mathematics tends to be immediately useful
to them, this makes the added difficulty of continuous mathematics
even less palatable.
%
As a result, their mathematical competence tends to suffer in areas
such as real and complex analysis, or linear algebra.

This is a serious problem, because this lack of competence tends to
infect the design of the entire curriculum.
%
For example, a course in
``\href{https://www.student.chalmers.se/sp/course?course_id=22612}
{Modeling of sustainable energy systems}'' for Chalmers'
CSE\footnote{CSE = Computer Science \& Engineering = Datateknik = D}
students has to be tailored around this limitation, meaning that the
models, methods, and tools that can be presented need to be
drastically simplified, to the point where contact with mainstream
research becomes impossible.


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
understanding of intuitive or vague notions (see, for example, the
work done at Chalmers in cooperation with the Potsdam Institute for
Climate Impact Research in the context of
\href{http://www.chalmers.se/en/departments/cse/news/Pages/Global-Systems-Science.aspx}{Global
  Systems Science}, \cite{LinckeJanssonetalDSLWC2009,
  ionescujansson2013DTPinSciComp, jaeger13:GSSshort,
  ionescujansson:LIPIcs:2013:3899, DBLP:journals/corr/BottaJICB16,
  Botta2017Avoidability}).


Thus, a course on designing and implementing DSLs can be an important
addition to an engineering curriculum.
%
Our key idea is to combine this with a rich source of domains and
applications: mathematics.
%
Indeed, mathematics offers countless examples of DSLs: the language of
group theory, say, or the language of probability theory, embedded in
that of measure theory.
%
The idea that the various branches of mathematics are in fact DSLs
embedded in the ``general purpose language'' of set theory was (even
if not expressed in these words) the driving idea of the Bourbaki
project, which exerted an enormous influence on present day
mathematics.

The course on \emph{DSLs of Mathematics (DSLM)} allows us to present
classical mathematical topics in a way which builds on the experience
of discrete mathematics: giving specifications of the concepts
introduced, paying attention to syntax and types, and so on.
%
For the mathematics students, used to a more informal style, the
increased formality is justified by the need to implement (fragments
of) the language.
%
We provide a wide range of applications of the DSLs introduced,
so that the new concepts can be seen ``in action'' as soon as
possible.

TODO: update with actual learning outcomes

The course has two major learning outcomes.
%
First, the students should be able to design and implement a DSL in a
new domain.
%
Second, they should be able to handle new mathematical areas using the
computer science perspective.

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
% **TODO: adjust to real areas covered.
% **TODO: add these comparisons
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
However, as the current data structures course (common to the Math and
CSE programmes) shows, math students are usually able to catch up fairly
quickly, and in any case we aim to keep to a restricted subset of
Haskell (no ``advanced'' features are required).

TODO: rewrite when the evaluation results are available

To assess the impact in terms of increased quality of education, we
plan to measure how well the students do in ulterior courses that
require mathematical competence (in the case of engineering students)
or software compentence (in the case of math students).
%
For example, for CS and CSE students we will measure the percentage of
students who, having taken DSLM, pass the third-year courses
\emph{\href{https://www.student.chalmers.se/sp/course?course_id=21865}{Transforms,
    signals and systems}} and
\emph{\href{https://www.student.chalmers.se/sp/course?course_id=21303}{Control
    Theory (Reglerteknik)}}, which are current major stumbling blocks.
%
For math students, we would like to measure their performance in
ulterior scientific computing courses.

Since the course is, at least initially, an elective one, we also have
the possibility of comparing the results with those of a control group
(students who have not taken the course).

The work that lead up to the current course is as follows:

\begin{itemize}
\item 2014: in interaction with our colleagues from the various study
  programmes, we performed an assessment of the current status of
  potential students for the course in terms of their training (what
  prerequisites we can reasonably assume) and future path (what
  mathematical fields they are likely to encounter in later studies),
  and we worked out a course plan (which we submitted in February
  2015, so that the first instance of the course could start in
  January 2016).
%
  We also make a survey of similar courses being offered at other
  universities, but did not find any close matches.

\item 2015: we developed course materials for use within the first
  instance, wrote a paper \citep{TFPIE15_DSLsofMath_IonescuJansson}
  about the course and pressented the pedagogical ideas at several
  events (TODO: perhaps fill in TFPIE'15, DSLDI'15, IFIP WG 2.1 \#73
  in Göteborg).
\item 2016: we ran the first instance of DSLM (partly paid by the
  regular course budget, partly by this project) with Cezar Ionescu as
  main lecturer.
\item 2017: we ran the second instance of DSLM (paid fully by the
  regular course budget), now with Patrik Jansson as main lecturer.
\item 2016 and 2017: we used the feedback from students following the
  standard Chalmers course evaluation in order to improve and further
  develop the course material.
\end{itemize}

Future work includes involving faculty from CSE and mathematics in the
development of other mathematics courses (prel.\ Linear Algebra,
Analysis) with the aim to incorporate these ideas also there.
%
A major concern will be to work together with our colleagues in the
mathematics department in order to distill the essential principles
that can be ``back-ported'' to the other mathematics courses, such as
Mathematical Analysis or Linear Algebra.
%
Ideally, the mathematical areas used in DSLM will become increasingly
challenging, the more the effective aspects of the computer science
perspective are adopted in the first-year mathematics courses.

\subsection{About this course}

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
we develop in the form of domain-specific languages, and we will see
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
The mathematical topic treated have been chosen either because we
expect all students to be familiar with them (for example, limits of
sequences, continuous functions, derivatives) or because they can be
useful in many applications (e.g., Laplace transforms, linear algebra).

\subsection{Who should read these lecture notes?}

TODO: Convert formal prerequisites to ``what is expected of the reader''

The student should have successfully completed

\begin{itemize}
\item a course in discrete mathematics as for example Introductory Discrete Mathematics.
\item 15 hec in mathematics, for example Linear Algebra and Calculus
\item 15 hec in computer science, for example (Introduction to Programming or Programming with Matlab) and Object-oriented Software Development
\item an additional 22.5 hec of any mathematics or computer science courses.
\end{itemize}

Informally: One full time year (60 hec) of university level study
consisting of a mix of mathematics and computer science.


\subsection{Roadmap}

TODO: write about the lecture plan and internal dependencies



\subsection{Notation and code convention}

TODO: fill in more about notation

\subsection{Acknowledgments}

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
