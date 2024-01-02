\chapter*{About this \course{}}
\addcontentsline{toc}{chapter}{About this \course{}}
\label{ch:intro}

%\section{Introduction}
\label{sec:intro}

%note: what the book is about
Software engineering involves modelling very different domains (e.g.,
business processes, typesetting, natural language, etc.) as software
systems.
%
The main idea of this book is that this kind of modelling is also
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
%TODO: Is the claim about "translations" true for the book?

%note: motivates use of FP and choice of topics
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

\section{Origins}
%note: historic background
This book started out as lecture notes aimed at covering the lectures
and exercises of the BSc-level course ``Domain-Specific Languages of
Mathematics'' (at Chalmers University of Technology and University of
Gothenburg).
%note: aim
The immediate aim of the \course{} is to improve the mathematical
education of computer scientists and the computer science education of
mathematicians.
%note: future
We believe the \course{} can be the starting point for far-reaching
changes, leading to a restructuring of the mathematical training
of engineers in particular, but perhaps also for mathematicians
themselves.

%note: different style than elsewhere in Ch.0
%note: start of "identify a gap" (to later fill with "the book"/course)
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
Until today, as far as we can judge, this perspective has been
convincingly demonstrated (at least since the classical textbook of
\citet{gries1993logical}) only in the field of discrete mathematics.
%
In fact, this demonstration has been so successful, that we see
discrete mathematics courses being taken over by computer science
departments.
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
%note: end of "identify a gap"

%note: start of "fill the gap"
\index{domain-specific language (DSL)}%
%
We propose that a focus on \emph{domain-specific languages} (DSLs) can
be used to repair this unsatisfactory state of affairs.
%
In computer science, a DSL ``is a computer language specialized to a
particular application domain'' (Wikipedia, 2021-12-27), and building
DSLs is increasingly becoming a standard industry practice.
%
Empirical studies show that DSLs lead to fundamental increases in
productivity, above alternative modelling approaches such as UML
\citep{tolvanen2011industrial}.
%
Moreover, building DSLs also offers the opportunity for
interdisciplinary activity and can assist in reaching a shared
understanding of intuitive or vague notions.
%
This is supported by our experience: an example is the
work done at Chalmers in cooperation with the Potsdam Institute for
Climate Impact Research in the context of
\href{http://www.chalmers.se/en/departments/cse/news/Pages/Global-Systems-Science.aspx}{Global
  Systems Science},
  \cite{DBLP:conf/dsl/LinckeJZI09,%{Generic Libraries in {C++} with Concepts from High-Level Domain Descriptions in {Haskell}},
  DBLP:conf/types/IonescuJ11,%     {Testing versus proving in climate impact research},
  DBLP:conf/ifl/IonescuJ12,%       {Dependently-Typed Programming in Scientific Computing - Examples from Economic Modelling}
  DBLP:journals/jfp/BottaJI17,%    {Contributions to a computational theory of policy advice and avoidability},
  DBLP:journals/corr/BottaJICB16,% {Sequential decision problems, dependent types and generic solutions},
  esd-9-525-2018,%                 {The impact of uncertainty on optimal emission policies}
  Botta2023MatterMost%             {Responsibility Under Uncertainty: Which Climate Decisions Matter Most?}
}.
% Perhaps cite
%    DBLP:conf/icfp/BernardyJZSP08 connecting Haskell and C++ {A comparison of {C++} concepts and {Haskell} type classes}
%      DBLP:journals/jfp/BernardyJZS10 {Generic programming with {C++} concepts and {Haskell} type classes - a comparison}
%    DBLP:conf/mpc/DanielssonJ04    {Chasing Bottoms}
%    DBLP:conf/popl/DanielssonHJG06 {Fast and loose reasoning is morally correct}, in relation to partial functions / non-termination, etc.
%    DBLP:journals/corr/BernardyJ16 {Certified Context-Free Parsing: A formalisation of {Valiant}'s Algorithm in {Agda}} in relation to Rings
%    DBLP:journals/jfp/BottaBJT21   {Extensional equality preservation and verified generic programming}

Thus, a course on designing and implementing DSLs can be an important
addition to an engineering curriculum.
%
Our key idea is to apply the DSL approach to a rich source of domains and
applications: mathematics.
%note: short ToC (Table of Contents)
Indeed, mathematics offers countless examples of DSLs: in this book we cover
types and complex arithmetics (\cref{sec:DSLComplex}), %Ch1
sets and logic             (\cref{sec:logic}),      %Ch2
functions and derivatives  (\cref{sec:types}),      %Ch3
algebras and morphisms     (\cref{sec:CompSem}),    %Ch4
power series               (\cref{sec:poly}),       %Ch5
differential equations     (\cref{sec:deriv}),      %Ch6
linear algebra             (\cref{sec:LinAlg}),     %Ch7
Laplace transforms         (\cref{sec:Laplace}),    %Ch8
probability theory         (\cref{ch:probability-theory}). %Ch9
%
The idea that the various branches of mathematics are in fact DSLs
embedded in the ``general purpose language'' of set theory was (even
if not expressed in these words) the driving idea of the Bourbaki
project\footnote{The Bourbaki group is the pseudonym of a group of
mathematicians publishing a series of textbooks in modern pure
mathematics, starting in the 1930:s. See
\href{https://en.wikipedia.org/wiki/Nicolas_Bourbaki}{Wikipedia, 2021-12-27}.}
which exerted an enormous influence on present day mathematics.

Hence, the topic of this \course{} is \emph{DSLs of Mathematics}.
%
It presents classical mathematical topics in a way which builds on the
experience of discrete mathematics: giving specifications of the
concepts introduced, paying attention to syntax and types, and so on.
%
For the mathematics students, the style of this \course{} will be more
formal than usual, at least from a linguistic perspective.
%
The increased formality is justified by the need to implement
(parts of) the languages.
%
We provide a wide range of applications of the DSLs introduced, so
that the new concepts can be seen ``in action'' as soon as possible.
%TODO: check how "in action" is actually followed up / implemented
For the computer science students, one aspect is to bring the
``computer-aided learning'' present in feedback from the compiler from
programming to also help in mathematics.

In our view a course based on this textbook should have two major
learning outcomes.
%
First, the students should be able to design and implement a DSL in a
new domain.
%
Second, they should be able to handle new mathematical areas using the
computer science perspective.
%
(For the detailed learning outcomes, see
\cref{fig:LearningOutcomes} in Appendix \ref{sec:DSLMcourse}.)

%*TODO: Check if this form is actually carried out (perhaps update)
To achieve these objectives, the \course{} consists of a sequence of case
studies in which a mathematical area is first presented, followed by a
careful analysis that reveals the domain elements needed to build a
language for that domain.
%
The DSL is first used informally, in order to ensure that it is
sufficient to account for intended applications (for example, solving
equations, or specifying a certain kind of mathematical object).
%
It is in this step that the computer science perspective proves
valuable for improving the reader's understanding of the mathematical
area.
%
The DSL is then implemented in Haskell.
%**TODO: add these comparisons, or update the text
%The resulting implementation can be compared with existing ones, such
%as Matlab in the case of linear algebra, or R in the case of
%statistical computations.
%
Finally, limitations of the DSL are assessed and the possibility for
further improvements discussed.
%
More about the course is presented in Appendix~\ref{sec:DSLMcourse}.


\section{Who should read this book?}

The book is recommended for Haskell developers who are learning maths
and would like to use Haskell to create concrete models out of
abstract maths concepts to improve their understanding.

The book explores the connection between mathematical structures and
Type-Driven Development~\citep{citeulike:14291916} of Haskell programs.
%
If you enjoyed ``The Haskell Road to Logic, Maths and Programming''
\citep{doets-haskellroadto-2004}, you might also enjoy this book.

It is also a book for the mathematically interested who wants to
explore functional programming and domain-specific languages.
%
The book helps put into perspective the domains of Mathematics and
Functional Programming and shows how Computer Science and Mathematics
can be usefully studied together.

We expect the reader to have knowledge corresponding to a few first-year
mathematics and computer science courses, preferably including
functional programming.
%
But we will review all mathematics using our methodology, and keep to a restricted subset of Haskell (no ``advanced''
features are required).

Working knowledge of functional programming is helpful, but it should
be possible to pick up quite a bit of Haskell along the way.
%

The book is not primarily a collection of ready-made code-snippets,
% , like the ``Numerical recipes in ...'' series, %JP: obscure reference
but rather uses Haskell as a ``tool for learning''.
%
Even code which cannot actually run can still be useful in the quest
to debug the mathematical understanding of the reader (through
scope-checking and type-checking) when trying to encode different
mathematical concepts in Haskell.

It would be an interesting endeavour to port the code from Haskell to
a language with an even stronger type system, like Agda
\citep{Norell09dependentlytyped}, Idris \citep{citeulike:14291916}, or
Lean \citep{conf/cade/MouraKADR15}.
%
The authors would welcome contributions in this direction.

% \section{Roadmap}
%TODO: write about the book plan and internal dependencies
%\jp{Reading guide as dependency graph - see also ``short ToC'' note above + \ref{fig:LearningOutcomes}}

\section{Notation and code convention}

The \course{} is a collection of literate programs: that is, it
consists of text interspersed with code fragments (in Haskell).
%
The source code of the book (including in particular all the Haskell
code) is available on GitHub in the repository
\url{https://github.com/DSLsofMath/DSLsofMath}.

Our code snippets are typeset using \texttt{lhs2tex} \citep{lhs2TeXGuide},
to hit a compromise between fidelity to the Haskell source and
maximimum readability from the point of view of someone used to
conventional mathematical notation.
%
For example, function composition is typically represented as a circle
in mathematics texts.
%
When typesetting, a suitable circle glyph can be obtained in various
ways, depending on the typesetting system: \verb+&#8728+ in HTML,
\verb+\circ+ in \TeX, or by the \textsc{ring operator} Unicode
codepoint (\verb"U+2218"), which appears ideal for the purpose.
%
This codepoint can also be used in Haskell (recent implementations
allow any sequence of codepoints from the Unicode \textsc{symbol}
class).
%
However, the Haskell Prelude uses instead the infix operator \verb+.+
(period), as a crude ASCII approximation, possibly chosen for its
availability and the ease with which it can be typed.
%
In this book, as a compromise, we use the period in our source code,
but our typesetting tool renders it as a circle glyph.
%
If, when looking at typeset pages, any doubt should remain regarding to
the form of the Haskell source, we urge the reader to consult the
GitHub repository.

%
The reader is encouraged to experiment with the examples to get a
feeling for how they work.
%
But instead of cutting and pasting code from the PDF, please use the
source code in the repository to avoid confusing error messages from
indentation and Unicode symbols.
%
A more radical, but perhaps more instructive alternative would be to
recreate all the Haskell examples from scratch.

Each chapter contains exercises to help the reader practice the
concepts just taught.
%
%if lectureNotes
Most exam questions from the first five exams of the DSLsofMath course
have been included as exercises, so for those of you taking the
course, you can check your progress towards the final examination.
%endif
%
Sometimes the chapter text contains short, inlined questions, like
``Exercise~\ref{exc:fmap}: What does function composition do to a
sequence?''.
%
In such cases there is some more explanation in the exercises section
at the end of the chapter, and the exercise number is a link to the
correct place in the document.

In several places the book contains an indented quote of a definition
or paragraph from a mathematical textbook, followed by detailed
analysis of that quote.
%
The aim is to improve the reader's skills in understanding, modelling,
and implementing mathematical text.

Some more advanced material that can be skipped is marked with a star
(\extraMaterial).

\section{Acknowledgments}

The support from Chalmers Quality Funding 2015 (Dnr C 2014-1712, based
on excellent results in the Swedish Higher Education Authority evaluation) is
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
have received funding from the European Union's Horizon 2020 research
and innovation programme.

Bernardy was supported by the Swedish Research Council, via grant
2014-39, funding the Centre for Linguistic Theory and Studies in
Probability.

Thanks also to Jane Spurr at College Publications for helping in
%if submit
publishing the book \citep{JanssonIonescuBernardyDSLsofMathBook2022}.
%else
publishing the book version
\citep{JanssonIonescuBernardyDSLsofMathBook2022} of this document.
%endif

The authors also wish to thank several anonymous reviewers and
students who have contributed with many suggestions for improvements.

