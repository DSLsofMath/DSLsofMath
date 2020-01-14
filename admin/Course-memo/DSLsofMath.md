% Domain-Specific Languages of Mathematics, 2018
% Course-memo for the 2018 instance of a 7.5hec BSc course at Chalmers and GU.
% Patrik Jansson

\url{http://github.com/DSLsofMath/DSLsofMath/}

Course codes: DAT326 / DIT982

## Course team

* Examiner&main lecturer: Patrik Jansson (patrikj@)
* Guest lecturer: Cezar Ionescu (cezar@)
* Teaching assistant: Daniel Schoepe (schoepe@)

## Objectives

The course presents classical mathematical topics from a computing
science perspective: giving specifications of the concepts introduced,
paying attention to syntax and types, and ultimately constructing DSLs
of some mathematical areas mentioned below.

Learning outcomes as in the
[course syllabus](https://www.student.chalmers.se/sp/course?course_id=26170).

* Knowledge and understanding
    * design and implement a DSL (Domain-Specific Language) for a new domain
    * organize areas of mathematics in DSL terms
    * explain main concepts of elementary real and complex analysis, algebra, and linear algebra
* Skills and abilities
    * develop adequate notation for mathematical concepts
    * perform calculational proofs
    * use power series for solving differential equations
    * use Laplace transforms for solving differential equations
* Judgement and approach
    * discuss and compare different software implementations of mathematical concepts

The course is elective for both computer science and mathematics
students at both Chalmers and GU.

## Course material

Lecture notes are freely available online. These notes + references
therein cover the course (but there is no printed course textbook).

## Course setup

* L = Lectures (Tue 13–15 and Thu 13–15 in EB)
    * Introduction: Haskell, complex numbers, syntax, semantics, evaluation, approximation
    * Basic concepts of analysis: sequences, limits, convergence, ...
    * Types and mathematics: logic, quantifiers, proofs and programs, Curry–Howard, ...
        * Type classes, derivatives, differentiation, calculational proofs
    * Domain-Specific Languages and algebraic structures, algebras, homomorphisms
    * Polynomials, series, power series
    * Power series and differential equations, exp, sin, log, Taylor series, ...
    * Linear algebra: vectors, matrices, functions, bases, dynamical systems as matrices and graphs
    * Laplace transform: exp, powers series cont., solving PDEs with Laplace
* E = Weekly exercise sessions (Tue 15–17 and Thu 15–17 in ES52)
    * Half time helping students solve problems in small groups
    * Half time joint problem solving at the whiteboard
* Schedule exceptions:
    * 2018-01-19 E: **Thu $\to$ Fri in ES52**.
    * 2018-01-26 E: **Thu $\to$ Fri in ES52**.
    * 2018-02-09 E: **Thu $\to$ Fri in ES52**.
    * 2018-02-16 L+E: **Thu $\to$ Fri in EA+ES52**.
    * 2018-03-08 Thu E in **ES53** this time only.

## Changes from last year

The main changes for 2018 (based on the [course eval meeting](../../eval/2017-04-28.md)) are

* New course literature (lecture notes covering the full course)
* More material on functional programming in Haskell
* Developed more exercises to solve (primarily easier exercises to start each week with)
* More solving of exercises at the whiteboard
* Schedule changes (alternating L, E, L, E instead of L, L, E, E)
* Weekly hand-ins to encourage students to spend more hours on the course (not part of the formal examination)

One of the student evaluators from 2017 (DaHe) was hired part time to help out with these improvements.

## Examination

There are two compulsory course elements:

* A = Assignments (written + oral examination in groups of three students)
    * two compulsory hand-in assignments (2018-01-30, 2018-02-27)
    * Grading: Pass or fail
* E = Exam (individual written exam at the end of the course)
    * Grading: **Chalmers**: U, 3, 4, 5; **GU**: U, G, VG
    * Date: [2018-03-13 at 14.00](https://www.student.chalmers.se/sp/course?course_id=26170)
    * Aids: One textbook of your choice

To pass the course you need to pass both course elements.
