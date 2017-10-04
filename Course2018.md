# Domain-Specific Languages of Mathematics

Homepage for the 2018 instance of a 7.5hec BSc course at Chalmers and GU.

Homepage: https://github.com/DSLsofMath/DSLsofMath/

Course codes: DAT326 / DIT982

## News

* 2017-09-28: First draft of the course page for the 2018 instance

## Course team

* Examiner & main lecturer: Patrik Jansson (patrikj AT)
* Lecturer: Cezar Ionescu (cezar AT)
* Teaching assistants: TBD (was Daniel Schoepe (schoepe AT), Frederik Hanghøj Iversen (hanghj AT student))

## Objectives

The course presents classical mathematical topics from a computing
science perspective: giving specifications of the concepts introduced,
paying attention to syntax and types, and ultimately constructing DSLs
of some mathematical areas mentioned below.

Learning outcomes as in the
[course syllabus](https://www.student.chalmers.se/sp/course?course_id=26170).

* Knowledge and understanding
    * design and implement a DSL (Domain Specific Language) for a new domain
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

Lecture notes + references therein cover the course but there is no
printed course textbook.

The main references are listed below.

## Course setup

* Lectures (Prel. Tue. 13-15 in TBD, and Thu 13-15 in TBD)
    * Introduction: Haskell, complex numbers, syntax, semantics, evaluation, approximation
    * Basic concepts of analysis: sequences, limits, convergence, ...
    * Types and mathematics: logic, quantifiers, proofs and programs, Curry-Howard, ...
	* Type classes, derivatives, derivation,
    * Domain Specific Languages and algebraic structures, algebras, homomorphisms
    * Polynomials, series, power series
    * Power series and differential equations, exp, sin, log, Taylor series, ...
    * Laplace transform
    * Linear algebra
* Weekly exercise sessions (Wednesdays 13-15 and Fridays 13-15)
    * Half time helping students solve problems in small groups
    * Half time joint problem solving at the whiteboard

## Changes from last year

The main changes for 2018 (based on the [course eval meeting](eval/2017-04-28.md)) are

* New course literature (complete lecture notes)
* Developed more exercises to solve (primarily easier exercises to start each week with)
* Weekly hand-ins to encourage students to spend more hours on the course
* More solving of exercises at the whiteboard
* Schedule changes (alternating L, E, L, E instead of L, L, E, E)

## Examination

There are two compulsory course elements:

* Assignments (written + oral examination in groups of three students)
    * two compulsory hand-in assignments (2018-01-30, 2018-02-27)
    * Grading: Pass or fail
* Exam (individual written exam at the end of the course)
    * Grading: Chalmers: U, 3, 4, 5; GU: U, G, VG
    * Date: [2018-03-13 at 14.00](https://www.student.chalmers.se/sp/course?course_id=26170)
    * Aids: One textbook of your choice

To pass the course you need to pass both course elements.

## Lectures

The latest PDF snapshot of the full lecture notes can be found in
[L/snapshots](L/snapshots/).

The "source code" for the lecture notes are in subdirectories of
L/: [L/01/](L/01), [L/02/](L/02/), etc.


## Exercises

See the [E/ subdirectory](E/README.md)

## Evaluation

DSLsofMath course evaluation student representatives 2018 (TBD):

| Email            | Name                   |
| ---------------- | ---------------------- |
| TBD		   | 			    |


## References

Some important references:

### Functional programming

- **Thinking Functionally with Haskell**, Richard Bird, Cambridge
  University Press, 2014
  [URL](http://www.cs.ox.ac.uk/publications/books/functional/)
- **Introduction to Functional Programming Using Haskell**, Richard
  Bird, Prentice-Hall, 1998.  A previous (but *very* different)
  version of the above.
- **An Introduction to Functional Programming**, Richard Bird and Phil
  Wadler, Prentice-Hall, 1988. A previous (but *very* different)
  version of *both* of the above.

### DSLs

- **Functional Programming for Domain-Specific Languages**, Jeremy
  Gibbons.  In *Central European Functional Programming School 2015*,
  LNCS 8606, 2015.
  [URL](http://link.springer.com/chapter/10.1007%2F978-3-319-15940-9_1)

  This is currently *the* standard reference to DSLs for the
  functional programmer.

- **Folding Domain-Specific Languages: Deep and Shallow Embeddings**,
  Jeremy Gibbons and Nicolas Wu,
  ICFP 2014. [URL](http://www.cs.ox.ac.uk/publications/publication7584-abstract.html)

  Available at the same link: a highly recommended
  [short version](http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/embedding-short.pdf)
  and the two videos of Jeremy presenting the most important ideas
  of DSLs in a very accessible way.

- **Programming Languages**, Mike Spivey.  Lecture notes of a course
  given at the CS Department in Oxford.  Useful material for
  understanding the design and implementation of embedded DSLs.
  [URL](http://spivey.oriel.ox.ac.uk/corner/Programming_languages)

- **Domain Specific Languages**, Martin Fowler, 2011.
  [URL](http://martinfowler.com/books/dsl.html)

  The view from the object-oriented programming perspective.

### The computer science perspective

- **Communicating Mathematics: Useful Ideas from Computer Science**,
  Charles Wells, *American Mathematical Monthly*, May 1995.  [URL](http://www.cwru.edu/artsci/math/wells/pub/pdf/commath.pdf)

  This article was one of the main triggers of this course.

### Mathematics

- **The Language of First-Order Logic, 3rd Edition**, Jon Barwise and John
  Etchemendy, 1993.  Out of print, but you can get it for one penny
  from Amazon UK.  A vast improvement over its successors (as Tony
  Hoare said about Algol 60).

- **Mathematics: Form and Function**, Saunders Mac Lane, Springer 1986.
  An overview of the relationships between the many mathematical
  domains.  Entertaining, challenging, rewarding.
  [Fulltext from the library](http://chalmers.summon.serialssolutions.com/sv-SE/search?ho=t&q=Mathematics%3A%20Form%20and%20Function)

- **Functional Differential Geometry**, Gerald Jay Sussman and Jack
  Wisdom, 2013, MIT.  A book about using programming as a means of
  understanding differential geometry.  Similar in spirit to the course,
  but more advanced and very different in form.  An earlier version
  appeared as an [AIM report](http://web.mit.edu/wisdom/www/AIM-2005-003.pdf).
