Domain-Specific Languages of Mathematics
========================================

News
----

2016-03-10: The example solutions to the problems of the practice exam
have been updated! [hs](Exam/MockE.hs)

2016-03-09: Example solutions to the problems of the practice exam we
went over today are up! [hs](Exam/MockE.hs)

2016-03-09: Update on a question posed today - a mathematics handbook
would be acceptable as the textbook you bring to the exam.

2016-03-09: Thank you all for the session today. We covered the two
first questions of the practice exam; we will continue with the
remaining two tomorrow during the second half of the exercise session,
after resubmissions are graded.  Example solutions to the first two
questions will be posted this evening.  OBS! : Be sure to read the
lecture notes!!

2016-03-05: Make sure to work through the practice exam before the
exercise session on Wednesday.

2016-03-04: Practice exam available! [pdf](Exam/PracticeExam.pdf)
[lhs](Exam/PracticeExam.lhs).  Solutions will be discussed in the
exercises session on 2016-03-09.

2016-02-19: Assignment 2 has been posted!

2016-02-12: Assignment 2 will be announced either Friday,  February 19 or
Monday, February 22.  It will be due Tuesday, 1st of March, 23:59.  The
grading will take place Thursday, 3rd of March, during the exercises
session, following the same protocol as for Assignment 1.

2016-01-25: The date of the exam has been changed to 2016-03-15, 14:00.

2016-01-25: Due to administrative reasons, we are down to one TA,
Irene.  Thanks Victor for your contributions to the course!

2016-01-22: First assignment has been posted!

Aim
---

The course will present classical mathematical topics from a computing
science perspective: giving specifications of the concepts introduced,
paying attention to syntax and types, and ultimately constructing DSLs
of some mathematical areas mentioned below.

Learning outcomes as in the [course syllabus]
(https://www.student.chalmers.se/sp/course?course_id=24179).

Course team
-----------

- Teacher : Cezar Ionescu (cezar AT)
- Examiner : Patrik Jansson (patrikj AT)
- Teaching assistant: Irene Lobo Valbuena (lobo AT), (until 2016-01-24 also Víctor López Juan)
- Intern : Adam Sandberg Ericsson (saadam AT)

Schedule
--------

- Lectures Mondays 10-12 (in ED) and Fridays 13-15 (in EB)

- Exercises Wednesdays 13-15 and Thursdays 10-12 (in Ft-4011)

- Guest lectures

    + Monday 2016-02-08: [Patrik Jansson](https://github.com/patrikja) on [DSLs](Lectures/07/README.md)

    + Friday 2016-02-12: Björn von Sydow on [An embedded DSL for Chebyshev interpolation](Lectures/Lecture08.pdf)

- Exam: 2016-03-15 afternoon.  Re-exam: 2016-08-23 afternoon.

Assignments
-----------

The assignments are to be solved in **teams of three**.

- [Assignment01](Assignments/Assignment01.lhs), due 2016-02-02 23:59.
  Submission via Fire, at [URL](https://xdat09.ce.chalmers.se/2016/lp3/dslm/)

- [Assignment02](Assignments/Assignment02.lhs), [pdf
  file](Assignments/Assignment02.pdf), due 2016-03-01 23:59.
  Submission via Fire, at [URL](https://xdat09.ce.chalmers.se/2016/lp3/dslm/)

Lectures
--------
*Note*: the summaries are written as a mixture of markdown and lhs.
To compile them, you can use a suitably modified form of this
[Makefile](comp/Makefile) and the [bib](comp/ref.bib) and
[template.tex](comp/template.tex) files.

- [Lecture 01, Introduction](Lectures/Lecture01.lhs).  [Slides](Lectures/slides01.pdf).
- [Lecture 02, Logic and Functional Programming, Part I](Lectures/Lecture02.lhs).
- [Lecture 03, Logic and Functional Programming, Part II](Lectures/Lecture03.lhs).
- [Lecture 04, Proofs and Programs in Mathematics](Lectures/Lecture04.lhs).
- [Lecture 05, Types in Mathematics](Lectures/Lecture05.lhs).
- [Lecture 06, Types in Mathematics, Part II](Lectures/Lecture06.lhs).
- [Lecture 07, Domain Specific Languages: Signal Shape](Lectures/07/README.md).
- [Lecture 08, An embedded DSL for Chebyshev interpolation](Lectures/Lecture08.pdf).
- [Lecture 09, Algebraic Structures and DSLs](Lectures/Lecture09.lhs). Some associated code is also [available](code/).
- [Lecture 10, Polynomials and Power Series](Lectures/Lecture10.lhs).
- [Lecture 11, Power Series and Differential Equations](Lectures/Lecture11.lhs).
- [Lecture 12, The Exponential Function, Taylor Series](Lectures/Lecture12.lhs).
- [Lecture 13, The Laplace Transform](Lectures/Lecture13.lhs).
- [Lecture 14, Basic Concepts of Analysis](Lectures/BasicConcepts.lhs).

Exercises
---------

- [Exercises for 2016-01-20](Exercises/Exercises-2016-01-20.lhs).
  [Solutions](Exercises/FOL.lhs) to selected exercises; you can use
  these for the assignment if you wish.
  [Arithmetic language](Exercises/Arithmetic.lhs) and its intepretation
  into the domain of type 'Integer'.
- [Exercises for 2016-01-21](Exercises/Exercises-2016-01-21.lhs).
- [Exercises for 2016-01-27--28](Exercises/Exercises-2016-01-27--28.lhs). You will need the file [AbstractFOL.lhs](Exercises/AbstractFOL.lhs).
- [Exercises for 2016-02-04](Exercises/Exercises-2016-02-04.lhs).
- [Exercises for 2016-02-10--11](Exercises/Exercises-2016-02-10--11.md).
- [Exercises for 2016-02-17](Exercises/Exercises-2016-02-17.lhs).
- [Exercises for 2016-02-18](Exercises/Exercises-2016-02-18.lhs).
  [Example solutions](Exercises/Ring.hs).
- [Exercises for 2016-02-24 and 25](Exercises/Exercises-2016-02-24--25.lhs).
- The exercises session of 2016-03-09 will be devoted to the solutions
  of the [practice exam](Exam/PracticeExam.pdf).
  Partial example [solutions](Exam/MockE.hs).
- An example of [typing mathematical
  entities](Lectures/TypingMaths.lhs), similar to exam question 4.

Literature
----------

There is no course textbook.  We will use many sources: references
will be provided in the lecture summaries.

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
  [URL](http://spivey.oriel.ox.ac.uk/corner/Programming_Languages)

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

More references will be added in due course.
