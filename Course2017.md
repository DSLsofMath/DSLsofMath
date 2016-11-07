Domain-Specific Languages of Mathematics
========================================

News
----

2016-09-04: Starting a new course page for the 2017 instance of the
DSLsofMath course. The 2016 instance is available
[as the Year 2016 release](https://github.com/DSLsofMath/DSLsofMath/releases/tag/Year2016).

Aim
---

The course presents classical mathematical topics from a computing
science perspective: giving specifications of the concepts introduced,
paying attention to syntax and types, and ultimately constructing DSLs
of some mathematical areas mentioned below.

Learning outcomes as in the
[course syllabus](https://www.student.chalmers.se/sp/course?course_id=24230).

Course team (TBD)
-----------

- Teacher & Examiner: Patrik Jansson (patrikj AT)
- Teaching assistant: Irene Lobo Valbuena (lobo AT)

Schedule (prel.)
--------

- Lectures Mondays 10-12 (in EB) and Mondays 15-17 (in EF)

- Exercises Wednesdays 13-15 and Thursdays 10-12 (in Ft-4011)

- Guest lectures (TBD)

- Exam: 2017-03-14 afternoon.  Re-exam: 2017-08-22 afternoon.

Assignments (TBD)
-----------

The assignments are to be solved in **teams of three**.

- [Assignment01](Assignments/Assignment01.lhs), due ??

- [Assignment02](Assignments/Assignment02.lhs), [pdf
  file](Assignments/Assignment02.pdf), due ??

Lectures (updates from 2016 are ongoing work)
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
- [Lecture 07, Algebraic Structures and DSLs](Lectures/Lecture09.lhs). Some associated code is also [available](code/).
- [Lecture 08, Polynomials and Power Series](Lectures/Lecture10.lhs).
- [Lecture 09, Power Series and Differential Equations](Lectures/Lecture11.lhs).
- [Lecture 10, The Exponential Function, Taylor Series](Lectures/Lecture12.lhs).
- Lecture 11, TBD: Probability theory or linear algebra (CeIo).
- Lecture 12, TBD: Probability theory or linear algebra (CeIo).
- [Lecture 13, The Laplace Transform](Lectures/Lecture13.lhs).
- [Lecture 14, Basic Concepts of Analysis](Lectures/BasicConcepts.lhs).

Exercises (TBD)
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

Evaluation
----------

(Invitation from Gelena Andreasian.)

The final course evaluation meeting will by on Friday 2016-04-29 at 15.15.

(Mötet äger rum fredag den 29 april kl 15.15-16.15 i mötesrum AoS Origo Koppen. Fysikhuset, ingången från Kemigården. I entréhallen gå in i korridoren till höger. Mötesrummet AoS Origo Koppen ligger på korridorens vänstra sida).


Invited participants:

* Daniel Chai (student)
* Fredrik Johansson (student)
* Wendy Mo (student)
* Patrik Jansson (Examiner)
* Cezar Ionescu (Teacher)
* Irene Lobo Valbuena (TA)
* SND <dns@dtek.se>
* Roger Johansson (PA)
