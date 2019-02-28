# Domain-Specific Languages of Mathematics

**Note: this is the archived 2017 instance of the course**

## News

* 2017-09-19: The lecture notes are slowly but surely being completed and converted from raw notes and blackboard pictures.
* 2017-08-23: Added the [re-exam + proposed solutions](Exam/2017-08/)
* 2017-04-28: Final course evaluation meeting (room Diana, Fysikhuset, 13:15 – 14:00)

## Course team

* Examiner & main lecturer: Patrik Jansson (patrikj AT)
* Lecturer: Cezar Ionescu (cezar AT)
* Teaching assistants: Daniel Schoepe (schoepe AT), Frederik Hanghøj Iversen (hanghj AT student)

## Objectives

The course presents classical mathematical topics from a computing
science perspective: giving specifications of the concepts introduced,
paying attention to syntax and types, and ultimately constructing DSLs
of some mathematical areas mentioned below.

Learning outcomes as in the
[course syllabus](https://www.student.chalmers.se/sp/course?course_id=24230).

## Schedule ([html](https://se.timeedit.net/web/chalmers/db1/public/riqQ7615X80Z06Q0Z45g0Y6Z6Z096Y35Q01vQY5Q5yo6q072oQ.html), [ics](https://se.timeedit.net/web/chalmers/db1/public/ri6Xl0gQ2560YZQQ05Z6974Y0Qy60073l1Q54Q061v555Zq8Z85Yn156.ics))

* Lectures Mondays 10–12 (in EB) and Mondays 15–17 (in EF)

* Exercises Wednesdays 13–15 ([ES53](http://maps.chalmers.se/#aeda8fb5-fb6b-499a-8824-5fadfa5b8371)) and Fridays 13–15 ([ED3582](http://maps.chalmers.se/#315adc60-539c-4f0a-bb4d-3088714bb881))

* Exam: 2017-03-14 afternoon.  Re-exam: 2017-08-22 afternoon.

## Assignments


The assignments are to be solved in **teams of three**.

* [Assignment01](Assignments/Assignment01.lhs), due 2017-01-31

* [Assignment02](Assignments/Assignment02.lhs),
  [pdf file](Assignments/Assignment02.pdf), due 2017-02-28

## Lectures

The "source code" for the lecture notes are in subdirectories of
L/: [L/01/](L/01), [L/02/](L/02/), etc.

I post a PDF snapshot at least once a week
in [L/snapshots/](L/snapshots/) – just pick the most recent one.

Updates from 2016 are ongoing work – the full collection of 2016
lectures are linked from the [2016 homepage](2016/Course2016.md).

## Exercises

See the [E/ subdirectory](E/README.md)

For the impatient: look at the [2016 homepage](2016/Course2016.md).

## Literature

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

More references will be added in due course.

## Evaluation

DSLsofMath course evaluation student representatives 2017:

| Email            | Name                   |
| ---------------- | ----------------------	|
| andreahe@student |  Andreas Henriksson	|
| heurlin@student  |  Daniel Heurlin		|
| molzer@student   |  Simon Mölzer			|
| vioskar@student  |  Oskar Vigren			|
| ariv@student     |  Ari von Nordenskjöld  |

## History

"Old news"

* 2017-01-24: [DSLsofMath google group](https://groups.google.com/forum/#!forum/dslsofmath) for discussions created.
* 2017-01-17: Schedule change: the exercise sessions will be in ED2480 on Wednesdays and in ED3582 on Fridays.
* 2017-01-15: [DSLsofMath course memo 2017](Course-memo/DSLsofMath.md)
* 2016-09-04: Starting a new course page for the 2017 instance of the
  DSLsofMath course. The 2016 instance is available
  [as the Year 2016 release](https://github.com/DSLsofMath/DSLsofMath/releases/tag/Year2016).
