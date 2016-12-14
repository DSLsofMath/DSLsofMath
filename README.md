# DSLsofMath
Domain Specific Languages of Mathematics

[DSLsofMath wiki page at Chalmers](http://wiki.portal.chalmers.se/cse/pmwiki.php/FP/DSLsofMath)

A quality project funded by Chalmers with 260kkr/y for 2014 and 2015 resulting in a new BSc level course from 2016.

**[2017 edition of the course.](Course2017.md)**

**[2016 edition of the course.](Course2016.md)** See also the [Year 2016 release](https://github.com/DSLsofMath/DSLsofMath/releases/tag/Year2016).

## Course schedule (prel.)

* Mo 10-12: lecture in EB
* Mo 15-17: lecture in EF
* We 13-15: exercise class in EDIT2505
* Fr 13-15: exercise class in EDIT2505

Source: TODO(2016-12-14): update link to the 2017 instance

([TimeEdit for the 2016 instance.](https://se.timeedit.net/web/chalmers/db1/public/ri157175X33Z06Q5Z36g0Y10y5016Y37Q09gQY5Q55767.html))

## More about the project
[A paper has been accepted for the proceedings of TFPIE](https://github.com/DSLsofMath/tfpie2015).

The project was also presented at [DSLDI 2015](https://github.com/DSLsofMath/dsldi2015) and
at the [IFIP WG 2.1 meeting #73](http://www.cse.chalmers.se/~patrikj/talks/WG2.1_Goteborg_Jansson_Ionescu_DSLsofMath.pdf).

See [QualityProject/](QualityProject/) for more details.

* [Chalmers project database entry](https://research.chalmers.se/en/project/?id=7038)

# Curriculum placement

* [Chalmers student portal](https://www.student.chalmers.se/sp/course?course_id=24230)
    * D = Datateknik (CSE = Computer Science and Engineering):
        * [D2: "half compulsory" in year 2, study period 3](https://www.student.chalmers.se/sp/programplan?program_id=1308&grade=2&conc_id=-1) (the DSLsofMath course is one of the two elective courses offered to the second year of the BSc)
        * elective in year 3, SP 3
    * IT = Informationsteknik (SE = Software Engineering):
        * [IT3: elective in year 3, SP 3](https://www.student.chalmers.se/sp/programplan?program_id=1312&grade=3&conc_id=-1)
    * TM = Teknisk matematik (Engineering Mathematics)
        * [TM3: elective in year 3, SP 3](https://www.student.chalmers.se/sp/programplan?program_id=1318&grade=3&conc_id=-1)
* University of Gothenburg
    * The course is part of the Computer Science Bachelor's Programme
    * and is also given as a single subject course at the University of Gothenburg.
    * Course plan link: http://kursplaner.gu.se/english/DIT982.pdf

# Course description (short abstract)

Software engineering involves modelling very different domains (e.g., business
processes, typesetting, natural language, etc.) as software systems.  The main
idea of this course is that this kind of modelling is also important when
tackling classical mathematics.  In particular, it is useful to introduce
abstract datatypes to represent mathematical objects, to specify the
mathematical operations performed on these objects, to pay attention to the
ambiguities of mathematical notation and understand when they express
overloading, overriding, or other forms of generic programming.  We shall
emphasise the dividing line between syntax (what mathematical expressions look
like) and semantics (what they mean).  This emphasis leads us to naturally
organise the software abstractions we develop in the form of domain-specific
languages, and we will see how each mathematical theory gives rise to one or
more such languages, and appreciate that many important theorems establish
"translations" between them.

Mathematical objects are immutable, and, as such, functional programming
languages are a very good fit for describing them.  We shall use Haskell as our
main vehicle, but only at a basic level, and we shall introduce the elements of
the language as they are needed.  The mathematical topic treated have been
chosen either because we expect all students to be familiar with them (for
example, limits of sequences, continuous functions, derivatives) or because they
can be useful in many applications (e.g., analytic functions, Laplace
transforms).

# Course prerequisites

## Formally (Chalmers):

The student should have successfully completed

* a course in discrete mathematics as for example Introductory Discrete Mathematics.
* 15 hec in mathematics, for example Linear Algebra and Calculus
* 15 hec in computer science, for example (Introduction to Programming or Programming with Matlab) and Object-oriented Software Development
* an additional 22.5 hec of any mathematics or computer science courses.

## Informally:

One full time year (60 hec) of university level study consisting of a mix of mathematics and computer science.
