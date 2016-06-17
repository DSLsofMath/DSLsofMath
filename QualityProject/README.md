# DSLsofMath course report 2016

## Resources spent

| Person | Hours |
| ------ | -----:|
| Cezar  | 240h  |
| Patrik |  80h  |
| Irene  |  98h  |
| Victor |  18h  |
| Total  | 436h  |

Adam Sandberg Eriksson also helped a bit but his hours are paid by the
DSLsofMath pedagogical project.

## Results

Exam results:

| Grade    | Percentage | # students |
| --------:| ----------:| ----------:|
|  5 (VG)  |        24% |          6 |
|  4 (G)   |        24% |          6 |
|  3 (G)   |        20% |          5 |
|  U (U)   |        32% |          8 |
| Total    |       100% |         25 |

There was one 100pts thesis (impressive!), and there was one with 0pts
(just one line of text). The median is 53pts and the average
is 55.5pts.

Assignments:

| Grade    | Percentage | # students |
| --------:| ----------:| ----------:|
| 5  (VG)  |       36%  |         10 |
| 4  (G)   |       50%  |         14 |
| 3  (G)   |       11%  |          3 |
| U  (U)   |        4%  |          1 |
| Total    |      100%  |         28 |

The assignments will be on a Pass/Fail grade scale from 2017.
[DAT326 course plan](https://www.student.chalmers.se/sp/course?course_id=24230)

## 2016-03-16:

The DSLsofMath course is coming to its end with the final written exam
yesterday (around 28 students). The work during study period 3
(Jan-Mar) has been focused on developing course material in parallell
with the actual teaching. The open course material is collected in
[this repository](https://github.com/DSLsofMath/DSLsofMath) and the
[2016 course instance homepage](../Course2016.md) contains links to
most of it. All in all we (mainly Cezar Ionescu) have produced

* notes from 14 lectures:
  [Intro](../Lectures/Lecture01.lhs),
  [Logic&FP 1](../Lectures/Lecture02.lhs),
  [Logic&FP 2](../Lectures/Lecture03.lhs),
  [Proofs&Programs](../Lectures/Lecture04.lhs),
  [Types 1](../Lectures/Lecture05.lhs),
  [Types 2](../Lectures/Lecture06.lhs),
  [DSLs](../Lectures/07/README.md),
  [Chebyshev interp.](../Lectures/Lecture08.pdf),
  [AlgStruct&DSLs](../Lectures/Lecture09.lhs),
  [Polynomials&Series](../Lectures/Lecture10.lhs),
  [Power Series & DiffEqs](../Lectures/Lecture11.lhs),
  [exp&Taylor](../Lectures/Lecture12.lhs),
  [Laplace](../Lectures/Lecture13.lhs),
  [Basic Analysis](../Lectures/BasicConcepts.lhs).
* seven sets of [weekly exercises](../Exercises/) and partial solutions (including a [practice exam](../Exam/PracticeExam.pdf) with [partial example solutions](../Exam/MockE.hs))
* and two compulsory hand-in assignments [Assignment 1](../Assignments/Assignment01.lhs) and [Assignment 2](../Assignments/Assignment02.lhs).

Upcoming activities include exam correction, course evaluation and
data collection (of earlier course results) to enable some
"measurement" of the results.

We aim to collect and refine the material during the autumn of 2016
with the aim of producing a book about "DSLs of Math" in time for the
next course instance.

----

In parallell with the course development Patrik Jansson is supervising
a group of five BSc students on a related project with the title:
["Matematikens domänspecifika språk (DSLsofMath) för andra
kurser"](https://github.com/DSLsofMath/BScProj). So far there is a
short tutorial for [Complex
Numbers](https://github.com/DSLsofMath/BScProj/blob/master/Tutorial/ComplexNumbers.lhs).

## 2015-11-15:

Time for a new status update for the pedagogical project "DSLsofMath" (funded by Chalmers' education quality money 2014-2015 via the CSE programme).

The resulting BSc-level elective course "Domain Specific Languages of Mathematics" will be "advertised" to prospective maths students on Monday (TM3) and to CSE students on Thursday (D2). Finally, on Friday I will give a "research talk" about some of the ideas underlying the course - see below and at this URL:
  http://www.chalmers.se/insidan/sites/cse/aktuellt/kalendarium/sok-kalendariet/dslsofmath-presenting

The teaching team of the new course consists of
  Cezar Ionescu (lecturer)
  Patrik Jansson (examiner)
  Irene Lobo Valbuena (TA)
  Víctor López Juan (TA)
and in the pedagogical project I'm very happy to also work with a "student research assistant" (amanuens) at 20%:
  Adam Sandberg Eriksson

We have proposed a BSc thesis project aimed at DV, D, IT, TM
  https://github.com/DSLsofMath/DSLsofMath/blob/master/BScProj/DSLsofMath_andra_kurser.md
with the aim to get a good mix of students to work out DSLsofMath-related material for other neighbouring courses.
(I'd also be interested in supervising MSc thesis projects in this direction.)

We have consulted with Eva Fülöp (as an expert on pedagogics and math didactics) about how to work towards a pedagogical publication next autumn - some notes are available here: https://github.com/DSLsofMath/DSLsofMath/tree/master/ped

The funding for the pedagogical project is soon coming to its end, but as we now (for 2016) have a proper course budget we will continue to do work on implementing the project ideas in the course (and perhaps we can inspire related changes also in other courses).

I'm very happy with what we have managed so far, and I'm looking forward to the start of the course in January.
  Patrik

http://www.chalmers.se/insidan/sites/cse/aktuellt/kalendarium/sok-kalendariet/dslsofmath-presenting
DSLsofMath: Presenting Mathematical Analysis Using Functional Programming
2015-11-20 11:00
Abstract:
In this talk (and the accompanying paper), we present the approach underlying a course on Domain-Specific Languages of Mathematics, which is currently being developed at Chalmers in response to difficulties faced by third-year students in learning and applying classical mathematics (mainly real and complex analysis). The main idea is to encourage the students to approach mathematical domains from a functional programming perspective: to identify the main functions and types involved and, when necessary, to introduce new abstractions; to give calculational proofs; to pay attention to the syntax of the mathematical expressions; and, finally, to organize the resulting functions and types in domain-specific languages.

Links:
https://github.com/DSLsofMath
http://www.cse.chalmers.se/~patrikj/papers/Ionescu_Jansson_DSLsofMath_TFPIE_2015_paper_preprint.pdf
http://www.cse.chalmers.se/~patrikj/talks/WG2.1_Goteborg_Jansson_Ionescu_DSLsofMath.pdf
Kategori Föreläsning
Föreläsare Patrik Jansson, ST-division
Tid 2015-11-20 11:00
Sluttid 2015-11-20 12:00
Plats EDIT room, 3364
Campus Johanneberg

----------------------------------------------------------------

## Status update 2015-04-28:

FYI: DSLs of Math (ny kurs 2016, status för det pedagogiska projektet)
to snd, Roger, Ana, Johan, peter, Cezar, Wolfgang, Moa, Bo, Koen, Ulla
(This letter is mainly in Swedish but the course will be in English, with Cezar as lecturer, so please use English for follow-up questions to Cezar and me.)

Igår skickade min PostDoc Cezar och jag in en artikel som beskriver idéerna bakom en ny kurs som vi fått pengar från PA@D (Roger) att utveckla (baserat på en ansökan med stöd från Miroslaw, Roger, Johan och Ana). Kursen är valfri, går i LP3 på kandidatnivå och ägs av TKDAT men är valfri på flera andra program.

Kursplan:
  https://www.student.chalmers.se/sp/course?course_id=24179
  (DAT325 / DIT982)

Wiki-sida om projektet:
  http://wiki.portal.chalmers.se/cse/pmwiki.php/FP/DSLsofMath
  (sidan innehåller också den ursprungliga projektansökan från våren 2014 trots att planerna ändrats lite)

Vi vill gärna få möjlighet att presentera kursen för studenter (D, IT, TM, DV@GU, Ma@GU) inför val av valfria kurser och vi vill gärna delta i något lärarmöte för att presentera kursen för lärarlaget. Det finns också möjlighet att göra exjobb i anknytning till detta projekt.

Mvh,
  Patrik and Cezar

Source: https://github.com/DSLsofMath/tfpie2015

Domain Specific Languages of Mathematics: Presenting Mathematical Analysis using Functional Programming
2015-04-27: Paper submission: "Domain-Specific Languages of Mathematics: Presenting Mathematical Analysis using Functional Programming" to Trends in Functional Programming in Education (TFPIE 2015). If accepted the paper will be submitted to ENTCS (RoMEO green)

Authors: Cezar Ionescu and Patrik Jansson, Chalmers

Abstract

In this paper, we present the approach underlying a course on Domain-Specific Languages of Mathematics, which is currently being developed at Chalmers in response to difficulties faced by third-year students in learning and applying classical mathematics (mainly real and complex analysis). The main idea is to encourage the students to approach mathematical domains from a functional programming perspective: to identify the main functions and types involved and, when necessary, to introduce new abstractions; to give calculational proofs; to pay attention to the syntax of the mathematical expressions; and, finally, to organize the resulting functions and types in domain-specific languages.

Links

http://wiki.portal.chalmers.se/cse/pmwiki.php/FP/DSLsofMath
