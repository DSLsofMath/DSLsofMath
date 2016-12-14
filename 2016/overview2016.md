Annotated learning outcomes
===========================

Learning outcomes:

-   Knowledge and understanding
    -   design and implement a DSL (Domain Specific Language) for a new
        domain
    -   organize areas of mathematics in DSL terms
    -   explain main concepts of elementary real and complex analysis,
        algebra, and linear algebra
-   Skills and abilities
    -   develop adequate notation for mathematical concepts
    -   perform calculational proofs
    -   use power series for solving differential equations
    -   use Laplace transforms for solving differential equations
-   Judgement and approach
    -   discuss and compare different software implementations of
        mathematical concepts

References:

-   Course plan:
    <https://www.student.chalmers.se/sp/course?course_id=24179>
-   Lectures:
    <https://github.com/DSLsofMath/DSLsofMath/tree/master/Lectures>
-   Exam:
    <https://github.com/DSLsofMath/DSLsofMath/raw/master/Exam/2016-03/DSLsofMath_exam.pdf>
-   Assignments:
    <https://github.com/DSLsofMath/DSLsofMath/tree/master/Assignments>
-   Exercises:
    <https://github.com/DSLsofMath/DSLsofMath/tree/master/Exercises>

Relating course parts to learning outcomes
==========================================

Notation: En -- exam question n; Ln -- lecture n; An -- assignment n;

Knowledge and understanding
---------------------------

### design and implement a DSL (Domain Specific Language) for a new domain

-   E1: implement DSL for lattices
-   L1, L2, L3, L4: first order languages and logic
-   L6, L9: Haskell typeclasses and DSLs
-   L7: domain specific languages
-   L8: EDSL for Chebyshev interpolation (guest lecture)

### organize areas of mathematics in DSL terms

-   E2: type a mathematical text
-   A1: a language for finite set theory
-   A2: derivatives, homomorphisms
-   L4: types for logical connectives
-   L5: types of operators, type an equation to make sense of it
    (Lagrange equations)
-   L6, L9: mathematical DSLs with Haskell typeclasses
-   L7: domain specific languages
-   L10: DSL for power series

### explain main concepts of elementary real and complex analysis, algebra, and linear algebra

-   E1: explain "lattice" in terms of a formalisation in Haskell
-   E2: type mathematical text
-   E4: define text formally & simplify using functions
-   L8: explain interpolation in terms of a Haskell library
-   L10: polynomials and power series
-   L12: Taylor series, Exponential function

Skills and abilities
--------------------

### develop adequate notation for mathematical concepts

-   E2, E4: type a mathematical text, write down definition formally
-   A1: a language for finite set theory
-   L1, L2, L3, L4, L5: logic, first order languages, types
-   L7: domain specific languages

### perform calculational proofs

-   E4: prove composition continuous
-   L6: calculate a program (`derive`)
-   L11: calculate eval definition from specification
-   L12: calculate exp laws for complex numbers, etc.
-   L13: calculate Laplace transform rules
-   (Ln: TODO: several other uses of calculation)

### use power series for solving differential equations

-   E3: solve diff equation
-   L11: power series & differential equations

### use Laplace transforms for solving differential equations

-   E3: solve diff equation
-   L13: the Laplace transform

Judgement and approach
----------------------

### discuss and compare different software implementations of mathematical concepts

-   A1: a language for finite set theory (assuming discussions
    during grading)
-   A2: working in groups => discussion
-   L6: shallow embeddings
-   L7: domain specific languages

----------------------------------------------------------------

# Inverse mapping

## Notation for learning outcomes

* KU = Knowledge and understanding
    * KU.DSL = design and implement a DSL (Domain Specific Language) for a new domain
    * KU.Org = organize areas of mathematics in DSL terms
    * KU.Explain = explain main concepts of elementary real and complex analysis, algebra, and linear algebra
* SA = Skills and abilities
    * SA.Notation = develop adequate notation for mathematical concepts
    * SA.Calc =perform calculational proofs
    * SA.Power = use power series for solving differential equations
    * SA.Laplace = use Laplace transforms for solving differential equations
* JA = Judgement and approach = discuss and compare different software implementations of mathematical concepts

## Mapping learning outcomes to course parts

Assignments:

| Learning outcome | Course part | Comment |
| ---------------- | ----------- | ------- |
| KU.Org:      | A1: |  a language for finite set theory                                        |
| SA.Notation: | A1: |  a language for finite set theory                                        |
| JA:          | A1: |  a language for finite set theory (assuming discussions during grading)  |
| KU.Org:      | A2: |  derivatives, homomorphisms                                              |
| JA:          | A2: |  working in groups => discussion                                         |
| SA.Notation  | A2: |  derivatives and homomorphisms                                           |

Exam:

| Learning outcome | Course part | Comment |
| ---------------- | ----------- | ------- |
| KU.DSL:          | E1: |  implement DSL for lattices                                   |
| KU.Explain:      | E1: |  explain "lattice" in terms of a formalisation in Haskell     |
| KU.Org:          | E2: |  type a mathematical text                                     |
| SA.Notation:     | E2: |  type a mathematical text, write down definition formally     |
| KU.Explain:      | E2: |  type mathematical text                                       |
| SA.Power:        | E3: |  solve diff equation                                          |
| SA.Laplace:      | E3: |  solve diff equation using Laplace!                           |
| KU.Explain:      | E4: |  define text formally & simplify using functions              |
| SA.Calc:         | E4: |  prove composition continuous                                 |
| SA.Notation:     | E4: |  type a mathematical text, write down definition formally     |

Lectures:

| Learning outcome | Course part | Comment |
| ---------------- | ----------- | ------- |
| KU.DSL:          | L1:  | first order languages and logic
| SA.Notation:     | L1:  | logic, first order languages, types
| KU.DSL:          | L2:  | first order languages and logic
| SA.Notation:     | L2:  | logic, first order languages, types
| KU.DSL:          | L3:  | first order languages and logic
| SA.Notation:     | L3:  | logic, first order languages, types
| KU.DSL:          | L4:  | first order languages and logic
| SA.Notation:     | L4:  | logic, first order languages, types
| KU.Org:          | L4:  | types for logical connectives
| SA.Notation:     | L5:  | logic, first order languages, types
| KU.Org:          | L5:  | types of operators, type an equation to make sense of it (Lagrange equations)
| KU.DSL:          | L6:  | Haskell typeclasses and DSLs
| SA.Calc:         | L6:  | calculate a program (`derive`)
| KU.Org:          | L6:  | mathematical DSLs with Haskell typeclasses
| JA:              | L6:  | shallow embeddings
| KU.DSL:          | L7:  | domain specific languages
| KU.Org:          | L7:  | domain specific languages
| SA.Notation:     | L7:  | domain specific languages
| JA:              | L7:  | domain specific languages
| KU.DSL:          | L8:  | EDSL for Chebyshev interpolation (guest lecture)
| KU.Explain:      | L8:  | explain interpolation in terms of a Haskell library
| KU.DSL:          | L9:  | Haskell typeclasses and DSLs
| KU.Org:          | L9:  | mathematical DSLs with Haskell typeclasses
| KU.Org:          | L10: | DSL for power series
| KU.Explain:      | L10: | polynomials and power series
| SA.Power:        | L11: | power series & differential equations
| SA.Calc:         | L11: | calculate eval definition from specification
| KU.Explain:      | L12: | Taylor series, Exponential function
| SA.Calc:         | L12: | calculate exp laws for complex numbers, etc.
| SA.Laplace:      | L13: | the Laplace transform
| SA.Calc:         | L13: | calculate Laplace transform rules
