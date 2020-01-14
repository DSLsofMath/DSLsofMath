# Lecture 7: (Embedded) Domain-Specific Languages

[Lecture slides](https://rawgit.com/DSLsofMath/DSLsofMath/master/Lectures/07/slides.html)

## Learning outcomes for lecture 7

This lecture we will focus on the following subset of the full
DSLsofMath course learning outcomes:
* design and implement a DSL for a new domain
* organize areas of mathematics in DSL terms
* develop adequate notation for mathematical concepts
* discuss and compare different software implementations of mathematical concepts

## Content links

[Bare Signal example](ex1/BareSignalExample.lhs)

[Derivative example](ex1/Derivative.lhs)

[Shape example](src/Example.hs)

```shell
PS1='\u:\W\$ '
stack ghci SignalShape:exe:ex1
stack ghci SignalShape:exe:ex2
stack ghci SignalShape:exe:example
test 0
```


## Summary

Useful functional building blocks: "functions from" and "functions to"

```Haskell
type Signal a = Time -> a
type Set e    = e -> Bool
```

Embedded Domain-Specific Languages:

* Different kinds of operations
    * constructor functions, combinators, run functions
* Implementation styles
    * Shallow: representation given by the semantics
    * Deep: representation given by the syntax
* Remember
    * Compositionality
    * Abstraction
