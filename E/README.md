# DSLsofMath exercises

## Week 1:

1. Read the [lecture notes](../L/01/L01.lhs) and complete the
   definition of the instance for |Num| for the datatype
   |ComplexP|. Also add a constructor for variables to enable writing
   expressions like |(Var "z") :*: toComplex 1|.

2. Read the next few pages of Appendix I (in [Adams & Essex]) defining
   the polar view of Complex Numbers and try to implement complex
   numbers again, this time based on magnitude and phase for the
   semantics.

3. Implement a simplifier |simp :: ComplexP r -> ComplexP r| that
   handles a few cases like |0 * x = 0|, |1 * x = x|, |(a + b) * c =
   a * c + b * c|, ...  What class context do you need to add to the
   type of simp?

References:

* Adams & Essex (2010): Calculus: a complete course, 7th
  edition. Pearson Canada. "Appendix I. Definition of Complex
  Numbers"

## Week 2:

TBD

Preparation: Read the [TFPIE'15 paper](https://arxiv.org/abs/1611.09475)
