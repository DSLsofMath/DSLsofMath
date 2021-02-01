Week & chapter 3: Types in mathematics

Learning outcomes

* Knowledge and understanding
** L3.1: organize areas of mathematics in DSL terms
** L3.1: explain main concepts of elementary real analysis
** L3: design and implement a DSL for derivatives

* Skills and abilities
** L3.1: develop adequate notation for mathematical concepts
** L3.2: perform calculational proofs

* Judgement and approach
** A1: discuss and compare different software implementations of mathematical concepts


On the blackboard:
* types for some examples, most importantly D
* derivative of a function f : Func = REAL -> REAL
* reminder of lim example from last week
* using lim to define |D|


data SynF -- Syntax datatype for 1-argument function expressions
type SemF -- Semantic type   for 1-argument function expressions
\begin{code}
eval :: SynF -> SemF
eval = error "eval: Live coding"

deriv :: SynF -> SynF
deriv = error "deriv: Live coding"

\end{code}
