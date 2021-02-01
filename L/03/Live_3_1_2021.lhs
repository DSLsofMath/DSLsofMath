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


* On the blackboard / other lecture parts:
+ types for some examples, most importantly D
+ limit of a function      [[https://youtu.be/Zky9J949jo4][lecture video part 1]]
+ using lim to define |D|  [[https://youtu.be/0Xzvn5nVbh8][lecture video part 2]]
+ derivative of a function f : Func = REAL -> REAL

* Live coding
DSL for derivatives ={here} a DSL for 1-argument functions (or 1-var. epressions)

Example functions / expressions:
  sq(x) = x^2
  tw(x) = 2*x
  c2(x) = 2
\begin{code}
data SynF -- Syntax datatype for 1-argument function expressions
data SemF -- Semantic type   for 1-argument function expressions
eval :: SynF -> SemF
eval = error "eval: Live coding"

deriv :: SynF -> SynF
deriv = error "deriv: Live coding"
\end{code}
