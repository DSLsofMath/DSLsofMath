Overview of example proofs from the 2016 instance:

* ../Lectures/BasicConcepts.lhs
    * An1= Limit point
    * An2= The limit of a sequence
    * An3= The limit of a function
    * An4= Continuity
    * An5= Differentiation
* ../Lectures/Lecture02.lhs
    * Nu1= ${\sqrt 2}$ is irrational.
* ../Lectures/Lecture03.lhs
    * Nu2= two irrational numbers a and b such that a^b is rational
* ../Lectures/Lecture04.lhs
    * Nu3= There exists an infinity of prime numbers
    * An4.1= ${a_n} convergent ∧ f : ℝ → ℝ continuous → {f a_n}$ convergent
	    * could be an exercise
* ../Exercises/Exercises-2016-02-04.lhs
    * An2.1 The sequence {a_n} = (0, 1, 0, 1, ...) does not converge.
    * An2.2 The limit of a convergent sequence is unique.

It would be good to find a place for Number Theory proofs like Nu1, Nu2, Nu3.

----------------------------------------------------------------

## Limit point

*Definition* (adapted from @rudin1964principles, page 28): Let `X` be
a subset of ℝ.  A point `p ∈ ℝ` is a limit point of `X` if for every
`ε > 0`, there exists `q ∈ X` such that `q ≠ p` and `|q - p| < ε`.

< Limp : ℝ → 𝒫 ℝ → Prop
< Limp p X = ∀ ε > 0. ∃ q ∈ X - {p}. |q-p| < ε

Notice that q depends on ε. Thus by introducing a function we can move the ∃ out.

< Q = ℝ_{> 0} → X - {p}
< Limp p X = ∃ q : Q. ∀ ε > 0. |q ε - p| < ε

Introduce the "disk function" D.

< D : ℝ → ℝ_{> 0} → 𝒫 ℝ
< D c r = {x | |x - c| < r}

< Limp p X = ∃ q : Q. ∀ ε > 0. q ε ∈ D p ε

Example: limit outside the set

< X = {1/n | n ∈ ℕ_{>0} }

Show that 0 is a limit point. Note that 0 ∉ X.

We want to prove |Limp 0 X|

< q ε = 1/n where n = ceiling (1/ε)

(where the definition of n comes from a calculation showing the property with D is satiafied)

Exercise: prove that 0 is the _only_ limit point of X.

*Proposition*:  If `X` is finite, then it has no limit points.

< ∀ p ∈ ℝ. not (Limp p X)

good excercise in quantifier negation.

< f : (q : Q) → ℝ_{>0}   such that let ε = f q in q ε ∉ D p ε

Note that q ε is in X -

----------------------------------------------------------------

An2= The limit of a sequence

< Lim : (ℕ→ℝ) → ℝ → Prop
< Lim a L = ∀ ε>0. ∃ N ∈ ℕ. ∀ n ≥ N. |a_n - L| < ε

< Lim a L = ∃ N ∈ ℝ_{>0} → ℕ. ∀ ε>0. ∀ n ≥ N ε. |a_n - L| < ε

Invent notation for |∀ n ≥ N ε. |a_n - L| < ε|:

< Img : (A → B) → 𝓟 A → 𝓟 B
< Img f X = {f a | a ∈ X}

< ImN : (ℕ→ ℝ) → ℕ → 𝓟 ℝ
< ImN a N = Img a {n | n ≥ N}

Then |a_n - L| < ε   ≡  a_n ∈ D L ε

<    ∀ n ≥ N ε. a_n ∈ D L ε:
< ≡  ImN a (N ε) ⊆ D L ε

Look - one quantifier eliminated!

< Lim a L = ∃ N ∈ ℝ_{>0} → ℕ. ∀ ε>0. ImN a (N ε) ⊆ D L ε

Example An2.1: alternating 0,1,0,1 does not converge.
  (negating quantifiers)

Example An2.2: The limit is unique - TODO: include the nice calculation in BasicConcepts.lhs

(Perhaps use Limp in some way?)

TODO: Start with limit of sequence, take limit point later (here).

Then limit of a function.

< lim f a L  ≡ …

Rudin (a cleaner definition): with f : X → ℝ and p limit point of X

< lim_{x→p} f(x) = q  ≡ ∀ ε>0. ∃ δ>0. ∀ x ∈ X. |x-p|<δ ⇒ |f x - q| < ε

< lim f p q  ≡ ∀ ε>0. ∃ δ>0. ∀ x ∈ X. |x-p|<δ ⇒ |f x - q| < ε

< lim f p q  ≡ ∀ ε>0. ∃ δ>0. ∀ x ∈ X. x ∈ D p δ ⇒ f x ∈ D q ε


----------------------------------------------------------------


----

Note: check ~/.emacs.d/elpa/haskell-mode-20161026.310/haskell-unicode-input-method.el

describe-char

----------------------------------------------------------------

Week 1: L1+2 Haskell + DSL intro
Week 2: L3+4 DSL + Types in Maths
Week 3: Basic concepts
Week 4-5: Poly, PowerSeries, ...
Week 6(CeIo): ProbTheory or LinAlg
Week 7: Laplace + review
