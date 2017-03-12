1.i.

> class AbMonoid a where
>   z      ::  a
>   cp     ::  a -> a -> a

1.ii.

> data AbMonoidExp v  =  V v
>                     |  Z
>                     |  CP (AbMonoidExp v) (AbMonoidExp v)

> instance AbMonoid (AbMonoidExp v) where
>   z           =   Z
>   cp          =   CP

1.iii

> instance AbMonoid Integer where
>   z           =   0
>   cp          =   (+)

Endofunctions under composition form a monoid, but not a commutative one.

1.iv

> eval :: AbMonoid a => AbMonoidExp v -> (v -> a) -> a
> eval  (V v) f        =  f v
> eval  Z f            =  z
> eval  (CP e1 e2) f   =  cp (eval e1 f) (eval e2 f)

1.v

> evalInteger   :: AbMonoidExp v -> (v -> Integer) -> Integer
> evalInteger e f      =  eval e f

> e1 = Z
> e2 = CP (CP (V "x") (V "y")) Z
> e3 = CP e2 (CP (V "z") e2)

> f "x" = 3
> f "y" = -2
> f "z" = 1

================================================================

2.a

P : 𝓟 Ω -> [0, 1]

∩ : 𝓟 Ω -> 𝓟 Ω -> 𝓟 Ω

/ : [0, 1] -> (0, 1] -> ℝ_{≥ 0}

P (_ | _) : 𝓟 Ω -> 𝓟+ Ω -> [0, 1]  where 𝓟+ Ω = {A | A ⊆ Ω, P A ≠ 0}

2.b

P : 𝓟+ Ω -> 𝓟 Ω -> [0, 1]

The Kolmogorov notation is much easier to type.

================================================================

3.a


3.b
									  -
    f'' t - 5 * f' t + 6 * f t = e^t

 => {  applying ℒ to both sides  }

    ℒ(f'' t - 5 * f' t + 6 * f t) = ℒ (e^t)

 => {  notation: F = ℒ f, and apply both sides to an arbitrary s }

    s^2 * F s - s - 4 - 5 * (s * F s - 1) + 6 * F s = 1 / (s - 1)

 => {  grouping terms }

    F s * (s^2 - 5 * s + 6) = (s^2 - 2*s + 2) / (s - 1)

 => {  s^2 - 5 * s + 6  =  (s - 2) * (s - 3) }

    F s  * (s - 1) * (s - 2) * (s - 3) = s^2 - 2 * s + 2

We want to write F as

    F s = A / (s - 1) + B / (s - 2) + C / (s - 3)

This is implied by the condition that for all s

    s^2 - 2 * s + 2  =  A * (s - 2) * (s - 3)
                     +  B * (s - 1) * (s - 3)
                     +  C * (s - 1) * (s - 2)

Computing the values of the LHS and RHS for s = 1, 2, 3, we obtain:

    A = 1/2   B = -2  C = 5/2

Therefore

    F s = 0.5 / (s - 1) - 2 / (s - 2) + 2.5 / (s - 3)

from which we obtain

    f t = 0.5 e^t - 2 * e^(2*t) + 2.5 * e^(3*t)

================================================================

4.i

∀ ε > 0. ∃ δ > 0. ∀ x ∈ X.  abs(x - c) < δ  ⇒  abs(f x - f c) < ε

4.ii

δ : ℝ_{> 0} -> ℝ_{> 0}

∃ δ : ℝ_{> 0} -> ℝ_{> 0}. ∀ ε > 0.  ∀ x ∈ X.  abs(x - c) < δ ε  ⇒  abs(f x - f c) < ε

4.iii

We are given δ_f and δ_g with above property and we need to construct δ (f+g) such that

∀ ε > 0. ∀ x ∈ X. abs(x - c) < δ_(f+g) ε  ⇒  abs((f+g) x - (f+g) c) < ε

We have

    abs((f+g) x - (f+g) c) < ε
 ⟺  { (f + g) x = f x + g x }
    abs(f x - f c + g x - g c) < ε
 ⟸  { triangle inequality }
    abs(f x - f c) + abs(g x - g c) < ε
 ⟸  { ε/2 + ε/2 = ε }
    abs(f x - f c) < ε / 2    ∧    abs(g x - g c) < ε / 2
 ⟸  { continuity of f and g in c }
    |x - c| < δ_f (ε / 2)     ∧    |x - c| < δ_g (ε / 2)
 ⟸  { min }
    |x - c| < min (δ_f (ε / 2), δ_g (ε / 2))
 ⟺  { Define δ_{f+g} ε = min (δ_f (ε / 2), δ_g (ε / 2)) }
    |x - c| = δ_{f+g} ε
