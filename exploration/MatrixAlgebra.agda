-- Matrices indexed by shape
module MatrixAlgebra (T : Set) (t : T) (_*T_ : T → T → T) (_+T_ : T → T → T) where

module Shape where
  -- shape of matrix
  data S : Set where
    -- 1
    L : S
    -- s₁ + s₂
    B : (s₁ s₂ : S) → S

  open import Data.Nat

  toNat : S → ℕ
  toNat L = 1
  toNat (B s s₁) = toNat s + toNat s₁

  -- fromNat : ℕ → S
  -- fromNat n = ?

open Shape

-- Matrix representation (and also vectors)
data M (a : Set) : (rows cols : S) → Set where
  -- 1x1 matrices
  <_>
    : a → M a L L

  -- 1xn matrices
  <_,_>
    : ∀ {c₁ c₂} → M a L c₁ → M a L c₂ → M a L (B c₁ c₂)

  -- nx1 matrices
  <_,_>′
    : ∀ {r₁ r₂} → M a r₁ L → M a r₂ L → M a (B r₁ r₂) L

  -- nxm matrices
  <_,_,_,_>
    : ∀ {r₁ r₂ c₁ c₂}
      (x : M a r₁ c₁) (y : M a r₁ c₂)
      (z : M a r₂ c₁) (w : M a r₂ c₂)
      → M a (B r₁ r₂) (B c₁ c₂)

infixr 60 _*_
infixr 50 _+_

_+_ : ∀ {r c} → M T r c → M T r c → M T r c
< x > + < x₁ > = < x +T x₁ >
< m , m₁ > + < n , n₁ > = < m + n , m₁ + n₁ >
< m , m₁ >′ + < n , n₁ >′ = < m + n , m₁ + n₁ >′
< m , m₁ , m₂ , m₃ > + < n , n₁ , n₂ , n₃ > = < m + n , m₁ + n₁ , m₂ + n₂ , m₃ + n₃ >

_*_ : ∀ {r₁ c₂ x} → M T r₁ x → M T x c₂ → M T r₁ c₂
< x > * < x₁ > = < x *T x₁ >
< x > * < n , n₁ > = < < x > * n , < x > * n₁ >

< m , m₁ > * < n ,
               n₁ >′ = m * n + m₁ * n₁
< m , m₁ > * < n , n₁ ,
               n₂ , n₃ > = < m * n + m₁ * n₂ , m * n₁ + m₁ * n₃ >

< m ,
  m₁ >′ * < x > = < m * < x > ,
                    m₁ * < x > >′
< m ,
  m₁ >′ * < n , n₁ > = < m * n , m * n₁ ,
                         m₁ * n , m₁ * n₁ >

< m , m₁ ,
  m₂ , m₃ > * < n ,
                n₁ >′ = < m * n + m₁ * n₁ ,
                          m₂ * n + m₃ * n₁ >′
< m , m₁ ,
  m₂ , m₃ > * < n , n₁ ,
                n₂ , n₃ > = < m * n + m₁ * n₂ , m * n₁ + m₁ * n₃ ,
                              m₂ * n + m₃ * n₂ , m₂ * n₁ + m₃ * n₃ >

-- 3x3 matrix
mat : M T (B L (B L L)) (B (B L L) L)
mat = < < < t > , < t > > , < t > ,
        < < t > , < t > ,
          < t > , < t > > , < < t > ,
                              < t > >′ >

-- 3x2 matrix
mat₁ : M T (B L (B L L)) (B L L)
mat₁ = < < t > , < t > , < < t > , < t > >′ , < < t > , < t > >′ >
