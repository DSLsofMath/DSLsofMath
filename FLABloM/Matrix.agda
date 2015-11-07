-- Matrices indexed by shape
module Matrix where

open import Shape

-- Matrix representation
data M (a : Set) : (rows cols : Shape) → Set where
  -- 1x1 matrices
  One
    : a → M a L L

  -- 1xn matrices
  Row
    : ∀ {c₁ c₂} → M a L c₁ → M a L c₂ → M a L (B c₁ c₂)

  -- nx1 matrices
  Col
    : ∀ {r₁ r₂} → M a r₁ L → M a r₂ L → M a (B r₁ r₂) L

  -- nxm matrices
  Q
    : ∀ {r₁ r₂ c₁ c₂} →
      M a r₁ c₁ → M a r₁ c₂ →
      M a r₂ c₁ → M a r₂ c₂ →
      M a (B r₁ r₂) (B c₁ c₂)


-- Matrix addition and multiplication need the corresponding
-- operations on the underlying type
module Operations (T : Set) (_*T_ : T → T → T) (_+T_ : T → T → T) where

  MT = M T

  infixr 60 _*_
  infixr 50 _+_

  _+_ : ∀ {r c} → MT r c → MT r c → MT r c
  One x     + One x₁    = One (x +T x₁)
  Row m m₁  + Row n n₁  = Row (m + n) (m₁ + n₁)
  Col m m₁  + Col n n₁  = Col (m + n) (m₁ + n₁)
  Q m00 m01
    m10 m11 + Q n00 n01
                n10 n11 = Q (m00 + n00) (m01 + n01)
                            (m10 + n10) (m11 + n11)

  _*_ : ∀ {r₁ c₂ x} → MT r₁ x → MT x c₂ → MT r₁ c₂
  One x     * One x₁    = One (x *T x₁)
  One x     * Row n n₁  = Row (One x * n) (One x * n₁)
  Row m m₁  * Col n n₁  = m * n + m₁ * n₁
  Row m m₁  * Q n00 n01
                n10 n11 = Row (m * n00 + m₁ * n10) (m * n01 + m₁ * n11)
  Col m m₁  * One x     = Col (m * One x) (m₁ * One x)
  Col m m₁  * Row n n₁  = Q (m * n)   (m * n₁)
                            (m₁ * n)  (m₁ * n₁)
  Q m00 m01
    m10 m11 * Col n n₁  = Col (m00 * n + m01 * n₁) (m10 * n + m11 * n₁)
  Q m00 m01
    m10 m11 * Q n00 n01
                n10 n11 = Q (m00 * n00 + m01 * n10) (m00 * n01 + m01 * n11)
                            (m10 * n00 + m11 * n10) (m10 * n01 + m11 * n11)

  module examples (t : T) where

    två   = B L L
    tre   = B två L
    tre'  = B L två
    -- 3x3 matrix
    mat : MT tre' tre
    mat = Q (Row (One t) (One t))                (One t)
            (Q (One t) (One t) (One t) (One t))  (Col (One t) (One t))

    -- 3x2 matrix
    mat₁ : MT tre' två
    mat₁ = Q  (One t)                (One t)
              (Col (One t) (One t))  (Col (One t) (One t))
