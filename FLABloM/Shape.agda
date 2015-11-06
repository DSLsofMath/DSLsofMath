module Shape where

-- shape of matrix
data Shape : Set where
  -- 1
  L : Shape
  -- s₁ + s₂
  B : (s₁ s₂ : Shape) → Shape

open import Data.Nat

toNat : Shape → ℕ
toNat L = 1
toNat (B s s₁) = toNat s + toNat s₁

-- fromNat : ℕ → S
-- fromNat n = ?
