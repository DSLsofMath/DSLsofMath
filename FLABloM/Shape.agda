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
