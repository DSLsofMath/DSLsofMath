module Shape where
open import Data.Product
open import Data.Maybe

-- shape of matrix
data Shape : Set where
  -- 1
  L : Shape
  -- s₁ + s₂
  B : (s₁ s₂ : Shape) → Shape

open import Data.Nat

toNat : Shape → ℕ
toNat L         = 1
toNat (B s s₁)  = toNat s + toNat s₁

-- | Divide in two (almost) equal parts
split : ℕ  ->  ℕ × ℕ
split zero       = (zero , zero)
split (suc zero) = (suc zero , zero)
split (suc (suc n)) with split n
... | (n1 , n2)  = (suc n1 , suc n2)


{-# NO_TERMINATION_CHECK #-}
-- Compute a balanced shape
fromNat : ℕ → Maybe Shape
fromNat zero     = nothing
fromNat (suc n)  with split n
... | (n1 , n2) with fromNat n1 | fromNat n2
...   | nothing  | nothing  = nothing
...   | just s1  | nothing  = just s1
...   | nothing  | just s2  = just s2
...   | just s1  | just s2  = just (B s1 s2)

-- TODO: perhaps add an empty shape (but probably in a separate
-- experiment file because many things change in the matrix
-- representation).
