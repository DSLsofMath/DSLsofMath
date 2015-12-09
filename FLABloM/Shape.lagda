%if False
\begin{code}
module Shape where
open import Data.Product
open import Data.Maybe

\end{code}
%endif

In this development matrix dimensions are represented not using
natural numbers but using a datatype that follows the
structure of the block matrices more closely: a non-empty binary tree |Shape|.
\begin{code}
data Shape : Set where
  L : Shape
  B : (s₁ s₂ : Shape) → Shape
\end{code}
The leafs of the tree, |L|, represent 1's
and nodes, |B s₁ s₂|, represent the sum of the two subtrees: |s₁ + s₂|

%if False
\begin{code}
open import Data.Nat

toNat : Shape → ℕ
toNat L         = 1
toNat (B s s₁)  = toNat s + toNat s₁

-- Divide in two (almost) equal parts
split : ℕ  ->  ℕ × ℕ
split zero       = (zero , zero)
split (suc zero) = (suc zero , zero)
split (suc (suc n)) with split n
... | (n1 , n2)  = (suc n1 , suc n2)


{-# NO_TERMINATION_CHECK #-}
-- Compute a balanced shape
fromNat : ℕ → Maybe Shape
fromNat zero       = nothing
fromNat (suc zero) = just L
fromNat n with split n
... | (n1 , n2) with fromNat n1 | fromNat n2
...   | nothing  | nothing  = nothing
...   | just s1  | nothing  = just s1
...   | nothing  | just s2  = just s2
...   | just s1  | just s2  = just (B s1 s2)

-- TODO: perhaps add an empty shape (but probably in a separate
-- experiment file because many things change in the matrix
-- representation).
\end{code}
%endif
