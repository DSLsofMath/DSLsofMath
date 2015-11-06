-- Matrices indexed by shape
module SquareMatrix where

open import Shape
open import Matrix

-- Square matrices have the same shape in both dimensions
Sq : Set → Shape → Set
Sq a shape = M a shape shape

open import SemiNearRingRecord

-- Lifting a SNR to a to a Square matrix of some shape
-- TODO: need to look at shape to make this one work?
Square : SemiNearRing → Shape → SemiNearRing
Square snr L = SNR
  where
  open SemiNearRing snr

  S = Sq s L

  _+S_ : S → S → S
  One x +S One x₁ = One (x +ₛ x₁)

  _∙S_ : S → S → S
  One x ∙S One x₁ = One (x ∙ₛ x₁)

  0S = One 0ₛ

  _≃S_ : S → S → Set
  One x ≃S One x₁ = x ≃ₛ x₁

  SNR : SemiNearRing
  SNR = record
          { s = S
          ; _≃ₛ_ = _≃S_
          ; 0ₛ = 0S
          ; _+ₛ_ = _+S_
          ; _∙ₛ_ = _∙S_
          ; isCommMon = {!!}
          ; zeroˡ = {!!}
          ; zeroʳ = {!!}
          ; _<∙>_ = {!!}
          }
Square snr (B shape shape₁) = {!!}
  -- where
  -- open SemiNearRing snr

  -- S = Sq s shape

  -- open Operations s _∙ₛ_ _+ₛ_
  --   renaming (_+_ to _+S_; _*_ to _∙S_)

  -- 0S = One 0ₛ

  -- -- _≃S_ : S → S → Set
  -- -- _≃S_ m n with shape
  -- -- ... | sh = ?


  -- SNR : SemiNearRing
  -- SNR =
  --   record
  --     { s = S
  --     ; _≃ₛ_ = {!!}
  --     ; 0ₛ = {!!}
  --     ; _+ₛ_ = {!!}
  --     ; _∙ₛ_ = {!!}
  --     ; isCommMon = {!!}
  --     ; zeroˡ = {!!}
  --     ; zeroʳ = {!!}
  --     ; _<∙>_ = {!!}
  --     }
