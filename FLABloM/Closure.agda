module Closure where

open import Shape
open import Matrix

open import SquareMatrix

open import Preliminaries
open import SemiNearRingRecord
open import ClosedSemiNearRingRecord

open import Data.Product


Closure : ClosedSemiNearRing → Shape → ClosedSemiNearRing
Closure csnr shape = CSNR
  where
  open ClosedSemiNearRing csnr using (snr)

  SNR = Square snr shape

  open SemiNearRing snr using (s)
  open SemiNearRing SNR renaming (s to S; _+s_ to _+S_; _∙s_ to _∙S_; _≃s_ to _≃S_)


  open ClosedSemiNearRing

  QS = λ W C → (W +S C ∙S C) ≃S C

  entire : ∀ r c (W : M s r c) → ∃ (QS {!!}) -- need the non-square versions of the SemiNearRing
  entire r c W = {!!}
  -- C , proof
  --   where
  --   C : S
  --   C = {!!}

  --   proof : (W +S C ∙S C) ≃S C
  --   proof = {!!}

  CSNR = record {
    snr = SNR ;
    entireQ = {!!} }
