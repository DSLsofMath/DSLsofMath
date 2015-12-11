\begin{code}
open import ClosedSemiRingRecord
open import SemiRingRecord
open import SemiNearRingRecord

module LiftCSR (csr : ClosedSemiRing) where

open import Data.Product

open import Shape
open import Matrix

import LiftSR renaming (Square to SquareSR)

open ClosedSemiRing csr
open SemiRing sr
open SemiNearRing snr

open LiftSR sr

EqS : ∀ {sh} → M s sh sh → M s sh sh → Set
EqS w c = 1S +S w ∙S c ≃S c

entireQS : ∀ {sh} (w : M s sh sh) → ∃ λ c → EqS w c
entireQS {L} (One w) =
  let (c , p) = entireQ w
  in (One c , p)
entireQS {B sh sh₁} (Q w w₁ w₂ w₃) =
  {!!} , {!!}

Square : Shape → ClosedSemiRing
Square shape =
  record
    { sr = SquareSR shape
    ; entireQ = entireQS }


\end{code}
