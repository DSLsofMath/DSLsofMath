\begin{code}
open import ClosedSemiRingRecord

module LiftCSR (csr : ClosedSemiRing) where

open import Data.Product
import Relation.Binary.EqReasoning as EqReasoning


open import Shape
open import Matrix

open import SemiRingRecord
open import SemiNearRingRecord

import LiftSR renaming (Square to SquareSR)

open ClosedSemiRing csr
open SemiRing sr
open SemiNearRing snr

open LiftSR sr

EqS : ∀ {sh} → M s sh sh → M s sh sh → Set
EqS w c = oneS +S w *S c ≃S c

entireQS : ∀ {sh} (w : M s sh sh) → Σ (M s sh sh) λ c → EqS w c
entireQS {L} (One w) =
  let (c , p) = entireQ w
  in (One c , p)
entireQS {B sh sh₁} (Q w11 w12 w21 w22) =
  let
    (w11* , p11) = entireQS w11
    Δ = w22 +S w21 *S w11* *S w12
    (Δ* , pΔ) = entireQS Δ
  in
  Q (w11* +S w11* *S w12 *S Δ* *S w21 *S w11*) (w11* *S w12 *S Δ*)
    (Δ* *S w21 *S w11*)                        Δ* ,
  {!!} ,
  (let open EqReasoning setoidS
  in begin
    zerS _ _ +S w11 *S w11* *S w12 *S Δ* +S w12 *S Δ*
  ≈⟨ {!!} ⟩ -- 0S identl
    w11 *S w11* *S w12 *S Δ* +S w12 *S Δ*
  ≈⟨ {!!} ⟩ -- +S commutes
    w12 *S Δ* +S w11 *S w11* *S w12 *S Δ*
  ≈⟨ {!!} ⟩ -- distribr backwards
    (oneS +S w11 *S w11*) *S w12 *S Δ*
  ≈⟨ {!!} ⟩ -- p11 on left of *S
    w11* *S w12 *S Δ*
  ∎) ,
  {!!} ,
  {!!}
-- ...

-- 0 + w11 * w11* * w12 * Δ* + w12 * Δ* = w11* * w12 * Δ*
-- {w11* = 1 + w11 * w11*}
-- => w11 * w11* * w12 * Δ* + w12 * Δ* = 1 + w11 * w11*

-- Lehmann says:
-- (Q w11 w12 w21 w22)* =
--   Q (w11* + w11* * w12 * Δ* * w21 * w11*) (w11* * w12 * Δ*)
--     (Δ* * w21 * w11*)                     (Δ*)

Square : Shape → ClosedSemiRing
Square shape =
  record
    { sr = SquareSR shape
    ; entireQ = entireQS }


\end{code}
