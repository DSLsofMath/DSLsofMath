open import SemiNearRingRecord
open import ClosedSemiNearRingRecord

module LiftCSNR (csnr : ClosedSemiNearRing) where

open import Relation.Binary.PropositionalEquality using (_≡_; refl)
import Relation.Binary.EqReasoning as EqReasoning
open import Algebra.FunctionProperties using (LeftZero; RightZero)
open import Algebra.Structures using (module IsCommutativeMonoid;
                                             IsCommutativeMonoid)
open import Data.Product
open import Data.Unit
import Level
open import SemiNearRingRecord
open import Preliminaries

open ClosedSemiNearRing csnr --using (snr; s)
open SemiNearRing snr


open import LiftSNR snr

open import Shape
open import Matrix

EqS : {ss : Shape} → M s ss ss → M s ss ss → Set
EqS W C = W +S (C ∙S C) ≃S C

entireQS : (ss : Shape) (W : M s ss ss) → ∃ (EqS W)
entireQS L (One x) with entireQ x
... | c , pf = (One c) , pf
entireQS (B ss ss₁) (Q w w₁ w₂ w₃) =
  let
    wc , pf = entireQS ss w
    w1c , pf1 = {!entireQS !}
    c21 = {!!}
    c22 = {!!}
  in
  (Q {!!} {!!} {!!} {!!}) ,
  ({!!} , {!!} , {!!} , {!!})
