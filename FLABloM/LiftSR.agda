open import SemiRingRecord

module LiftSR (sr : SemiRing) where

open import Shape
open import Matrix

open import Algebra.Structures
open import Relation.Binary
open import Data.Product hiding (swap)
import Relation.Binary.EqReasoning as EqReasoning


open import SemiNearRingRecord

open SemiRing sr
open SemiNearRing snr

open import LiftSNR snr

1S : {shape : Shape} → M s shape shape
1S {L} = One 1s
1S {B shape shape₁} =
  Q 1S       (0S _ _)
    (0S _ _) 1S

∙-identitylS : {r c : Shape} (x : M s r c) → (1S ∙S x) ≃S x
∙-identitylS {L} {L} (One x) = ∙-identityl x
∙-identitylS {L} {B c c₁} (Row x x₁) = ∙-identitylS x , ∙-identitylS x₁
∙-identitylS {B r r₁} {L} (Col x x₁) =
  (let open EqReasoning setoidS
  in begin
    1S ∙S x +S 0S r r₁ ∙S x₁
  ≈⟨ <+S> r L (∙-identitylS x) (zeroSˡ r r₁ L x₁) ⟩
    x +S 0S r L
  ≈⟨ commS r L x (0S r L) ⟩
    0S r L +S x
  ≈⟨ identSˡ r L x ⟩
    x
  ∎) ,
  (let open EqReasoning setoidS
  in begin
    0S r₁ r ∙S x +S 1S ∙S x₁
  ≈⟨ <+S> r₁ L (zeroSˡ r₁ r L x) (∙-identitylS x₁) ⟩
    0S r₁ L +S x₁
  ≈⟨ identSˡ r₁ L x₁ ⟩
    x₁
  ∎)
∙-identitylS {B r r₁} {B c c₁} (Q x x₁ x₂ x₃) = {!!}
