\begin{code}
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

open import LiftSNR snr renaming (Square to SquareSNR) public

1S : {shape : Shape} → M s shape shape
1S {L} = One 1s
1S {B shape shape₁} =
  Q 1S       (0S _ _)
    (0S _ _) 1S

∙-identitylS : {r c : Shape} (x : M s r c) → (1S ∙S x) ≃S x
∙-identitylS {L} {L} (One x) = ∙-identˡs x
∙-identitylS {L} {B c c₁} (Row x x₁) = ∙-identitylS x , ∙-identitylS x₁
∙-identitylS {B r r₁} {L} (Col x x₁) =
  (let open EqReasoning setoidS
  in begin
    1S ∙S x +S 0S r r₁ ∙S x₁
  ≈⟨ <+S> r L (∙-identitylS x) (zeroSˡ r r₁ L x₁) ⟩
    x +S 0S r L
  ≈⟨ identSʳ r L x ⟩
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
∙-identitylS {B r r₁} {B c c₁} (Q x x₁ x₂ x₃) =
  -- 1S ∙S x +S 0S r r₁ ∙S x₂ ≃S x
  (let open EqReasoning setoidS
  in begin
    1S ∙S x +S 0S r r₁ ∙S x₂
  ≈⟨ <+S> r c (∙-identitylS x) (zeroSˡ r r₁ c x₂) ⟩
    x +S 0S r c
  ≈⟨ identSʳ r c x ⟩
    x
  ∎) ,
  -- 1S ∙S x₁ +S 0S r r₁ ∙S x₃ ≃S x₁
  (let open EqReasoning setoidS
  in begin
    1S ∙S x₁ +S 0S r r₁ ∙S x₃
  ≈⟨ <+S> r c₁ (∙-identitylS x₁) (zeroSˡ r r₁ c₁ x₃) ⟩
    x₁ +S 0S r c₁
  ≈⟨ identSʳ r c₁ x₁ ⟩
    x₁
  ∎) ,
  (let open EqReasoning setoidS
  in begin
    0S r₁ r ∙S x +S 1S ∙S x₂
  ≈⟨ <+S> r₁ c (zeroSˡ r₁ r c x) (∙-identitylS x₂) ⟩
    0S r₁ c +S x₂
  ≈⟨ identSˡ r₁ c x₂ ⟩
    x₂
  ∎) ,
  (let open EqReasoning setoidS
  in begin
    0S r₁ r ∙S x₁ +S 1S ∙S x₃
  ≈⟨ <+S> r₁ c₁ (zeroSˡ r₁ r c₁ x₁) (∙-identitylS x₃) ⟩
    0S r₁ c₁ +S x₃
  ≈⟨ identSˡ r₁ c₁ x₃ ⟩
    x₃
  ∎)

∙-identityrS : {r c : Shape} (x : M s r c) → (x ∙S 1S) ≃S x
∙-identityrS {L} {L} (One x) = ∙-identʳs x
∙-identityrS {L} {B c c₁} (Row x x₁) =
  (let open EqReasoning setoidS
  in begin
    x ∙S 1S +S x₁ ∙S 0S c₁ c
  ≈⟨ <+S> L c (∙-identityrS x) (zeroSʳ L c₁ c x₁) ⟩
    x +S 0S L c
  ≈⟨ identSʳ L c x ⟩
    x
  ∎) ,
  (let open EqReasoning setoidS
  in begin
    x ∙S 0S c c₁ +S x₁ ∙S 1S
  ≈⟨ <+S> L c₁ (zeroSʳ L c c₁ x) (∙-identityrS x₁) ⟩
    0S L c₁ +S x₁
  ≈⟨ identSˡ L c₁ x₁ ⟩
    x₁
  ∎)
∙-identityrS {B r r₁} {L} (Col x x₁) = ∙-identityrS x , ∙-identityrS x₁
∙-identityrS {B r r₁} {B c c₁} (Q x x₁ x₂ x₃) =
  (let open EqReasoning setoidS
  in begin
    x ∙S 1S +S x₁ ∙S 0S c₁ c
  ≈⟨ <+S> r c (∙-identityrS x) (zeroSʳ r c₁ c x₁) ⟩
    x +S 0S r c
  ≈⟨ identSʳ r c x ⟩
    x
  ∎) ,
  (let open EqReasoning setoidS
  in begin
    x ∙S 0S c c₁ +S x₁ ∙S 1S
  ≈⟨ <+S> r c₁ (zeroSʳ r c c₁ x) (∙-identityrS x₁) ⟩
    0S r c₁ +S x₁
  ≈⟨ identSˡ r c₁ x₁ ⟩
    x₁
  ∎) ,
  (let open EqReasoning setoidS
  in begin
    x₂ ∙S 1S +S x₃ ∙S 0S c₁ c
  ≈⟨ <+S> r₁ c (∙-identityrS x₂) (zeroSʳ r₁ c₁ c x₃) ⟩
    x₂ +S 0S r₁ c
  ≈⟨ identSʳ r₁ c x₂ ⟩
    x₂
  ∎) ,
  (let open EqReasoning setoidS
  in begin
     x₂ ∙S 0S c c₁ +S x₃ ∙S 1S
  ≈⟨ <+S> r₁ c₁ (zeroSʳ r₁ c c₁ x₂) (∙-identityrS x₃) ⟩
    0S r₁ c₁ +S x₃
  ≈⟨ identSˡ r₁ c₁ x₃ ⟩
    x₃
  ∎)

Square : Shape → SemiRing
Square shape =
  record
    { snr = SquareSNR shape
    ; 1s = 1S ; ∙-identˡs = ∙-identitylS
    ; ∙-identʳs = ∙-identityrS }

\end{code}
