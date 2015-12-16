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

oneS : {shape : Shape} → M s shape shape
oneS {L}              =  One ones
oneS {B shape shape₁} =  Q oneS       (zerS _ _)
                         (zerS _ _) oneS

*-identlS : {r c : Shape} (x : M s r c) → (oneS *S x) ≃S x
*-identlS {L}      {L}      (One x)    = *-identls x
*-identlS {L}      {B c c₁} (Row x x₁) = *-identlS x , *-identlS x₁
*-identlS {B r r₁} {L}      (Col x x₁) =
  (let open EqReasoning setoidS
  in begin
    oneS *S x +S zerS r r₁ *S x₁
  ≈⟨ <+S> r L (*-identlS x) (zeroSˡ r r₁ L x₁) ⟩
    x +S zerS r L
  ≈⟨ identSʳ r L x ⟩
    x
  ∎) ,
  (let open EqReasoning setoidS
  in begin
    zerS r₁ r *S x +S oneS *S x₁
  ≈⟨ <+S> r₁ L (zeroSˡ r₁ r L x) (*-identlS x₁) ⟩
    zerS r₁ L +S x₁
  ≈⟨ identSˡ r₁ L x₁ ⟩
    x₁
  ∎)
*-identlS {B r r₁} {B c c₁} (Q x x₁ x₂ x₃) =
  -- oneS *S x +S zerS r r₁ *S x₂ ≃S x
  (let open EqReasoning setoidS
  in begin
    oneS *S x +S zerS r r₁ *S x₂
  ≈⟨ <+S> r c (*-identlS x) (zeroSˡ r r₁ c x₂) ⟩
    x +S zerS r c
  ≈⟨ identSʳ r c x ⟩
    x
  ∎) ,
  -- oneS *S x₁ +S zerS r r₁ *S x₃ ≃S x₁
  (let open EqReasoning setoidS
  in begin
    oneS *S x₁ +S zerS r r₁ *S x₃
  ≈⟨ <+S> r c₁ (*-identlS x₁) (zeroSˡ r r₁ c₁ x₃) ⟩
    x₁ +S zerS r c₁
  ≈⟨ identSʳ r c₁ x₁ ⟩
    x₁
  ∎) ,
  (let open EqReasoning setoidS
  in begin
    zerS r₁ r *S x +S oneS *S x₂
  ≈⟨ <+S> r₁ c (zeroSˡ r₁ r c x) (*-identlS x₂) ⟩
    zerS r₁ c +S x₂
  ≈⟨ identSˡ r₁ c x₂ ⟩
    x₂
  ∎) ,
  (let open EqReasoning setoidS
  in begin
    zerS r₁ r *S x₁ +S oneS *S x₃
  ≈⟨ <+S> r₁ c₁ (zeroSˡ r₁ r c₁ x₁) (*-identlS x₃) ⟩
    zerS r₁ c₁ +S x₃
  ≈⟨ identSˡ r₁ c₁ x₃ ⟩
    x₃
  ∎)

*-identrS : {r c : Shape} (x : M s r c) → (x *S oneS) ≃S x
*-identrS {L} {L} (One x) = *-identrs x
*-identrS {L} {B c c₁} (Row x x₁) =
  (let open EqReasoning setoidS
  in begin
    x *S oneS +S x₁ *S zerS c₁ c
  ≈⟨ <+S> L c (*-identrS x) (zeroSʳ L c₁ c x₁) ⟩
    x +S zerS L c
  ≈⟨ identSʳ L c x ⟩
    x
  ∎) ,
  (let open EqReasoning setoidS
  in begin
    x *S zerS c c₁ +S x₁ *S oneS
  ≈⟨ <+S> L c₁ (zeroSʳ L c c₁ x) (*-identrS x₁) ⟩
    zerS L c₁ +S x₁
  ≈⟨ identSˡ L c₁ x₁ ⟩
    x₁
  ∎)
*-identrS {B r r₁} {L} (Col x x₁) = *-identrS x , *-identrS x₁
*-identrS {B r r₁} {B c c₁} (Q x x₁ x₂ x₃) =
  (let open EqReasoning setoidS
  in begin
    x *S oneS +S x₁ *S zerS c₁ c
  ≈⟨ <+S> r c (*-identrS x) (zeroSʳ r c₁ c x₁) ⟩
    x +S zerS r c
  ≈⟨ identSʳ r c x ⟩
    x
  ∎) ,
  (let open EqReasoning setoidS
  in begin
    x *S zerS c c₁ +S x₁ *S oneS
  ≈⟨ <+S> r c₁ (zeroSʳ r c c₁ x) (*-identrS x₁) ⟩
    zerS r c₁ +S x₁
  ≈⟨ identSˡ r c₁ x₁ ⟩
    x₁
  ∎) ,
  (let open EqReasoning setoidS
  in begin
    x₂ *S oneS +S x₃ *S zerS c₁ c
  ≈⟨ <+S> r₁ c (*-identrS x₂) (zeroSʳ r₁ c₁ c x₃) ⟩
    x₂ +S zerS r₁ c
  ≈⟨ identSʳ r₁ c x₂ ⟩
    x₂
  ∎) ,
  (let open EqReasoning setoidS
  in begin
     x₂ *S zerS c c₁ +S x₃ *S oneS
  ≈⟨ <+S> r₁ c₁ (zeroSʳ r₁ c c₁ x₂) (*-identrS x₃) ⟩
    zerS r₁ c₁ +S x₃
  ≈⟨ identSˡ r₁ c₁ x₃ ⟩
    x₃
  ∎)

Square : Shape → SemiRing
Square shape = record
  { snr = SquareSNR shape
  ; ones = oneS
  ; *-identls = *-identlS
  ; *-identrs = *-identrS }

\end{code}
