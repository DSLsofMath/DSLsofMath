module SemiNearRingRecord where


import Algebra.FunctionProperties
  using (LeftZero; RightZero; _DistributesOverˡ_;_DistributesOverʳ_; Idempotent)
import Function using (_on_)
import Level
import Relation.Binary.EqReasoning as EqReasoning
import Relation.Binary.On using (isEquivalence)
import Algebra.Structures using (module IsCommutativeMonoid; IsCommutativeMonoid)
open import Relation.Binary
  using (module IsEquivalence; IsEquivalence; _Preserves₂_⟶_⟶_ ; Setoid)
open import Data.Product renaming (_,_ to _,,_) -- just to avoid clash with other commas

open import Preliminaries

-- SemiNearRing, from ValiantAgda
--
-- Ring without negation and multiplicative identity, "Rg"?
record SemiNearRing : Set₁ where
  field
    s : Set
    _≃ₛ_ : s → s → Set
    0ₛ : s
    _+ₛ_ : s → s → s
    _∙ₛ_ : s → s → s

  open Algebra.Structures                using (IsCommutativeMonoid)
  open Algebra.FunctionProperties _≃ₛ_   using (LeftZero; RightZero)

  field
    isCommMon : IsCommutativeMonoid _≃ₛ_ _+ₛ_ 0ₛ
    zeroˡ : LeftZero 0ₛ _∙ₛ_
    zeroʳ : RightZero 0ₛ _∙ₛ_
    _<∙>_ : ∀ {x y u v} → (x ≃ₛ y) → (u ≃ₛ v) → (x ∙ₛ u ≃ₛ y ∙ₛ v)

  infix 4 _≃ₛ_; infixl 6 _+ₛ_; infixl 7 _∙ₛ_

  open Algebra.FunctionProperties _≃ₛ_
    using (Idempotent; _DistributesOverˡ_; _DistributesOverʳ_)

  field
     idem   : Idempotent _+ₛ_

     distl  : _∙ₛ_ DistributesOverˡ _+ₛ_
     distr  : _∙ₛ_ DistributesOverʳ _+ₛ_
       -- expands to |∀ a b c →  (a +s b) *s c   ≃s   (a *s c) +s (b *s c)|

  infix  4 _≤ₛ_
  _≤ₛ_ : s -> s -> Set
  x ≤ₛ y =  x +ₛ y ≃ₛ y

  open Algebra.Structures.IsCommutativeMonoid isCommMon public
    hiding (refl)
    renaming
     (  isEquivalence  to isEquivₛ
     ;  assoc          to assocₛ
     ;  comm           to commₛ
     ;  ∙-cong         to _<+>_
     ;  identityˡ      to identityˡₛ
     )
  identityʳₛ = proj₂ identity

  sSetoid : Setoid _ _
  sSetoid = record {  Carrier        = s;
                      _≈_            = _≃ₛ_;
                      isEquivalence  = isEquivₛ }

  open IsEquivalence isEquivₛ public
    hiding (reflexive) renaming (refl to reflₛ ; sym to symₛ ; trans to transₛ)

  LowerBounds  = LowerBound _≤ₛ_
