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
    _≃s_ : s → s → Set
    0s : s
    _+s_ : s → s → s
    _∙s_ : s → s → s

  open Algebra.Structures                using (IsCommutativeMonoid)
  open Algebra.FunctionProperties _≃s_   using (LeftZero; RightZero)

  field
    isCommMon : IsCommutativeMonoid _≃s_ _+s_ 0s
    zeroˡ : LeftZero 0s _∙s_
    zeroʳ : RightZero 0s _∙s_
    _<∙>_ : ∀ {x y u v} → (x ≃s y) → (u ≃s v) → (x ∙s u ≃s y ∙s v)

  infix 4 _≃s_; infixl 6 _+s_; infixl 7 _∙s_

  open Algebra.FunctionProperties _≃s_
    using (Idempotent; _DistributesOverˡ_; _DistributesOverʳ_)

  field
     idem   : Idempotent _+s_
       -- ∀ x → x +s x ≃s x

     distl  : _∙s_ DistributesOverˡ _+s_
     distr  : _∙s_ DistributesOverʳ _+s_
       -- expands to |∀ a b c →  (a +s b) *s c   ≃s   (a *s c) +s (b *s c)|

  infix  4 _≤s_
  _≤s_ : s -> s -> Set
  x ≤s y =  x +s y ≃s y

  open Algebra.Structures.IsCommutativeMonoid isCommMon public
    hiding (refl)
    renaming
     (  isEquivalence  to isEquivs
     ;  assoc          to assocs
     ;  comm           to comms
     ;  ∙-cong         to _<+>_
     ;  identityˡ      to identityˡs
     )

  identityʳs = proj₂ identity

  sSetoid : Setoid _ _
  sSetoid = record {  Carrier        = s;
                      _≈_            = _≃s_;
                      isEquivalence  = isEquivs }

  open IsEquivalence isEquivs public
    hiding (reflexive) renaming (refl to refls ; sym to syms ; trans to transs)

  LowerBounds  = LowerBound _≤s_
