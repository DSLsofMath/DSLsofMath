-- Matrices indexed by shape
module SquareMatrix where

open import Shape
open import Matrix

-- Square matrices have the same shape in both dimensions
Sq : Set → Shape → Set
Sq a shape = M a shape shape

open import SemiNearRingRecord
open import Algebra.Structures
open import Relation.Binary
open import Data.Product hiding (swap)

open import Relation.Binary.PropositionalEquality hiding (trans; sym) renaming (refl to refl-≡)
import Relation.Binary.EqReasoning as EqReasoning

import LiftSNR

-- Lifting a SNR to a to a Square matrix of some shape
Square : SemiNearRing → Shape → SemiNearRing
Square snr shape = SNR
  where

  open LiftSNR snr

  isEquivS =
    record
      { refl = reflS shape shape
      ; sym = symS shape shape
      ; trans = transS shape shape }

  isSemgroupS =
    record
      { isEquivalence = isEquivS
      ; assoc = assocS shape shape
      ; ∙-cong = <+S> shape shape }

  isCommMonS =
    record
      { isSemigroup = isSemgroupS
      ; identityˡ = identSˡ shape shape
      ; comm = commS shape shape }

  SNR : SemiNearRing
  SNR =
    record
      { s = S
      ; _≃s_ = _≃S_ {shape} {shape}
      ; 0s = 0S shape shape
      ; _+s_ = _+S_
      ; _∙s_ = _∙S_
      ; isCommMon = isCommMonS
      ; zeroˡ = zeroSˡ shape shape shape
      ; zeroʳ = zeroSʳ shape shape shape
      ; _<∙>_ = <∙S> shape shape shape
      ; idem = idemS shape shape
      ; distl = distlS {shape} {shape}
      ; distr = distrS {shape} {shape}
      }
