\begin{code}
module ClosedSemiRingRecord where

open import Relation.Binary.PropositionalEquality using (_≡_; refl)
import Relation.Binary.EqReasoning as EqReasoning
open import Algebra.FunctionProperties using (LeftZero; RightZero)
open import Algebra.Structures using (module IsCommutativeMonoid;
                                             IsCommutativeMonoid)
open import Data.Product
open import Data.Unit

open import Preliminaries

open import SemiNearRingRecord
open import SemiRingRecord

record ClosedSemiRing : Set₁ where
  field
    sr : SemiRing

  open SemiRing sr
  open SemiNearRing snr

  Eq : s → s → Set
  Eq w c = 1s +s w ∙s c ≃s c
  -- defining equation of the closure of a SemiRing

  Closure : s → s → Set
  Closure w c = Least _≤s_ (Eq w) c

  field
    entireQ : Entire Eq
  -- Entire R = ∀ a ⇾ ∃ \ b → R a b

  closure : s → s
  closure = fun entireQ

  closureHasAll : {w : s} → Eq w (closure w)
  closureHasAll = correct entireQ
  -- 1 + a ∙ a* = a*

\end{code}
