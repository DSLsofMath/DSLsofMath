\begin{code}

module ClosedSemiNearRingRecord where

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


record ClosedSemiNearRing : Set₁ where
  field
    snr : SemiNearRing

  open SemiNearRing snr

  Eq : s → s → Set
  Eq w c = w +s c ∙s c ≃s c

  Closure : s → s → Set
  Closure w c = Least _≤s_ (Eq w) c

  field
    entireQ : Entire Eq
  -- Entire R = ∀ a ⇾ ∃ \ b → R a b

  closure : s → s
  closure = fun entireQ

  closureHasAll : {w : s} → Eq w (closure w)
  closureHasAll = correct entireQ
  -- ~ a+ = a +s a+ ‌∙s a+

\end{code}
