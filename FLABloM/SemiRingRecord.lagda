\begin{code}

module SemiRingRecord where

import Algebra.FunctionProperties
  using (LeftIdentity; RightIdentity)
import Function using (_on_)
import Relation.Binary.EqReasoning as EqReasoning
import Relation.Binary.On using (isEquivalence)
import Algebra.Structures using (module IsCommutativeMonoid; IsCommutativeMonoid)
open import Relation.Binary
  using (module IsEquivalence; IsEquivalence; _Preserves₂_⟶_⟶_ ; Setoid)
open import Data.Product renaming (_,_ to _,,_) -- just to avoid clash with other commas

open import Preliminaries

open import SemiNearRingRecord

record SemiRing : Set₁ where
  field
    snr : SemiNearRing

  open SemiNearRing snr

  field
    1s : s

  open Algebra.FunctionProperties _≃s_ using (LeftIdentity; RightIdentity)

  field
    ∙-identˡs : LeftIdentity 1s _∙s_
    ∙-identʳs : RightIdentity 1s _∙s_

\end{code}
