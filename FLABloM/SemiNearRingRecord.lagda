
%if False
\begin{code}
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
\end{code}
%endif

\paragraph{Seminearrings}

The weakest structure in this development are semi near rings, we
define an Agda record to hold the operations and properties of a
SemiNearRing
\begin{code}
record SemiNearRing : Set₁ where
\end{code}
A semi near ring for a type |s| needs a equivalence relation |≃s|, a
zero |zers| and two binary operations |+s| and |*s| (often called addition
and multiplication).
%
\savecolumns[SNRR]
\begin{code}
  field
    s : Set
    _≃s_ : s → s → Set
    zers : s
    _+s_ : s → s → s
    _*s_ : s → s → s

  open Algebra.Structures
    using (IsCommutativeMonoid)
  open Algebra.FunctionProperties _≃s_
    using (LeftZero; RightZero)
\end{code}
A seminearring is also a commutative monoid under addition (i.e. |+s|
commutes and zero is the left and right identity of |+s|)
%
\restorecolumns[SNRR]
\begin{code}
  field
    isCommMon : IsCommutativeMonoid _≃s_ _+s_ zers
    zeroˡ : LeftZero zers _*s_
    zeroʳ : RightZero zers _*s_
    _<*>_ : ∀ {x y u v} → (x ≃s y) → (u ≃s v) → (x *s u ≃s y *s v)

  infix 4 _≃s_; infixl 6 _+s_; infixl 7 _*s_

\end{code}

The semirings in this development also have idempotent addition and
are distributive.
%
\restorecolumns[SNRR]
\begin{code}
  open Algebra.FunctionProperties _≃s_
    using (Idempotent; _DistributesOverˡ_; _DistributesOverʳ_)

  field
     idem   : Idempotent _+s_
       -- expands to |∀ x → x +s x ≃s x|

     distl  : _*s_ DistributesOverˡ _+s_
     distr  : _*s_ DistributesOverʳ _+s_
       -- expands to |∀ a b c →  (a +s b) *s c   ≃s   (a *s c) +s (b *s c)|
\end{code}
%if False
\begin{code}
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
\end{code}
%endif
