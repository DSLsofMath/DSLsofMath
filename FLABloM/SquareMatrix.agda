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
open import Data.Product

-- Lifting a SNR to a to a Square matrix of some shape
Square : SemiNearRing → Shape → SemiNearRing
Square snr shape = SNR
  where
  open SemiNearRing snr

  S = Sq s shape

  open Operations s _∙ₛ_ _+ₛ_
    renaming (_+_ to _+S_; _*_ to _∙S_)

  lift0 : (r c : Shape) → M s r c
  lift0 L L = One 0ₛ
  lift0 L (B s s₁) = Row (lift0 L s) (lift0 L s₁)
  lift0 (B r r₁) L = Col (lift0 r L) (lift0 r₁ L)
  lift0 (B r r₁) (B s s₁) =
      Q (lift0 r s) (lift0 r s₁)
        (lift0 r₁ s) (lift0 r₁ s₁)

  0S = lift0 shape shape

  lift≃ : (r c : Shape) →
        M s r c → M s r c → Set
  lift≃ L L (One x) (One x₁) = x ≃ₛ x₁
  lift≃ L (B c₁ c₂) (Row m m₁) (Row n n₁) = lift≃ L c₁ m n × lift≃ L c₂ m₁ n₁
  lift≃ (B r₁ r₂) L (Col m m₁) (Col n n₁) = lift≃ r₁ L m n × lift≃ r₂ L m₁ n₁
  lift≃ (B r₁ r₂) (B c₁ c₂) (Q m00 m01 m10 m11) (Q n00 n01 n10 n11) =
    lift≃ r₁ c₁ m00 n00 × lift≃ r₁ c₂ m01 n01 ×
    lift≃ r₂ c₁ m10 n10 × lift≃ r₂ c₂ m11 n11

  _≃S_ = lift≃ shape shape

  liftRefl : (r c : Shape) →
    {X : M s r c} → lift≃ r c X X
  liftRefl L L {X = One x} = reflₛ {x}
  liftRefl L (B c₁ c₂) {X = Row X Y} = liftRefl L c₁ , liftRefl L c₂
  liftRefl (B r₁ r₂) L {X = Col X Y} = liftRefl r₁ L , liftRefl r₂ L
  liftRefl (B r₁ r₂) (B c₁ c₂) {X = Q X Y Z W} =
    liftRefl r₁ c₁ , liftRefl r₁ c₂ ,
    liftRefl r₂ c₁ , liftRefl r₂ c₂

  reflS = liftRefl shape shape

  liftSym : (r c : Shape) →
    let R' = lift≃ r c
    in {i j : M s r c} → R' i j → R' j i
  liftSym L L {One x} {One x₁} p = symₛ p
  liftSym L (B c₁ c₂) {Row i₁ i₂} {Row j₁ j₂} (p , q) = liftSym L c₁ p , liftSym L c₂ q
  liftSym (B r₁ r₂) L {Col i₁ i} {Col j j₁} (p , q) = liftSym r₁ L p , liftSym r₂ L q
  liftSym (B r r₂) (B c₁ c₂) {Q i₂ i i₃ i₁} {Q j j₁ j₂ j₃} (p , q , x , y) =
    liftSym r c₁ p , liftSym r c₂ q ,
    liftSym r₂ c₁ x , liftSym r₂ c₂ y

  symS : {i j : M s shape shape} → i ≃S j → j ≃S i
  symS p = liftSym shape shape p

  liftTrans : (r c : Shape) →
    let R' = lift≃ r c
    in {i j k : M s r c} → R' i j → R' j k → R' i k
  liftTrans L L {One x} {One x₁} {One x₂} p q = trans p q
  liftTrans L (B c₁ c₂) {Row i i₁} {Row j j₁} {Row k k₁} (p , q) (p' , q') =
    liftTrans L c₁ p p' , liftTrans L c₂ q q'
  liftTrans (B r₁ r₂) L {Col i i₁} {Col j j₁} {Col k k₁} (p , q) (p' , q') =
    liftTrans r₁ L p p' , liftTrans r₂ L q q'
  liftTrans (B r₁ r₂) (B c₁ c₂) {Q i₂ i i₃ i₁} {Q j₂ j j₃ j₁} {Q k k₁ k₂ k₃}
    (p , q , x , y) (p' , q' , x' , y') =
    liftTrans r₁ c₁ p p' , liftTrans r₁ c₂ q q' ,
    liftTrans r₂ c₁ x x' , liftTrans r₂ c₂ y y'

  transS : {i j k : M s shape shape} → i ≃S j → j ≃S k → i ≃S k
  transS p q = liftTrans shape shape p q

  liftAssoc : (r c : Shape) (x y z : M s r c) → lift≃ r c ((x +S y) +S z) (x +S (y +S z))
  liftAssoc L L (One x) (One y) (One z) = assocₛ x y z
  liftAssoc L (B c c₁) (Row x x₁) (Row y y₁) (Row z z₁) =
    liftAssoc L c x y z  , liftAssoc L c₁ x₁ y₁ z₁
  liftAssoc (B r r₁) L (Col x x₁) (Col y y₁) (Col z z₁) =
    liftAssoc r L x y z , liftAssoc r₁ L x₁ y₁ z₁
  liftAssoc (B r r₁) (B c c₁) (Q x x₁ x₂ x₃) (Q y y₁ y₂ y₃) (Q z z₁ z₂ z₃) =
    (liftAssoc r c x y z) , (liftAssoc r c₁ x₁ y₁ z₁) ,
    (liftAssoc r₁ c x₂ y₂ z₂) , (liftAssoc r₁ c₁ x₃ y₃ z₃)

  assocS : (x y z : M s shape shape) → ((x +S y) +S z) ≃S (x +S (y +S z))
  assocS x y z = liftAssoc shape shape x y z

  isEquivS =
    record
      { refl = reflS
      ; sym = symS
      ; trans = transS }
  isSemgroupS =
    record
      { isEquivalence = isEquivS
      ; assoc = assocS
      ; ∙-cong = {!!} }
  isCommMonS =
    record
      { isSemigroup = isSemgroupS
      ; identityˡ = {!!}
      ; comm = {!!} }


  SNR : SemiNearRing
  SNR =
    record
      { s = S
      ; _≃ₛ_ = _≃S_
      ; 0ₛ = 0S
      ; _+ₛ_ = _+S_
      ; _∙ₛ_ = _∙S_
      ; isCommMon = isCommMonS
      ; zeroˡ = {!!}
      ; zeroʳ = {!!}
      ; _<∙>_ = {!!}
      }
