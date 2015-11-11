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

  0S : (r c : Shape) → M s r c
  0S L L = One 0ₛ
  0S L (B s s₁) = Row (0S L s) (0S L s₁)
  0S (B r r₁) L = Col (0S r L) (0S r₁ L)
  0S (B r r₁) (B s s₁) =
      Q (0S r s) (0S r s₁)
        (0S r₁ s) (0S r₁ s₁)

  ≃S : (r c : Shape) →
        M s r c → M s r c → Set
  ≃S L L (One x) (One x₁) = x ≃ₛ x₁
  ≃S L (B c₁ c₂) (Row m m₁) (Row n n₁) = ≃S L c₁ m n × ≃S L c₂ m₁ n₁
  ≃S (B r₁ r₂) L (Col m m₁) (Col n n₁) = ≃S r₁ L m n × ≃S r₂ L m₁ n₁
  ≃S (B r₁ r₂) (B c₁ c₂) (Q m00 m01 m10 m11) (Q n00 n01 n10 n11) =
    ≃S r₁ c₁ m00 n00 × ≃S r₁ c₂ m01 n01 ×
    ≃S r₂ c₁ m10 n10 × ≃S r₂ c₂ m11 n11


  _≃S'_ : {r c : Shape} → M s r c → M s r c → Set
  _≃S'_ {r} {c} m n = ≃S r c m n


  reflS : (r c : Shape) →
    {X : M s r c} → X ≃S' X
  reflS L L {X = One x} = reflₛ {x}
  reflS L (B c₁ c₂) {X = Row X Y} = reflS L c₁ , reflS L c₂
  reflS (B r₁ r₂) L {X = Col X Y} = reflS r₁ L , reflS r₂ L
  reflS (B r₁ r₂) (B c₁ c₂) {X = Q X Y Z W} =
    reflS r₁ c₁ , reflS r₁ c₂ ,
    reflS r₂ c₁ , reflS r₂ c₂

  symS : (r c : Shape) →
    {i j : M s r c} → i ≃S' j → j ≃S' i
  symS L L {One x} {One x₁} p = symₛ p
  symS L (B c₁ c₂) {Row i₁ i₂} {Row j₁ j₂} (p , q) = symS L c₁ p , symS L c₂ q
  symS (B r₁ r₂) L {Col i₁ i} {Col j j₁} (p , q) = symS r₁ L p , symS r₂ L q
  symS (B r r₂) (B c₁ c₂) {Q i₂ i i₃ i₁} {Q j j₁ j₂ j₃} (p , q , x , y) =
    symS r c₁ p , symS r c₂ q ,
    symS r₂ c₁ x , symS r₂ c₂ y

  transS : (r c : Shape) →
    {i j k : M s r c} → i ≃S' j → j ≃S' k → i ≃S' k
  transS L L {One x} {One x₁} {One x₂} p q = trans p q
  transS L (B c₁ c₂) {Row i i₁} {Row j j₁} {Row k k₁} (p , q) (p' , q') =
    transS L c₁ p p' , transS L c₂ q q'
  transS (B r₁ r₂) L {Col i i₁} {Col j j₁} {Col k k₁} (p , q) (p' , q') =
    transS r₁ L p p' , transS r₂ L q q'
  transS (B r₁ r₂) (B c₁ c₂) {Q i₂ i i₃ i₁} {Q j₂ j j₃ j₁} {Q k k₁ k₂ k₃}
    (p , q , x , y) (p' , q' , x' , y') =
    transS r₁ c₁ p p' , transS r₁ c₂ q q' ,
    transS r₂ c₁ x x' , transS r₂ c₂ y y'

  isEquivS =
    record
      { refl = reflS shape shape
      ; sym = symS shape shape
      ; trans = transS shape shape }

  liftAssoc : (r c : Shape) (x y z : M s r c) → ((x +S y) +S z) ≃S' (x +S (y +S z))
  liftAssoc L L (One x) (One y) (One z) = assocₛ x y z
  liftAssoc L (B c c₁) (Row x x₁) (Row y y₁) (Row z z₁) =
    liftAssoc L c x y z  , liftAssoc L c₁ x₁ y₁ z₁
  liftAssoc (B r r₁) L (Col x x₁) (Col y y₁) (Col z z₁) =
    liftAssoc r L x y z , liftAssoc r₁ L x₁ y₁ z₁
  liftAssoc (B r r₁) (B c c₁) (Q x x₁ x₂ x₃) (Q y y₁ y₂ y₃) (Q z z₁ z₂ z₃) =
    (liftAssoc r c x y z) , (liftAssoc r c₁ x₁ y₁ z₁) ,
    (liftAssoc r₁ c x₂ y₂ z₂) , (liftAssoc r₁ c₁ x₃ y₃ z₃)

  lift<+> : (r c : Shape) {x y u v : M s r c} →
    x ≃S' y → u ≃S' v → (x +S u) ≃S' (y +S v)
  lift<+> L L {One x} {One x₁} {One x₂} {One x₃} p q = p <+> q
  lift<+> L (B c c₁) {Row x x₁} {Row y y₁} {Row u u₁} {Row v v₁} (p , p₁) (q , q₁) =
    lift<+> L c p q , lift<+> L c₁ p₁ q₁
  lift<+> (B r r₁) L {Col x x₁} {Col y y₁} {Col u u₁} {Col v v₁} (p , p₁) (q , q₁) =
    lift<+> r L p q , lift<+> r₁ L p₁ q₁
  lift<+> (B r r₁) (B c c₁) {Q x x₁ x₂ x₃} {Q y y₁ y₂ y₃} {Q u u₁ u₂ u₃} {Q v v₁ v₂ v₃}
    (p , p₁ , p₂ , p₃) (q , q₁ , q₂ , q₃) =
    lift<+> r c p q , lift<+> r c₁ p₁ q₁ ,
    lift<+> r₁ c p₂ q₂ , lift<+> r₁ c₁ p₃ q₃

  isSemgroupS =
    record
      { isEquivalence = isEquivS
      ; assoc = liftAssoc shape shape
      ; ∙-cong = lift<+> shape shape }


  liftIdentˡ : (r c : Shape) (x : M s r c) →
     0S r c +S x ≃S' x
  liftIdentˡ L L (One x) = identityˡₛ x
  liftIdentˡ L (B c c₁) (Row x x₁) = liftIdentˡ L c x , liftIdentˡ L c₁ x₁
  liftIdentˡ (B r r₁) L (Col x x₁) = liftIdentˡ r L x , liftIdentˡ r₁ L x₁
  liftIdentˡ (B r r₁) (B c c₁) (Q x x₁ x₂ x₃) =
    liftIdentˡ r c x , liftIdentˡ r c₁ x₁ ,
    liftIdentˡ r₁ c x₂ , liftIdentˡ r₁ c₁ x₃

  liftComm : (r c : Shape) → (x y : M s r c) →
    (x +S y) ≃S' (y +S x)
  liftComm L L (One x) (One x₁) = commₛ x x₁
  liftComm L (B c c₁) (Row x x₁) (Row y y₁) = (liftComm L c x y) , (liftComm L c₁ x₁ y₁)
  liftComm (B r r₁) L (Col x x₁) (Col y y₁) = (liftComm r L x y) , (liftComm r₁ L x₁ y₁)
  liftComm (B r r₁) (B c c₁) (Q x x₁ x₂ x₃) (Q y y₁ y₂ y₃) =
    liftComm r c x y , liftComm r c₁ x₁ y₁ ,
    liftComm r₁ c x₂ y₂ , liftComm r₁ c₁ x₃ y₃

  isCommMonS =
    record
      { isSemigroup = isSemgroupS
      ; identityˡ = liftIdentˡ shape shape
      ; comm = liftComm shape shape }


  -- TODO: can I use ≃S to 'rewrite' types?
  liftZeroˡ : (a b c : Shape) (x : M s b c) →
    let 0ˡ = 0S a b
        0ʳ = 0S a c
    in (0ˡ ∙S x) ≃S' 0ʳ
  liftZeroˡ L L L (One x) = zeroˡ x
  liftZeroˡ L L (B c c₁) (Row x x₁) = (liftZeroˡ L L c x) , (liftZeroˡ L L c₁ x₁)
  liftZeroˡ L (B b b₁) L (Col x x₁) = {!!}
  liftZeroˡ L (B b b₁) (B c c₁) (Q x x₁ x₂ x₃) = {!!} , {!!}
  liftZeroˡ (B a a₁) L L (One x) = liftZeroˡ a L L (One x) , liftZeroˡ a₁ L L (One x)
  liftZeroˡ (B a a₁) L (B c c₁) (Row x x₁) =
    liftZeroˡ a L c x , liftZeroˡ a L c₁ x₁ ,
    liftZeroˡ a₁ L c x , liftZeroˡ a₁ L c₁ x₁
  liftZeroˡ (B a a₁) (B b b₁) L (Col x x₁) = {!!} , {!!}
  liftZeroˡ (B a a₁) (B b b₁) (B c c₁) (Q x x₁ x₂ x₃) =
    {!liftZeroˡ a b c x!} , ({!!} , ({!!} , {!!}))


  SNR : SemiNearRing
  SNR =
    record
      { s = S
      ; _≃ₛ_ = ≃S shape shape
      ; 0ₛ = 0S shape shape
      ; _+ₛ_ = _+S_
      ; _∙ₛ_ = _∙S_
      ; isCommMon = isCommMonS
      ; zeroˡ = liftZeroˡ shape shape shape
      ; zeroʳ = {!!}
      ; _<∙>_ = {!!}
      }
