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

open import Relation.Binary.PropositionalEquality hiding (trans; sym) renaming (refl to refl-≡)
import Relation.Binary.EqReasoning as EqReasoning


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

  <+S> : (r c : Shape) {x y u v : M s r c} →
    x ≃S' y → u ≃S' v → (x +S u) ≃S' (y +S v)
  <+S> L L {One x} {One x₁} {One x₂} {One x₃} p q = p <+> q
  <+S> L (B c c₁) {Row x x₁} {Row y y₁} {Row u u₁} {Row v v₁} (p , p₁) (q , q₁) =
    <+S> L c p q , <+S> L c₁ p₁ q₁
  <+S> (B r r₁) L {Col x x₁} {Col y y₁} {Col u u₁} {Col v v₁} (p , p₁) (q , q₁) =
    <+S> r L p q , <+S> r₁ L p₁ q₁
  <+S> (B r r₁) (B c c₁) {Q x x₁ x₂ x₃} {Q y y₁ y₂ y₃} {Q u u₁ u₂ u₃} {Q v v₁ v₂ v₃}
    (p , p₁ , p₂ , p₃) (q , q₁ , q₂ , q₃) =
    <+S> r c p q , <+S> r c₁ p₁ q₁ ,
    <+S> r₁ c p₂ q₂ , <+S> r₁ c₁ p₃ q₃

  isSemgroupS =
    record
      { isEquivalence = isEquivS
      ; assoc = liftAssoc shape shape
      ; ∙-cong = <+S> shape shape }


  identSˡ : (r c : Shape) (x : M s r c) →
     0S r c +S x ≃S' x
  identSˡ L L (One x) = identityˡₛ x
  identSˡ L (B c c₁) (Row x x₁) = identSˡ L c x , identSˡ L c₁ x₁
  identSˡ (B r r₁) L (Col x x₁) = identSˡ r L x , identSˡ r₁ L x₁
  identSˡ (B r r₁) (B c c₁) (Q x x₁ x₂ x₃) =
    identSˡ r c x , identSˡ r c₁ x₁ ,
    identSˡ r₁ c x₂ , identSˡ r₁ c₁ x₃

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
      ; identityˡ = identSˡ shape shape
      ; comm = liftComm shape shape }


  setoidS : (r c : Shape) → Setoid _ _
  setoidS r c =
    record
      { Carrier = M s r c
      ; _≈_ = ≃S r c
      ; isEquivalence =
        record
          { refl = reflS r c ; sym = symS r c ; trans = transS r c } }


  zeroSˡ : (a b c : Shape) (x : M s b c) →
    (0S a b ∙S x) ≃S' 0S a c
  zeroSˡ L L L (One x) = zeroˡ x
  zeroSˡ L L (B c c₁) (Row x x₁) = (zeroSˡ L L c x) , (zeroSˡ L L c₁ x₁)
  zeroSˡ L (B b b₁) L (Col x x₁) =
    let
      open EqReasoning (setoidS L L)
      ih = zeroSˡ L b L x
      ih₁ = zeroSˡ L b₁ L x₁
    in begin
      (Row (0S L b) (0S L b₁) ∙S (Col x x₁))
    ≡⟨ refl-≡ ⟩
      (0S L b) ∙S x +S (0S L b₁) ∙S x₁
    ≈⟨ <+S> L L {0S L b ∙S x} {0S L L} {0S L b₁ ∙S x₁} {0S L L} ih ih₁ ⟩
      0S L L +S 0S L L
    ≈⟨ identSˡ L L (0S L L) ⟩
      0S L L
    ∎
  zeroSˡ L (B b b₁) (B c c₁) (Q x x₁ x₂ x₃) =
    (let
      open EqReasoning (setoidS L c)
      ih = zeroSˡ L b c x
      ih₁ = zeroSˡ L b₁ c x₂
     in begin
       0S L b ∙S x +S 0S L b₁ ∙S x₂
     ≈⟨ <+S> L c ih ih₁ ⟩
       0S L c +S 0S L c
     ≈⟨ identSˡ L c (0S L c) ⟩
       0S L c
     ∎) ,
    (let
      open EqReasoning (setoidS L c₁)
      ih = zeroSˡ L b c₁ x₁
      ih₁ = zeroSˡ L b₁ c₁ x₃
    in begin
      0S L b ∙S x₁ +S 0S L b₁ ∙S x₃
    ≈⟨ <+S> L c₁ ih ih₁ ⟩
      0S L c₁ +S 0S L c₁
    ≈⟨ identSˡ L c₁ _ ⟩
      0S L c₁
    ∎)
  zeroSˡ (B a a₁) L L (One x) = zeroSˡ a L L (One x) , zeroSˡ a₁ L L (One x)
  zeroSˡ (B a a₁) L (B c c₁) (Row x x₁) =
    zeroSˡ a L c x , zeroSˡ a L c₁ x₁ ,
    zeroSˡ a₁ L c x , zeroSˡ a₁ L c₁ x₁
  zeroSˡ (B a a₁) (B b b₁) L (Col x x₁) =
    (let
      open EqReasoning (setoidS a L)
      ih = zeroSˡ a b L x
      ih₁ = zeroSˡ a b₁ L x₁
    in begin
      0S a b ∙S x +S 0S a b₁ ∙S x₁
    ≈⟨ <+S> a L ih ih₁ ⟩
      0S a L +S 0S a L
    ≈⟨ identSˡ a L _ ⟩
      0S a L
    ∎) ,
    (let
      open EqReasoning (setoidS a₁ L)
      ih = zeroSˡ a₁ b L x
      ih₁ = zeroSˡ a₁ b₁ L x₁
    in begin
      0S a₁ b ∙S x +S 0S a₁ b₁ ∙S x₁
    ≈⟨ <+S> a₁ L ih ih₁ ⟩
      0S a₁ L +S 0S a₁ L
    ≈⟨ identSˡ a₁ L _ ⟩
      0S a₁ L
    ∎)
  zeroSˡ (B a a₁) (B b b₁) (B c c₁) (Q x x₁ x₂ x₃) =
    (let
      open EqReasoning (setoidS a c)
      ih = zeroSˡ a b c x
      ih₁ = zeroSˡ a b₁ c x₂
    in begin
      0S a b ∙S x +S 0S a b₁ ∙S x₂
    ≈⟨ <+S> a c ih ih₁ ⟩
      0S a c +S 0S a c
    ≈⟨ identSˡ a c _ ⟩
      0S a c
    ∎) ,
    (let
      open EqReasoning (setoidS a c₁)
      ih = zeroSˡ a b c₁ x₁
      ih₁ = zeroSˡ a b₁ c₁ x₃
    in begin
      0S a b ∙S x₁ +S 0S a b₁ ∙S x₃
    ≈⟨ <+S> a c₁ ih ih₁ ⟩
      0S a c₁ +S 0S a c₁
    ≈⟨ identSˡ a c₁ _ ⟩
      0S a c₁
    ∎) ,
    (let
      open EqReasoning (setoidS a₁ c)
      ih = zeroSˡ a₁ b c x
      ih₁ = zeroSˡ a₁ b₁ c x₂
    in begin
      0S a₁ b ∙S x +S 0S a₁ b₁ ∙S x₂
    ≈⟨ <+S> a₁ c ih ih₁ ⟩
      0S a₁ c +S 0S a₁ c
    ≈⟨ identSˡ a₁ c _ ⟩
      0S a₁ c
    ∎) ,
    (let
      open EqReasoning (setoidS a₁ c₁)
      ih = zeroSˡ a₁ b c₁ x₁
      ih₁ = zeroSˡ a₁ b₁ c₁ x₃
    in begin
      0S a₁ b ∙S x₁ +S 0S a₁ b₁ ∙S x₃
    ≈⟨ <+S> a₁ c₁ ih ih₁ ⟩
      0S a₁ c₁ +S 0S a₁ c₁
    ≈⟨ identSˡ a₁ c₁ _ ⟩
      0S a₁ c₁
    ∎)

  zeroSʳ : (a b c : Shape) (x : M s a b) →
    (x ∙S 0S b c) ≃S' 0S a c
  zeroSʳ L L L (One x) = zeroʳ x
  zeroSʳ L L (B c c₁) (One x) =
    (zeroSʳ L L c (One x)) , (zeroSʳ L L c₁ (One x))
  zeroSʳ L (B b b₁) L (Row x x₁) =
    let
      open EqReasoning (setoidS L L)
      ih = zeroSʳ L b L x
      ih₁ = zeroSʳ L b₁ L x₁
    in begin
      Row x x₁ ∙S Col (0S b L) (0S b₁ L)
    ≡⟨ refl-≡ ⟩
      (x ∙S 0S b L) +S (x₁ ∙S 0S b₁ L)
    ≈⟨ <+S> L L {x ∙S 0S b L} {0S L L} {x₁ ∙S 0S b₁ L} {0S L L} ih ih₁ ⟩
      0S L L +S 0S L L
    ≈⟨ identSˡ L L (0S L L) ⟩
      0S L L
    ∎
  zeroSʳ L (B b b₁) (B c c₁) (Row x x₁) =
    (let
      open EqReasoning (setoidS L c)
      ih = zeroSʳ L b c x
      ih₁ = zeroSʳ L b₁ c x₁
    in begin
      x ∙S 0S b c +S x₁ ∙S 0S b₁ c
    ≈⟨ <+S> L c ih ih₁ ⟩
      0S L c +S 0S L c
    ≈⟨ identSˡ L c (0S L c) ⟩
      0S L c
    ∎) ,
    (let
      open EqReasoning (setoidS L c₁)
      ih = zeroSʳ L b c₁ x
      ih₁ = zeroSʳ L b₁ c₁ x₁
    in begin
      x ∙S 0S b c₁ +S x₁ ∙S 0S b₁ c₁
    ≈⟨ <+S> L c₁ ih ih₁ ⟩
      0S L c₁ +S 0S L c₁
    ≈⟨ identSˡ L c₁ (0S L c₁) ⟩
      0S L c₁
    ∎)
  zeroSʳ (B a a₁) L L (Col x x₁) = zeroSʳ a L L x , zeroSʳ a₁ L L x₁
  zeroSʳ (B a a₁) L (B c c₁) (Col x x₁) =
    zeroSʳ a L c x ,
    zeroSʳ a L c₁ x ,
    zeroSʳ a₁ L c x₁ ,
    zeroSʳ a₁ L c₁ x₁
  zeroSʳ (B a a₁) (B b b₁) L (Q x x₁ x₂ x₃) =
    (let
      open EqReasoning (setoidS _ _)
      ih = zeroSʳ a b L x
      ih₁ = zeroSʳ a b₁ L x₁
    in begin
      x ∙S 0S b L +S x₁ ∙S 0S b₁ L
    ≈⟨ <+S> a L ih ih₁ ⟩
      0S a L +S 0S a L
    ≈⟨ identSˡ a L (0S _ _) ⟩
      0S a L
    ∎) ,
    (let
      open EqReasoning (setoidS _ _)
      ih = zeroSʳ a₁ b L x₂
      ih₁ = zeroSʳ a₁ b₁ L x₃
    in begin
      x₂ ∙S 0S b L +S x₃ ∙S 0S b₁ L
    ≈⟨ <+S> a₁ L ih ih₁ ⟩
      0S _ _ +S 0S _ _
    ≈⟨ identSˡ a₁ L (0S _ _) ⟩
      0S a₁ L
    ∎)
  zeroSʳ (B a a₁) (B b b₁) (B c c₁) (Q x x₁ x₂ x₃) =
    (let
      open EqReasoning (setoidS _ _)
      ih = zeroSʳ a b c x
      ih₁ = zeroSʳ a b₁ c x₁
    in begin
      x ∙S 0S b c +S x₁ ∙S 0S b₁ c
    ≈⟨ <+S> a c ih ih₁ ⟩
      0S _ _ +S 0S _ _
    ≈⟨ identSˡ a c (0S _ _) ⟩
      0S _ _
    ∎) ,
    (let
      -- open EqReasoning (setoidS _ _)
      ih = zeroSʳ a b c₁ x
      ih₁ = zeroSʳ a b₁ c₁ x₁
    in transS a c₁ (<+S> a c₁ ih ih₁) (identSˡ a c₁ (0S _ _))
    -- begin
    --   x ∙S 0S b c₁ +S x₁ ∙S 0S b₁ c₁
    -- ≈⟨ <+S> a c₁ ih ih₁ ⟩
    --   0S _ _ +S 0S _ _
    -- ≈⟨ identSˡ a c₁ (0S _ _) ⟩
    --   0S a c₁
    -- ∎
    ) ,
    (let
      open EqReasoning (setoidS _ _)
      ih = zeroSʳ a₁ b c x₂
      ih₁ = zeroSʳ a₁ b₁ c x₃
    in begin
      x₂ ∙S 0S b c +S x₃ ∙S 0S b₁ c
    ≈⟨ <+S> a₁ c ih ih₁ ⟩
      0S _ _ +S 0S _ _
    ≈⟨ identSˡ a₁ c (0S _ _) ⟩
      0S a₁ c
    ∎) ,
    (let
      open EqReasoning (setoidS _ _)
      ih = zeroSʳ a₁ b c₁ x₂
      ih₁ = zeroSʳ a₁ b₁ c₁ x₃
    in begin
      x₂ ∙S 0S b c₁ +S x₃ ∙S 0S b₁ c₁
    ≈⟨ <+S> a₁ c₁ ih ih₁ ⟩
      0S _ _ +S 0S _ _
    ≈⟨ identSˡ a₁ c₁ (0S _ _) ⟩
      0S a₁ c₁
    ∎)

  <∙S> : (a b c : Shape) {x y : M s a b} {u v : M s b c} →
    x ≃S' y → u ≃S' v → (x ∙S u) ≃S' (y ∙S v)
  <∙S> L L L {One x} {One x₁} {One x₂} {One x₃} p q = p <∙> q
  <∙S> L L (B c c₁) {One x} {One x₁} {Row u u₁} {Row v v₁} p (q , q₁) =
    (<∙S> L L c {One x} {One x₁} {u} {v} p q) ,
    <∙S> L L c₁ {One x} {One x₁} {u₁} {v₁} p q₁
  <∙S> L (B b b₁) L {Row x x₁} {Row y y₁} {Col u u₁} {Col v v₁} (p , p₁) (q , q₁) =
    -- Row x x₁ ∙S Col u u₁ ≃S' Row y y₁ ∙S Col v v₁
    let
      open EqReasoning (setoidS _ _)
      ih = <∙S> _ _ _ {x} {y} {u} {v} p q
      ih₁ = <∙S> _ _ _ {x₁} {y₁} {u₁} {v₁} p₁ q₁
    in begin
      Row x x₁ ∙S Col u u₁
    ≡⟨ refl-≡ ⟩
      x ∙S u +S x₁ ∙S u₁
    ≈⟨ <+S> L L {x ∙S u} {y ∙S v} {x₁ ∙S u₁} {y₁ ∙S v₁} ih ih₁ ⟩
      y ∙S v +S y₁ ∙S v₁
    ∎
  <∙S> L (B b b₁) (B c c₁) {Row x x₁} {Row y y₁} {Q u u₁ u₂ u₃} {Q v v₁ v₂ v₃} (p , p₁) (q , q₁ , q₂ , q₃) =
    {!!} ,
    {!!}
  <∙S> (B a a₁) L L {Col x x₁} {Col y y₁} {One x₂} {One x₃} (p , p₁) q =
    {!!} ,
    {!!}
  <∙S> (B a a₁) L (B c c₁) {Col x x₁} {Col y y₁} {Row u u₁} {Row v v₁} (p , p₁) (q , q₁) =
    {!!} ,
    {!!} ,
    {!!} ,
    {!!}
  <∙S> (B a a₁) (B b b₁) L {Q x x₁ x₂ x₃} {Q y y₁ y₂ y₃} {Col u u₁} {Col v v₁} (p , p₁ , p₂ , p₃) (q , q₁) =
    {!!} ,
    {!!}
  <∙S> (B a a₁) (B b b₁) (B c c₁) {Q x x₁ x₂ x₃} {Q y y₁ y₂ y₃} {Q u u₁ u₂ u₃} {Q v v₁ v₂ v₃} (p , p₁ , p₂ , p₃) (q , q₁ , q₂ , q₃) =
    {!!}

  idemS : (r c : Shape) (x : M s r c) → x +S x ≃S' x
  idemS L L (One x) = idem x
  idemS L (B c c₁) (Row x x₁) = (idemS _ _ x) , (idemS _ _ x₁)
  idemS (B r r₁) L (Col x x₁) = (idemS _ _ x) , (idemS _ _ x₁)
  idemS (B r r₁) (B c c₁) (Q x x₁ x₂ x₃) =
    (idemS _ _ x) , (idemS _ _ x₁ , (idemS _ _ x₂  , idemS _ _ x₃))

  SNR : SemiNearRing
  SNR =
    record
      { s = S
      ; _≃ₛ_ = ≃S shape shape
      ; 0ₛ = 0S shape shape
      ; _+ₛ_ = _+S_
      ; _∙ₛ_ = _∙S_
      ; isCommMon = isCommMonS
      ; zeroˡ = zeroSˡ shape shape shape
      ; zeroʳ = zeroSʳ shape shape shape
      ; _<∙>_ = <∙S> shape shape shape
      ; idem = idemS shape shape
      ; distl = {!!}
      ; distr = {!!}
      }
