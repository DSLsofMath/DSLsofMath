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


-- Lifting a SNR to a to a Square matrix of some shape
Square : SemiNearRing → Shape → SemiNearRing
Square snr shape = SNR
  where
  open SemiNearRing snr

  S = Sq s shape

  infixr 60 _∙S_
  infixr 50 _+S_


  _+S_ : ∀ {r c} → M s r c → M s r c → M s r c
  One x     +S One x₁    = One (x +ₛ x₁)
  Row m m₁  +S Row n n₁  = Row (m +S n) (m₁ +S n₁)
  Col m m₁  +S Col n n₁  = Col (m +S n) (m₁ +S n₁)
  Q m00 m01
    m10 m11 +S Q n00 n01
                n10 n11 = Q (m00 +S n00) (m01 +S n01)
                            (m10 +S n10) (m11 +S n11)

  _∙S_ : ∀ {r m c} → M s r m → M s m c → M s r c
  One x     ∙S One x₁    = One (x ∙ₛ x₁)
  One x     ∙S Row n n₁  = Row (One x ∙S n) (One x ∙S n₁)
  Row m m₁  ∙S Col n n₁  = m ∙S n +S m₁ ∙S n₁
  Row m m₁  ∙S Q n00 n01
                n10 n11 = Row (m ∙S n00 +S m₁ ∙S n10) (m ∙S n01 +S m₁ ∙S n11)
  Col m m₁  ∙S One x     = Col (m ∙S One x) (m₁ ∙S One x)
  Col m m₁  ∙S Row n n₁  = Q (m ∙S n)   (m ∙S n₁)
                            (m₁ ∙S n)  (m₁ ∙S n₁)
  Q m00 m01
    m10 m11 ∙S Col n n₁  = Col (m00 ∙S n +S m01 ∙S n₁) (m10 ∙S n +S m11 ∙S n₁)
  Q m00 m01
    m10 m11 ∙S Q n00 n01
                n10 n11 = Q (m00 ∙S n00 +S m01 ∙S n10) (m00 ∙S n01 +S m01 ∙S n11)
                            (m10 ∙S n00 +S m11 ∙S n10) (m10 ∙S n01 +S m11 ∙S n11)

  0S : (r c : Shape) → M s r c
  0S L L = One 0ₛ
  0S L (B s s₁) = Row (0S L s) (0S L s₁)
  0S (B r r₁) L = Col (0S r L) (0S r₁ L)
  0S (B r r₁) (B s s₁) =
      Q (0S r s) (0S r s₁)
        (0S r₁ s) (0S r₁ s₁)

  _≃S_ : {r c : Shape} →
        M s r c → M s r c → Set
  _≃S_ {L} {L} (One x) (One x₁) = x ≃ₛ x₁
  _≃S_ {L} {(B c₁ c₂)} (Row m m₁) (Row n n₁) =
    _≃S_ m n × _≃S_ m₁ n₁
  _≃S_ {(B r₁ r₂)} {L} (Col m m₁) (Col n n₁) =
    _≃S_ m n × _≃S_ m₁ n₁
  _≃S_ {(B r₁ r₂)} {(B c₁ c₂)} (Q m00 m01 m10 m11) (Q n00 n01 n10 n11) =
    _≃S_ m00 n00 ×
    _≃S_ m01 n01 ×
    _≃S_ m10 n10 ×
    _≃S_ m11 n11


  reflS : (r c : Shape) →
    {X : M s r c} → X ≃S X
  reflS L L {X = One x} = reflₛ {x}
  reflS L (B c₁ c₂) {X = Row X Y} = reflS L c₁ , reflS L c₂
  reflS (B r₁ r₂) L {X = Col X Y} = reflS r₁ L , reflS r₂ L
  reflS (B r₁ r₂) (B c₁ c₂) {X = Q X Y Z W} =
    reflS r₁ c₁ , reflS r₁ c₂ ,
    reflS r₂ c₁ , reflS r₂ c₂

  symS : (r c : Shape) →
    {i j : M s r c} → i ≃S j → j ≃S i
  symS L L {One x} {One x₁} p = symₛ p
  symS L (B c₁ c₂) {Row i₁ i₂} {Row j₁ j₂} (p , q) = symS L c₁ p , symS L c₂ q
  symS (B r₁ r₂) L {Col i₁ i} {Col j j₁} (p , q) = symS r₁ L p , symS r₂ L q
  symS (B r r₂) (B c₁ c₂) {Q i₂ i i₃ i₁} {Q j j₁ j₂ j₃} (p , q , x , y) =
    symS r c₁ p , symS r c₂ q ,
    symS r₂ c₁ x , symS r₂ c₂ y

  transS : (r c : Shape) →
    {i j k : M s r c} → i ≃S j → j ≃S k → i ≃S k
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

  assocS : (r c : Shape) (x y z : M s r c) → ((x +S y) +S z) ≃S (x +S (y +S z))
  assocS L L (One x) (One y) (One z) = assocₛ x y z
  assocS L (B c c₁) (Row x x₁) (Row y y₁) (Row z z₁) =
    assocS L c x y z  , assocS L c₁ x₁ y₁ z₁
  assocS (B r r₁) L (Col x x₁) (Col y y₁) (Col z z₁) =
    assocS r L x y z , assocS r₁ L x₁ y₁ z₁
  assocS (B r r₁) (B c c₁) (Q x x₁ x₂ x₃) (Q y y₁ y₂ y₃) (Q z z₁ z₂ z₃) =
    (assocS r c x y z) , (assocS r c₁ x₁ y₁ z₁) ,
    (assocS r₁ c x₂ y₂ z₂) , (assocS r₁ c₁ x₃ y₃ z₃)

  <+S> : (r c : Shape) {x y u v : M s r c} →
    x ≃S y → u ≃S v → (x +S u) ≃S (y +S v)
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
      ; assoc = assocS shape shape
      ; ∙-cong = <+S> shape shape }


  identSˡ : (r c : Shape) (x : M s r c) →
     0S r c +S x ≃S x
  identSˡ L L (One x) = identityˡₛ x
  identSˡ L (B c c₁) (Row x x₁) = identSˡ L c x , identSˡ L c₁ x₁
  identSˡ (B r r₁) L (Col x x₁) = identSˡ r L x , identSˡ r₁ L x₁
  identSˡ (B r r₁) (B c c₁) (Q x x₁ x₂ x₃) =
    identSˡ r c x , identSˡ r c₁ x₁ ,
    identSˡ r₁ c x₂ , identSˡ r₁ c₁ x₃

  commS : (r c : Shape) → (x y : M s r c) →
    (x +S y) ≃S (y +S x)
  commS L L (One x) (One x₁) = commₛ x x₁
  commS L (B c c₁) (Row x x₁) (Row y y₁) = (commS L c x y) , (commS L c₁ x₁ y₁)
  commS (B r r₁) L (Col x x₁) (Col y y₁) = (commS r L x y) , (commS r₁ L x₁ y₁)
  commS (B r r₁) (B c c₁) (Q x x₁ x₂ x₃) (Q y y₁ y₂ y₃) =
    commS r c x y , commS r c₁ x₁ y₁ ,
    commS r₁ c x₂ y₂ , commS r₁ c₁ x₃ y₃

  isCommMonS =
    record
      { isSemigroup = isSemgroupS
      ; identityˡ = identSˡ shape shape
      ; comm = commS shape shape }


  setoidS : {r c : Shape} → Setoid _ _
  setoidS {r} {c} =
    record
      { Carrier = M s r c
      ; _≈_ = _≃S_ {r} {c}
      ; isEquivalence =
        record
          { refl = reflS r c ; sym = symS r c ; trans = transS r c } }


  zeroSˡ : (a b c : Shape) (x : M s b c) →
    (0S a b ∙S x) ≃S 0S a c
  zeroSˡ L L L (One x) = zeroˡ x
  zeroSˡ L L (B c c₁) (Row x x₁) = (zeroSˡ L L c x) , (zeroSˡ L L c₁ x₁)
  zeroSˡ L (B b b₁) L (Col x x₁) =
    let
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
    (x ∙S 0S b c) ≃S 0S a c
  zeroSʳ L L L (One x) = zeroʳ x
  zeroSʳ L L (B c c₁) (One x) =
    (zeroSʳ L L c (One x)) , (zeroSʳ L L c₁ (One x))
  zeroSʳ L (B b b₁) L (Row x x₁) =
    let
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
      ih = zeroSʳ a b c₁ x
      ih₁ = zeroSʳ a b₁ c₁ x₁
    in transS a c₁ (<+S> a c₁ ih ih₁) (identSˡ a c₁ (0S _ _))
    ) ,
    (let
      open EqReasoning setoidS
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
      open EqReasoning setoidS
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
    x ≃S y → u ≃S v → (x ∙S u) ≃S (y ∙S v)
  <∙S> L L L {One x} {One x₁} {One x₂} {One x₃} p q = p <∙> q
  <∙S> L L (B c c₁) {One x} {One x₁} {Row u u₁} {Row v v₁} p (q , q₁) =
    (<∙S> L L c {One x} {One x₁} {u} {v} p q) ,
    <∙S> L L c₁ {One x} {One x₁} {u₁} {v₁} p q₁
  <∙S> L (B b b₁) L {Row x x₁} {Row y y₁} {Col u u₁} {Col v v₁} (p , p₁) (q , q₁) =
    -- Row x x₁ ∙S Col u u₁ ≃S Row y y₁ ∙S Col v v₁
    let
      open EqReasoning setoidS
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
    (let
      ih = <∙S> L b c p q
      ih₁ = <∙S> L b₁ c p₁ q₂
    in <+S> L c ih ih₁) ,
    <+S> L c₁ (<∙S> L b c₁ p q₁) (<∙S> L b₁ c₁ p₁ q₃)
  <∙S> (B a a₁) L L {Col x x₁} {Col y y₁} {One x₂} {One x₃} (p , p₁) q =
    <∙S> a L L p q ,
    <∙S> a₁ L L p₁ q
  <∙S> (B a a₁) L (B c c₁) {Col x x₁} {Col y y₁} {Row u u₁} {Row v v₁} (p , p₁) (q , q₁) =
    <∙S> a L c p q ,
    <∙S> a L c₁ p q₁  ,
    <∙S> a₁ L c p₁ q ,
    <∙S> a₁ L c₁ p₁ q₁
  <∙S> (B a a₁) (B b b₁) L {Q x x₁ x₂ x₃} {Q y y₁ y₂ y₃} {Col u u₁} {Col v v₁} (p , p₁ , p₂ , p₃) (q , q₁) =
    <+S> a L (<∙S> a b L p q) (<∙S> a b₁ L p₁ q₁) ,
    <+S> a₁ L (<∙S> a₁ b L p₂ q) (<∙S> a₁ b₁ L p₃ q₁ )
  <∙S> (B a a₁) (B b b₁) (B c c₁) {Q x x₁ x₂ x₃} {Q y y₁ y₂ y₃} {Q u u₁ u₂ u₃} {Q v v₁ v₂ v₃} (p , p₁ , p₂ , p₃) (q , q₁ , q₂ , q₃) =
    <+S> a c (<∙S> a b c p q) (<∙S> a b₁ c p₁ q₂) ,
    <+S> a c₁ (<∙S> a b c₁ p q₁) (<∙S> a b₁ c₁ p₁ q₃) ,
    <+S> a₁ c (<∙S> a₁ b c p₂ q) (<∙S> a₁ b₁ c p₃ q₂) ,
    <+S> a₁ c₁ (<∙S> a₁ b c₁ p₂ q₁) (<∙S> a₁ b₁ c₁ p₃ q₃)

  idemS : (r c : Shape) (x : M s r c) → x +S x ≃S x
  idemS L L (One x) = idem x
  idemS L (B c c₁) (Row x x₁) = (idemS _ _ x) , (idemS _ _ x₁)
  idemS (B r r₁) L (Col x x₁) = (idemS _ _ x) , (idemS _ _ x₁)
  idemS (B r r₁) (B c c₁) (Q x x₁ x₂ x₃) =
    (idemS _ _ x) , (idemS _ _ x₁ , (idemS _ _ x₂  , idemS _ _ x₃))

  swapMid : {r c : Shape} (x y z w : M s r c) →
    (x +S y) +S (z +S w) ≃S (x +S z) +S (y +S w)
  swapMid {r} {c} x y z w =
    let open EqReasoning setoidS
    in begin
      (x +S y) +S (z +S w)
    ≈⟨ assocS _ _ x y (z +S w) ⟩
      x +S y +S z +S w
    ≈⟨ <+S> r c (reflS r c) (symS r c (assocS r c y z w)) ⟩
      x +S (y +S z) +S w
    ≈⟨ <+S> r c (reflS r c) (<+S> r c (commS r c y z) (reflS r c)) ⟩
      x +S (z +S y) +S w
    ≈⟨ <+S> r c (reflS r c) (assocS r c z y w) ⟩
      x +S z +S y +S w
    ≈⟨ symS r c (assocS r c x z (y +S w)) ⟩
      (x +S z) +S (y +S w)
    ∎

  distlHelp : ∀ {a b b₁ c₁}
              (x : M s a b)
              (y z : M s b c₁)
              (x₁ : M s a b₁)
              (y₁ z₁ : M s b₁ c₁) →
            (x ∙S (y +S z)) ≃S (x ∙S y +S x ∙S z) →
            (x₁ ∙S (y₁ +S z₁)) ≃S (x₁ ∙S y₁ +S x₁ ∙S z₁) →
            (x ∙S (y +S z) +S x₁ ∙S (y₁ +S z₁))
            ≃S ((x ∙S y +S x₁ ∙S y₁) +S x ∙S z +S x₁ ∙S z₁)
  distlHelp x y z x₁ y₁ z₁ p q =
    let open EqReasoning setoidS
    in begin
      x ∙S (y +S z) +S x₁ ∙S (y₁ +S z₁)
    ≈⟨ <+S> _ _ {x ∙S (y +S z)} {x ∙S y +S x ∙S z}
                {x₁ ∙S (y₁ +S z₁)} {x₁ ∙S y₁ +S x₁ ∙S z₁} p q ⟩
      (x ∙S y +S x ∙S z) +S x₁ ∙S y₁ +S x₁ ∙S z₁
    ≈⟨ swapMid (x ∙S y) (x ∙S z) (x₁ ∙S y₁) (x₁ ∙S z₁) ⟩
      (x ∙S y +S x₁ ∙S y₁) +S x ∙S z +S x₁ ∙S z₁
    ∎

  distlS : {a b c : Shape} (x : M s a b) (y z : M s b c) →
    (x ∙S (y +S z)) ≃S ((x ∙S y) +S (x ∙S z))
  distlS {L} {L} {L} (One x) (One y) (One z) = distl x y z
  distlS {L} {L} {B c c₁} (One x) (Row y y₁) (Row z z₁) =
    distlS (One x) y z ,
    distlS (One x) y₁ z₁
  distlS {L} {(B b b₁)} {L} (Row x x₁) (Col y y₁) (Col z z₁) =
    distlHelp x y z x₁ y₁ z₁ (distlS x y z) (distlS x₁ y₁ z₁)
  distlS {L} {(B b b₁)} {(B c c₁)} (Row x x₁) (Q y y₁ y₂ y₃) (Q z z₁ z₂ z₃) =
    distlHelp x y z x₁ y₂ z₂ (distlS x y z) (distlS x₁ y₂ z₂) ,
    distlHelp x y₁ z₁ x₁ y₃ z₃  (distlS x y₁ z₁) (distlS x₁ y₃ z₃)
  distlS {(B a a₁)} {L} {L} (Col x x₁) (One x₂) (One x₃) =
    distlS x (One x₂) (One x₃) ,
    distlS x₁ (One x₂) (One x₃)
  distlS {(B a a₁)} {L} {(B c c₁)} (Col x x₁) (Row y y₁) (Row z z₁) =
    distlS x y z ,
    distlS x y₁ z₁ ,
    distlS x₁ y z ,
    distlS x₁ y₁ z₁
  distlS {(B a a₁)} {(B b b₁)} {L} (Q x x₁ x₂ x₃) (Col y y₁) (Col z z₁) =
    distlHelp x y z x₁ y₁ z₁ (distlS x y z) (distlS x₁ y₁ z₁) ,
    distlHelp x₂ y z x₃ y₁ z₁ (distlS x₂ y z) (distlS x₃ y₁ z₁)
  distlS {(B a a₁)} {(B b b₁)} {(B c c₁)} (Q x x₁ x₂ x₃) (Q y y₁ y₂ y₃) (Q z z₁ z₂ z₃) =
    distlHelp x y z x₁ y₂ z₂ (distlS x y z) (distlS x₁ y₂ z₂) ,
    distlHelp x y₁ z₁ x₁ y₃ z₃ (distlS x y₁ z₁) (distlS x₁ y₃ z₃) ,
    distlHelp x₂ y z x₃ y₂ z₂ (distlS x₂ y z) (distlS x₃ y₂ z₂)  ,
    distlHelp x₂ y₁ z₁ x₃ y₃ z₃ (distlS x₂ y₁ z₁) (distlS x₃ y₃ z₃)

  distrHelp : ∀ {r m m₁ c : Shape}
              (x : M s m c)
              (y z : M s r m)
              (x₁ : M s m₁ c)
              (y₁ z₁ : M s r m₁) →
            ((y +S z) ∙S x) ≃S (y ∙S x +S z ∙S x) →
            ((y₁ +S z₁) ∙S x₁) ≃S (y₁ ∙S x₁ +S z₁ ∙S x₁) →
            ((y +S z) ∙S x +S (y₁ +S z₁) ∙S x₁)
            ≃S ((y ∙S x +S y₁ ∙S x₁) +S z ∙S x +S z₁ ∙S x₁)
  distrHelp x y z x₁ y₁ z₁ p q =
    let open EqReasoning setoidS
    in begin
      (y +S z) ∙S x +S (y₁ +S z₁) ∙S x₁
    ≈⟨ <+S> _ _ {(y +S z) ∙S x} {y ∙S x +S z ∙S x}
                {(y₁ +S z₁) ∙S x₁} {y₁ ∙S x₁ +S z₁ ∙S x₁} p q ⟩
      (y ∙S x +S z ∙S x) +S y₁ ∙S x₁ +S z₁ ∙S x₁
    ≈⟨ swapMid (y ∙S x) (z ∙S x) (y₁ ∙S x₁) (z₁ ∙S x₁) ⟩
      (y ∙S x +S y₁ ∙S x₁) +S z ∙S x +S z₁ ∙S x₁
    ∎

  distrS : {r m c : Shape} (x : M s m c) (y z : M s r m) →
    ((y +S z) ∙S x) ≃S ((y ∙S x) +S (z ∙S x))
  distrS {L} {L} {L} (One x) (One y) (One z) =
    distr x y z
  distrS {L} {L} {B c c₁} (Row x x₁) (One x₂) (One x₃) =
    (distrS x (One x₂) (One x₃)) ,
    (distrS x₁ (One x₂) (One x₃))
  distrS {L} {B m m₁} {L} (Col x x₁) (Row y y₁) (Row z z₁) =
    distrHelp x y z x₁ y₁ z₁ (distrS x y z) (distrS x₁ y₁ z₁)
  distrS {L} {B m m₁} {B c c₁} (Q x x₁ x₂ x₃) (Row y y₁) (Row z z₁) =
    (distrHelp x y z x₂ y₁ z₁ (distrS x y z) (distrS x₂ y₁ z₁)) ,
    (distrHelp x₁ y z x₃ y₁ z₁ (distrS x₁ y z) (distrS x₃ y₁ z₁))
  distrS {B r r₁} {L} {L} (One x) (Col y y₁) (Col z z₁) =
    distrS (One x) y z ,
    distrS (One x) y₁ z₁
  distrS {B r r₁} {L} {B c c₁} (Row x x₁) (Col y y₁) (Col z z₁) =
    (distrS x y z) ,
    (distrS x₁ y z) ,
    (distrS x y₁ z₁) ,
    (distrS x₁ y₁ z₁)
  distrS {B r r₁} {B m m₁} {L} (Col x x₁) (Q y y₁ y₂ y₃) (Q z z₁ z₂ z₃) =
    (distrHelp x y z x₁ y₁ z₁ (distrS x y z) (distrS x₁ y₁ z₁)) ,
    (distrHelp x y₂ z₂ x₁ y₃ z₃ (distrS x y₂ z₂) (distrS x₁ y₃ z₃))
  distrS {B r r₁} {B m m₁} {B c c₁} (Q x x₁ x₂ x₃) (Q y y₁ y₂ y₃) (Q z z₁ z₂ z₃) =
    distrHelp x y z x₂ y₁ z₁ (distrS x y z) (distrS x₂ y₁ z₁) ,
    distrHelp x₁ y z x₃ y₁ z₁ (distrS x₁ y z) (distrS x₃ y₁ z₁) ,
    distrHelp x y₂ z₂ x₂ y₃ z₃ (distrS x y₂ z₂) (distrS x₂ y₃ z₃) ,
    distrHelp x₁ y₂ z₂ x₃ y₃ z₃ (distrS x₁ y₂ z₂) (distrS x₃ y₃ z₃)


  SNR : SemiNearRing
  SNR =
    record
      { s = S
      ; _≃ₛ_ = _≃S_ {shape} {shape}
      ; 0ₛ = 0S shape shape
      ; _+ₛ_ = _+S_
      ; _∙ₛ_ = _∙S_
      ; isCommMon = isCommMonS
      ; zeroˡ = zeroSˡ shape shape shape
      ; zeroʳ = zeroSʳ shape shape shape
      ; _<∙>_ = <∙S> shape shape shape
      ; idem = idemS shape shape
      ; distl = distlS {shape} {shape}
      ; distr = distrS {shape} {shape}
      }
