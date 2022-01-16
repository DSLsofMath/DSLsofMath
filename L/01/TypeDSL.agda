-- A DSL example in the language Agda: "polynomial types"
module TypeDSL where
open import Data.Empty
open import Data.Unit
open import Data.Sum
open import Data.Product
open import Data.Nat

data E : Set1 where
  Add   : E -> E -> E
  Mul   : E -> E -> E
  Zero  : E
  One   : E

eval : E -> Set
eval (Add x y)  = (eval x) ⊎ (eval y)
eval (Mul x y)  = (eval x) × (eval y)
eval Zero       = ⊥
eval One        = ⊤

two   = Add One One
three = Add One two

test1 : eval One
test1 = tt

false : eval two
false = inj₁ tt
true : eval two
true = inj₂ tt

test3 : eval three
test3 = inj₁ tt

card : E -> ℕ
card (Add x y)  = card x + card y
card (Mul x y)  = card x * card y
card Zero       = 0
card One        = 1

open import Data.Vec as V

variable
    m n : ℕ
    A B : Set

enumAdd : Vec A m -> Vec B n -> Vec (A ⊎ B) (m + n)
enumAdd xs ys = V.map inj₁ xs ++ V.map inj₂ ys

-- cartesianProduct
enumMul : Vec A m → Vec B n → Vec (A × B) (m * n)
enumMul xs ys = concat (V.map  (\a -> V.map  ((a ,_)) ys)  xs)

enumerate : (t : E) -> Vec (eval t) (card t)
enumerate (Add x y)  =  enumAdd  (enumerate x) (enumerate y)
enumerate (Mul x y)  =  enumMul  (enumerate x) (enumerate y) 
enumerate Zero       =  []
enumerate One        =  [ tt ]

test : Vec (eval three) 3
test = enumerate three
-- inj₁ tt ∷ inj₂ (inj₁ tt) ∷ inj₂ (inj₂ tt) ∷ []

-- Exercise: add a constructor for function types to the syntax E 
