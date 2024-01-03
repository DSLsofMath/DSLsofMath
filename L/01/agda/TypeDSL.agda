-- A DSL example in the language Agda: "polynomial types"
module TypeDSL where
open import Data.Nat using (ℕ;_+_;_*_)

data E : Set where
  Zero  : E
  One   : E
  Add   : (x : E) -> (y : E) -> E
  Mul   : (x : E) -> (y : E) -> E
-- Exercise: add Pow x y ~=  x^y  ~  y -> x

two : E
two = Add One One

four : E
four = Mul two two

-- First semantics: compute the natural number "value" of the expression
card : E -> ℕ
card Zero = 0
card One = 1
card (Add x y) = card x + card y
card (Mul x y) = card x * card y

data Empty : Set where
data Unit : Set where unit : Unit
data Either (a : Set) (b : Set) : Set where
  Left  : a -> Either a b
  Right : b -> Either a b
data Both (a : Set) (b : Set) : Set where
  _,_ : a -> b -> Both a b

-- Second semantics: compute the corresponding finite type
typ : E -> Set
typ Zero = Empty
typ One = Unit
typ (Add x y) = Either (typ x) (typ y)   -- disjoint union type    = sum type
typ (Mul x y) = Both   (typ x) (typ y)   -- cartesian product type = pair type

Bool : Set
Bool = typ two

false : Bool
false = Left unit
true : Bool
true = Right unit

BothBoolBool : Set
BothBoolBool = typ four

ex1 : BothBoolBool
ex1 = ( false , true )

open import Data.Vec as V

variable
  a b : Set
  m n : ℕ

enumAdd : Vec a m -> Vec b n -> Vec (Either a b) (m + n)
enumAdd as bs = V.map Left as ++ V.map Right bs

enumMul : Vec a m -> Vec b n -> Vec (Both   a b) (m * n)
enumMul as bs = concat (V.map (\ a -> V.map (\ b -> (a , b)) bs) as)

-- Third semantics: enumerate all the values in a vector
enumerate : (t : E) -> Vec (typ t) (card t)
enumerate Zero = []
enumerate One = [ unit ]
enumerate (Add x y) = enumAdd (enumerate x) (enumerate y)
enumerate (Mul x y) = enumMul (enumerate x) (enumerate y)

test2 : Vec (typ two) (card two) -- Vec Bool 2
test2 = enumerate two
  -- false ∷ true ∷ []

-- eqCheck : (t : E) -> (e1 e2 : typ t) -> Bool
-- another exercise

test4 : Vec (typ four) (card four) -- Vec BothBoolBool 4
test4 = enumerate four
{-
(false , false) ∷
(false , true) ∷
(true , false) ∷
(true , true) ∷ []
-}

-- Exercise: add a constructor for function types
