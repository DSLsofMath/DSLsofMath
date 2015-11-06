-- Matrices indexed by shape
module SquareMatrix where

open import Shape
open import Matrix

-- Square matrices have the same shape in both dimensions
Sq : Set → S → Set
Sq type shape = M type shape shape
