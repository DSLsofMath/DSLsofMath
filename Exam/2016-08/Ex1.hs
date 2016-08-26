{-# LANGUAGE GADTs #-}

{-
An *abelian monoid* is a set M together with a constant (nullary
operation) zero : M and a binary operation oplus : M -> M -> M such that:

* zero is a unit of oplus
* oplus is associative
* oplus is commutative
-}

-- i. Define a type class AbMonoid that corresponds to the abelian monoid structure

class AbMonoid m where
  zero   :: m
  oplus  :: m -> m -> m

-- ii. Define a datastructure AbMonoidExp ... and define an AbMonoid instance ...

data AbMonoidExp v where
  Zero   :: AbMonoidExp v
  Oplus  :: AbMonoidExp v -> AbMonoidExp v -> AbMonoidExp v
  Var    :: v -> AbMonoidExp v

instance AbMonoid (AbMonoidExp v) where
  zero   = Zero
  oplus  = Oplus

-- iii. Find one other instance ... and a non-instance ...

instance AbMonoid Int where
  zero   = 0
  oplus  = (+)

-- The empty type Void has no instance (because it has no zero)

-- iv. Define a general evaluator for AbMonoidExp expressions on the
-- basis of an assignment function.

eval :: AbMonoid m => (v -> m) -> AbMonoidExp v -> m
eval f Zero         =  zero
eval f (Oplus l r)  =  oplus (eval f l) (eval f r)
eval f (Var v)      =  f v

-- v. Specialise the evaluator ... three AbMonoidExp expressions ...

evalAME :: (v -> Int) -> AbMonoidExp v -> Int
evalAME = eval

e1' :: v -> AbMonoidExp v
e1' x      = Oplus (Var x) Zero
e2' x y z  = Oplus (Var x) (Oplus (Var y) (Var z))
e3' x y    = Oplus (Var x) (Var y)

e1, e2, e3 :: AbMonoidExp String
e1 = e1' "x"
e2 = e2' "x" "y" "z"
e3 = e3' "x" "y"

assInt :: String -> Int
assInt "x" = 1
assInt "y" = 2
assInt "z" = 4

test =    evalAME assInt e1 == 1
       && evalAME assInt e2 == 7
       && evalAME assInt e3 == 3
