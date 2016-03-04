{-# LANGUAGE FlexibleInstances, FlexibleContexts, Rank2Types #-}
module Ring where

import Test.QuickCheck


-- 1. Type class for the ring structure
class Ring v where
  add     :: v -> v -> v
  addId   :: v
  addInv  :: v -> v

  mul     :: v -> v -> v
  mulId   :: v


{- Not part of the exercise, still useful: properties a 'Ring' class type
   instance should satisfy. Expressed so as to be testable via QuickCheck -}

-- * Unnecessary shorthand; convenient but obscuring.
-- NOTE: the 'Eq' constraint is only necessary due to the way in which we have
--  defined the properties - alternate ways (with additional parameters) of
--  defining these are possible in order to make these properties also
--  applicable  to, for instance, functions (cfr: an instance later)
type MonoP a = (Ring a, Eq a) => a -> Bool
type BinP a  = (Ring a, Eq a) => a -> a -> Bool
type TernP a = (Ring a, Eq a) => a -> a -> a -> Bool

-- * General, parameterised operation and element properties
prop_id           o   e x     = x `o` e == x && e `o` x == x
prop_assoc        o     x y z = o (o x y) z == o x (o y z)
prop_commutative  o     x y   = o x y == o y x
prop_inv          o r e x     = o x (r x) == e && o (r x) x == e
prop_distr        p q   x y z = p x (y `q` z) == p x y `q` p x z &&
                                p (x `q` y) z == p x z `q` p y z

-- * Concrete properties for the 'Ring' class
prop_addId, prop_mulId, prop_addInv :: MonoP a
prop_addId        = prop_id  add        addId
prop_mulId        = prop_id  mul        mulId
prop_addInv       = prop_inv add addInv addId

prop_addAssoc, prop_mulAssoc, prop_mulAddDistr :: TernP a
prop_addAssoc     = prop_assoc add
prop_mulAssoc     = prop_assoc mul
prop_mulAddDistr  = prop_distr mul add

prop_addCommu :: BinP a
prop_addCommu = prop_commutative add



-- 2. Datatype for the language of ring expressions and its 'Ring' instance
data RExpr v = AddId
             | MulId
             | Add (RExpr v) (RExpr v)
             | Mul (RExpr v) (RExpr v)
             | AddInv (RExpr v)
             | V v
  deriving (Show, Eq)


instance Ring (RExpr v) where
  addId   = AddId
  mulId   = MulId

  add     = Add
  mul     = Mul

  addInv  = AddInv



-- 3. Some other instances of the Ring class

-- * Integers form a ring structure with the usual addition and multiplication
-- operations: one would hope the given instance of 'Num' for 'Integer' in
-- Haskell, from which we take the operations and neutral elements, actually
-- respects the (usual) properties a ring calls for.
instance Ring Integer where
  addId = 0
  mulId = 1

  add = (+)
  mul = (*)

  addInv = negate


-- * The set of functions whose codomain is a ring forms a ring
instance Ring a => Ring (x -> a) where
  addId = const addId
  mulId = const mulId

  add f g = \a -> f a `add` g a
  mul f g = \a -> f a `mul` g a

  addInv f = \a -> addInv $ f a


-- * 2-by-2 matrices over a ring form a ring
type Mx2 a = ((a,a), (a,a))

instance Ring a => Ring (Mx2 a) where
  addId = repMx2 addId
  mulId = ((mulId,addId),(addId,mulId))

  add = addComp (addComp add)
    where addComp p (x,y) (u,v) = (p x u, p y v)
  mul ((x,y),(u,v)) ((a,b),(c,d)) = mapMx2 (uncurry add . pairM1 (uncurry mul))
    (( ((x,a), (y,c)) , ((x,b), (y,d)) ),
     ( ((u,a), (v,c)) , ((u,b), (v,d)) ))

  addInv = mapMx2 addInv

-- Helper functions
repMx2 :: a -> Mx2 a
repMx2 i = ((i,i), (i,i))

pairM1 :: (a -> b) -> (a,a) -> (b,b)
pairM1 f (x,y) = (f x, f y)

mapMx2 :: (a -> b) -> Mx2 a -> Mx2 b
mapMx2 = pairM1 . pairM1



-- 4. A general evaluator for ring expressions, given an assignment function
evalR :: Ring a => (v -> a) -> RExpr v -> a
evalR f AddId       = addId
evalR f MulId       = mulId
evalR f (Add a b)   = add (evalR f a) (evalR f b)
evalR f (Mul a b)   = mul (evalR f a) (evalR f b)
evalR f (AddInv a)  = addInv (evalR f a)
evalR f (V x)       = f x



-- 5. Specialisation (ie: "instantiation") of our general evaluator to the other
--    'Ring' instances
evalRI :: (v -> Integer) -> RExpr v -> Integer
evalRI = evalR

evalRF :: Ring a => (v -> (x -> a)) -> RExpr v -> x -> a
evalRF = evalR

evalRMx2 :: Ring a => (v -> Mx2 a) -> RExpr v -> Mx2 a
evalRMx2 = evalR


-- * Some example expressions
type RVar = String

-- Different ways of writing expressions of the same type
examples :: [ RExpr RVar ]
examples = [ Add (AddInv ß) $ Mul (Add µ MulId) µ
           , AddInv $ foldl Mul MulId [ Add AddId ß, ß, AddInv ß ]
           , (ƒ `add` addId) `mul` addInv µ `mul` ƒ `mul` (ß `add` addInv ƒ)
           ]
  where ß = V "ß"
        µ = V "µ"
        ƒ = V "ƒ"

-- We define our assignments via an environment and a lookup function
type Env var domain = [(var, domain)]

lookUp :: Eq v => Env v r -> v -> r
lookUp ((y,v) : rest) x | x == y    = v
                        | otherwise = lookUp rest x

-- For the concrete assignments, we fix the variable and ring types, of course
mvI :: Env RVar Integer
mvI = [ ("ß", 17), ("µ", -3), ("ƒ", 2) ]
mvF :: Env RVar (Integer -> Integer)
mvF = [ ("ß", const 17), ("µ", (+5) . (^2)), ("ƒ", (*7)) ]
mvM :: Env RVar (Mx2 Integer)
mvM = [ ("ß", (( 0, 1), (-3,-8)))
      , ("µ", ((11,-2), ( 2, 0)))
      , ("ƒ", (( 4, 0), ( 0, 0))) ]
