{-# LANGUAGE DeriveFunctor #-}
-- |

module MatrixAlgebra2 where

import Test.QuickCheck


-- | 2^n-by-2^n matrices, "quadtree like sparse representation"
data Mat a
  = Q (Mat a) (Mat a)
      (Mat a) (Mat a) -- ^ 2^2n-by-2^2n matrix, composed by four
                      -- 2^n-by-2^n matrices
  | Id a -- ^ a * I (2^n-by-2^n identity matrix scaled by a)
  | Z -- ^ zero matrix
  deriving (Functor,Show)

type S = Double
type M = Mat S

-- | scale matrix
(..*) :: Num a => a -> Mat a -> Mat a
a ..* m = fmap (a *) m


instance Num a => Num (Mat a) where
  fromInteger 0 = Z
  fromInteger n = Id (fromInteger n)

  Z + m = m
  m + Z = m
  (Id a) + (Id b) = Id (a + b)
  m@(Id{}) + (Q x y
                z w) = Q (m + x) y
                          z      (m + w)
  m@(Q{}) + n@(Id{}) = n + m
  (Q a b
     c d) + (Q x y
               z w) = Q (a+x) (b+y)
                        (c+z) (d+w)

  Z * _ = Z
  _ * Z = Z
  (Id a) * m = a ..* m
  m * (Id b) = b ..* m
  (Q a b
     c d) * (Q x y
               z w)
    = Q (a*x+b*z) (a*y+b*w)
        (c*x+d*z) (c*y+d*w)

  abs    = fmap abs       -- TODO: norm?
  signum = fmap signum    -- TODO: should this be undefined?
  negate = fmap negate

{-
instance Eq a => Eq (Mat a) where
  (==) = eqMat (==)

type Equal a = a -> a -> Bool

eqMat :: Equal a -> Equal (Mat a)
eqMat eq = eqM where
  eqM Z Z = True
  eqM (Id a) (Id b) = eq a b
  eqM (Q a00 a01 a10 a11) (Q b00 b01 b10 b11) =
    eqM a00 b00 && eqM a01 b01 && eqM a10 b10 && eqM a11 b11
  eqM Z (Id b) = eq 0 b
  eqM (Id a) Z = eq a 0

-- and a few more lines.

But perhaps it is more natural to check if (a-b == 0).

-}

isZ :: (Eq a, Num a) => Mat a -> Bool
isZ Z            = True
isZ (Id a)       = 0==a
isZ (Q a b c d)  = isZ a && isZ b && isZ c && isZ d

instance (Eq a, Num a) => Eq (Mat a) where
  x == y = isZ (x-y)

instance Arbitrary a => Arbitrary (Mat a) where
  arbitrary = oneof [pure Z
                    ,Id <$> arbitrary
                    ,Q <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    ]
  shrink Z = []
  shrink (Id a) = Z : (Id <$> shrink a)
  shrink (Q a b c d) = [Z, a, b, c, d] ++
                       [Q a' b' c' d' | (a',b',c',d') <- shrink (a,b,c,d)]


-- * Vectors

data Vec a
  = V (Vec a) (Vec a) -- ^ a 2^2n vector composed by two 2^n vectors
  | VOne a -- ^ a 2^n sized vector containing a's
  | ZV
  deriving (Functor,Show)

instance Num a => Num (Vec a) where
  fromInteger n = VOne (fromInteger n)

  ZV + v = v
  v + ZV = v
  (VOne a) + (VOne x) = VOne (a+x)
  v@(VOne{}) + (V x y) = V (v + x) (v + y)
  v + w@(VOne{}) = w + v
  (V a b) + (V x y) = V (a+x) (b+y)

  -- pointwise multiplication
  ZV * _ = ZV
  _ * ZV = ZV
  (VOne a) * w = fmap (a *) w
  v * (VOne x) = fmap (* x) v
  (V a b) * (V x y) = V (a * x) (b * y)

  abs = fmap abs
  signum = fmap signum
  negate = fmap negate

isZV :: (Eq a, Num a) => Vec a -> Bool
isZV ZV       = True
isZV (VOne a) = 0==a
isZV (V a b)  = isZV a && isZV b

instance (Eq a, Num a) => Eq (Vec a) where
  x == y = isZV (x - y)

instance Arbitrary a => Arbitrary (Vec a) where
  arbitrary = oneof [pure ZV, VOne <$> arbitrary, V <$> arbitrary <*> arbitrary]

  shrink (V v1 v2) = [ZV, v1, v2] ++ [V v1' v2' | (v1',v2') <- shrink (v1,v2)]
  shrink (VOne v) = ZV : (VOne <$> shrink v)
  shrink ZV = []

-- * Vector and matrix operations

infix 7 .*
infix 7 *.
infix 7 .*.

-- | multiply matrix on left by vector
(.*) :: Num a => Vec a -> Mat a -> Vec a
_ .* Z = ZV
ZV .* _ = ZV
(V a0 a1) .* (Q b00 b01
                b10 b11) = V (a0 .* b00 + a1 .* b10)
                             (a0 .* b01 + a1 .* b11)
(V a0 a1) .* m@(Id{}) = V (a0 .* m)
                          (a1 .* m)
v@(VOne{}) .* (Q b00 b01
                 b10 b11) = V (v .* b00 + v .* b10)
                              (v .* b01 + v .* b11)
(VOne a) .* (Id b) = VOne (a * b)

prop_vm_id_right :: Vec Integer -> Bool
prop_vm_id_right = \v -> v .* 1 == v

-- | multiply matrix on right by vector
(*.) :: Num a => Mat a -> Vec a -> Vec a
_ *. ZV                   = ZV
Z *. _                    = ZV
m@(Id{}) *. (V b0
               b1)        = V (m *. b0)
                              (m *. b1)
(Q a00 a01
   a10 a11) *. v@(VOne{}) = V (a00 *. v + a01 *. v)
                              (a10 *. v + a11 *. v)
(Q a00 a01
   a10 a11) *. (V b0
                  b1)     = V (a00 *. b0 + a01 *. b1)
                              (a10 *. b0 + a11 *. b1)
(Id v) *. (VOne m)        = VOne (v * m)

prop_mv_id_left :: Vec Integer -> Bool
prop_mv_id_left = \v -> 1 *. v == v

-- | dot product -- might surprising since the size of the vectors is
-- not explicitly stated anywhere
(.*.) :: Num a => Vec a -> Vec a -> a
ZV .*. _                 = 0
_ .*. ZV                 = 0
(V a1 a2) .*. v@(VOne{}) = a1 .*. v + a2 .*. v
v@(VOne{}) .*. (V b1 b2) = v .*. b1 + v .*. b2
(V a1 a2) .*. (V b1 b2)  = a1 .*. b1 + a2 .*. b2
(VOne a) .*. (VOne b)    = a * b
