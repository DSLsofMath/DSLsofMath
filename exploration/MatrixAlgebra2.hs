{-# LANGUAGE DeriveFunctor #-}
-- |

module MatrixAlgebra2 where


-- | Square matrices, quadtree like sparse representation
data Mat a
  = Q (Mat a) (Mat a)
      (Mat a) (Mat a) -- ^ 2x2 matrix
  | Id a -- ^ a * I (identity matrix scaled by a)
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

-- data Vec a
--   = V a a
--   deriving (Functor,Show)

-- instance Num a => Num (Vec a) where
--   fromInteger n = V (fromInteger n) (fromInteger n)

--   (V a b) + (V x y) = V (a+x) (b+y)

--   (V a b) * (V x y) = V (a*x) (b*y)

--   abs = fmap abs
--   signum = fmap signum
--   negate = fmap negate

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
