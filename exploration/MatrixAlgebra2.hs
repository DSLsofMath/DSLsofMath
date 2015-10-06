{-# LANGUAGE DeriveFunctor #-}
-- |

module MatrixAlgebra2 where


-- | Square matrices, quadtree like sparse representation
data Mat a
  = M (Mat a) (Mat a)
      (Mat a) (Mat a) -- ^ 2x2 matrix
  | Id a -- ^ a * I (identity matrix scaled by a)
  deriving (Functor,Show)

type S = Double
type M = Mat S

-- | scale matrix
(..*) :: Num a => a -> Mat a -> Mat a
a ..* m = fmap (a *) m

instance Num a => Num (Mat a) where
  fromInteger n
    = Id (fromInteger n)

  (Id a) + (Id x) = Id (a + x)
  m@(Id{}) + (M x y
                z w) = M (m + x) y
                          z       (m + w)
  m@(M{}) + n@(Id{}) = n + m
  (M a b
     c d) + (M x y
               z w) = M (a+x) (b+y)
                        (c+z) (d+w)

  (Id a) * m = a ..* m
  m * (Id x) = x ..* m
  (M a b
     c d) * (M x y
               z w)
    = M (a*x+b*z) (a*y+b*w)
        (c*x+d*z) (c*y+d*w)

  abs = fmap abs
  signum = fmap signum
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
