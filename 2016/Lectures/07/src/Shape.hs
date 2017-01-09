-- | Simple library for 2D shapes.
module Shape
  ( module ShapeImpl
  , module Shape
  , module Matrix
  ) where

import Matrix
-- import Shape.Shallow as ShapeImpl
import Shape.Deep as ShapeImpl

-- | Derived combinators

scale :: Vec -> Shape -> Shape
scale v = transform (matrix  (vecX v)  0 
                             0         (vecY v))

rotate :: Angle -> Shape -> Shape
rotate d = transform (matrix  c  (-s) 
                              s  c   )
  where  c = cos d
         s = sin d

difference :: Shape -> Shape -> Shape
difference sh1 sh2 = sh1 `intersect` invert sh2
