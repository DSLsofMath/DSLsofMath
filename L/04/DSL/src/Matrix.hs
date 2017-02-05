-- | Simple 2D matrix algebra.
module Matrix
  ( Matrix, Vec, Point, Angle
  , vecX, vecY, ptX, ptY
  , matrix, point, vec
  , cross, mul, inv, sub
  ) where

type Angle  = Double
data Vec    = V { vecX, vecY :: Double }
type Point  = Vec
data Matrix = M Vec Vec

-- | Matrix creation
matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = M (V a b) 
                   (V c d)

-- | Vector creation
vec :: Double -> Double -> Vec
vec = V

-- | Point creation.
point :: Double -> Double -> Point
point = vec

-- | Cross product
cross :: Vec -> Vec -> Double
cross (V a b) (V c d) = a * c + b * d

-- | Matrix multiplication
mul :: Matrix -> Vec -> Vec
mul (M r1 r2) v = V (cross r1 v) (cross r2 v)

-- | Matrix inversion
inv :: Matrix -> Matrix
inv (M (V a b) (V c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

-- | Subtraction
sub :: Point -> Vec -> Point
sub (V x y) (V dx dy) = V (x - dx) (y - dy)

-- Run functions

ptX, ptY :: Point -> Double
ptX = vecX
ptY = vecY
