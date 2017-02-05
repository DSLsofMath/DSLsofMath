{-# LANGUAGE GADTs #-} -- only for presentation
-- | Simple library for 2D shapes. Deep embedding.
module Shape.Deep
  (
  -- * Types
    Shape -- abstract
  -- * Constructor functions
  , empty, disc, square
  -- * Primitive combinators
  , transform, translate
  , union, intersect, invert
  -- * Run functions
  , inside
  ) where

import Matrix

-- | In a deep embedding our objects are represented by a datatype
--   with constructors for each primitive operation.
{-
From the lecture slides:

type Shape
-- Constructor functions
empty   :: Shape
disc    :: Shape
square  :: Shape
-- Combinators
translate :: Vec ->   Shape -> Shape
scale     :: Vec ->   Shape -> Shape
rotate    :: Angle -> Shape -> Shape
union     :: Shape -> Shape -> Shape
intersect :: Shape -> Shape -> Shape
difference:: Shape -> Shape -> Shape

Later scale, rotate, difference are derived 
from invert and transform:

invert    :: Shape -> Shape
transform :: Matrix -> Shape -> Shape

-}

data Shape where
  -- Constructor functions
  Empty   :: Shape
  Disc    :: Shape
  Square  :: Shape
  -- Combinators
  Translate :: Vec ->   Shape -> Shape
  Transform :: Matrix-> Shape -> Shape
  Union     :: Shape -> Shape -> Shape
  Intersect :: Shape -> Shape -> Shape
  Invert    :: Shape -> Shape

{-
-- Shape without GADT syntax
data Shape = Empty | Disc | Square
           | Translate Vec Shape
           | Transform Matrix Shape
           | Union Shape Shape
           | Intersect Shape Shape
           | Invert Shape
-}

-- * Creation functions

empty  :: Shape
empty  = Empty

disc   :: Shape
disc   = Disc  

square :: Shape
square = Square

-- * Primitive combinators

transform :: Matrix -> Shape -> Shape
transform = Transform

translate :: Vec -> Shape -> Shape
translate = Translate

union :: Shape -> Shape -> Shape
union = Union

intersect :: Shape -> Shape -> Shape
intersect = Intersect

invert :: Shape -> Shape
invert = Invert

-- * Run function

inside :: Point -> Shape -> Bool
_p `inside` Empty             = False
p  `inside` Disc              = sqDistance p <= 1
p  `inside` Square            = maxnorm  p <= 1
p  `inside` Translate v sh    = (p `sub` v) `inside` sh
p  `inside` Transform m sh    = (inv m `mul` p) `inside` sh
p  `inside` Union sh1 sh2     = p `inside` sh1 || p `inside` sh2
p  `inside` Intersect sh1 sh2 = p `inside` sh1 && p `inside` sh2
p  `inside` Invert sh         = not (p `inside` sh)

-- * Helper functions

sqDistance :: Point -> Double
sqDistance p = x*x+y*y -- proper distance would use sqrt
  where x = ptX p
        y = ptY p

maxnorm :: Point -> Double
maxnorm p = max (abs x) (abs y)
  where x = ptX p
        y = ptY p

-- | More reading: http://en.wikipedia.org/wiki/Norm_%28mathematics%29#Properties
