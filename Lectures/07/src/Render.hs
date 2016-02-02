-- | Textual rendering of a 'Shape'.
module Render where

import Shape

-- | A window specifies what part of the world to render and at which
--   resolution.
data Window = Window
  {  bottomLeft  :: Point
  ,  topRight    :: Point
  ,  resolution  :: (Int, Int)
  }

defaultWindow :: Window
defaultWindow = Window
  { bottomLeft  = point (-1.5) (-1.5)
  , topRight    = point 1.5 1.5
  , resolution  = (40, 40)
--  , resolution = (50, 50) -- for larger terminal windows
  }

-- | Generate a list of evenly spaces samples between two values.
samples :: Double -> Double -> Int -> [Double]
samples x0 x1 n = take n $ iterate (+dx) x0
  where
    dx = (x1 - x0) / fromIntegral (n - 1)

-- | Generate the matrix of points corresponding to the pixels of a window.
pixels :: Window -> [[Point]]
pixels (Window p0 p1 (w,h)) =
  [  [ point x y | x <- samples (ptX p0) (ptX p1) w ]
  |  y <- reverse $ samples (ptY p0) (ptY p1) h
  ]

-- | Render a shape in a given window.
render :: Window -> Shape -> String
render win sh = unlines $ map (concatMap putPixel) (pixels win)
  where
    putPixel p  | p `inside` sh = "[]"
                | otherwise     = "  "
