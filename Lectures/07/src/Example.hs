-- | Animation examples.
module Example where

import Control.Applicative

import Signal  (Signal, constS, ($$), mapS, timeS)
import Shape   (Shape, disc, square, difference, 
                scale, translate, vec, rotate)
import Animate (animate)
import Render  (defaultWindow)

-- | A rotating square
rotatingSquare :: Signal Shape
rotatingSquare = constS rotate $$ timeS $$ constS square
              -- Using the Control.Applicative interface:
-- rotatingSquare = rotate <$> timeS <*> pure square

-- | A stranger drop-in replacement for rotate in rotatingSquare
transmogrify :: Double -> Shape -> Shape
transmogrify d s = scale (vec (sin d) (cos d)) (rotate d s)

-- | A bouncing ball
bouncingBall :: Signal Shape
-- bouncingBall = constS translate $$ pos $$ constS ball
bouncingBall = translate <$> pos <*> pure ball
  where
    ball = scale (vec 0.5 0.5) disc
    pos  = constS vec $$ bounceX $$ bounceY
    bounceY = mapS ((0.8*) . sin . (3*)) timeS
--    bounceX = constS 0
    bounceX = mapS ((0.8*) . sin . (2*)) timeS
--    bounceX = mapS (0.3*) bounceY

-- | Combining the two
example :: Signal Shape
--example = constS difference $$ rotatingSquare $$ bouncingBall
example = difference <$> rotatingSquare <*> bouncingBall

{-
-- Illustrate type error and finding the solution
example2 = difference <$> one <*> two
    where one :: Signal Shape 
          one = example
          two :: Signal Shape
          two = scale (vec (-1) (0.5)) one
-}                        
            
runExample :: IO ()
runExample = animate defaultWindow 0 endTime example
  where endTime = 15

-- main = runExample
