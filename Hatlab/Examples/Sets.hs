import Hatlab.Plot
import Hatlab.Relations

p :: (Double, Double) -> (Double, Double) -> (Double, Double)
p (c, c') (a, b) = (c+a*a-b*b, c'+2*a*b)

abs_ :: (Double, Double) -> Double
abs_ (x, y) = sqrt (x*x+y*y)

isMandelbrot :: Double -> Double -> Bool
isMandelbrot a b = all (\compl -> (abs_ compl) <= 2) $ take 200 $ iterate (p ab) (0, 0)
    where
        ab = (a, b)
