sq :: Double -> Double
sq(x) = x^2

twice :: (Double -> Double) -> (Double -> Double)
twice f = \x -> f(f(x))

quad :: Double -> Double
quad = twice sq

test :: Double
test = quad 3

list :: [Double -> Double]
list = [(1+), (2*), sq, quad]
