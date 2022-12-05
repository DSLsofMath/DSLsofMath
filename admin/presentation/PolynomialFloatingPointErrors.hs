-- A numerical exploration: f and g are the same polynomial on real
-- numbers, but differ on floating-point. There is only one root (at
-- x=1) with multiplicity 6, but there are several million
-- zero-crossings near x=1 for g as shown below.
f x = (x-1)^6
g x = x^6 - 6*x^5 + 15*x^4 - 20*x^3 + 15*x^2 - 6*x + 1

-- the smallest detectable change of 1 (eps == 2.220446049250313e-16 == 2^^(-52))
eps :: Double
eps = last (takeWhile (\e->1+e/=1) (iterate (/2) 1))

f' x = f x / eps
g' x = g x / eps

test = map round $ take 100 $ map g' $ iterate (eps+) 1
-- Starting from 1, stepping one eps at a time, the pattern is
-- [0,0,-4,0,0,0,4,0] repeated at least a million times

test2 = length $ takeWhile (0==) (zipWith (-) (map round $ map g' $ iterate (eps+) 1)
                                              (concat (repeat [0,0,-4,0,0,0,4,0])))
-- The first 16777219 elements match the pattern => 75% of those are zeroes
-- Then comes the first 8*eps. I wonder when the last zero comes...
{-
λ> 16777219*eps
3.725290964595729e-9
-}


xs = takeWhile (<=end) (iterate (step+) start)
start = 1-10^6*eps
end = 1+10^6*eps
step = eps -- (end-start)/177 
