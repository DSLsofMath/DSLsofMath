> module Main where
> import Hatlab.Derivatives
> import Hatlab.Plot
>
> e1 :: Expression
> e1 = sin X
> d  = derivative
> es = iterate d e1
> test1 = plot (take 4 es)

> e2 :: Expression
> e2 = cos (2*acos X)

> main = test1
