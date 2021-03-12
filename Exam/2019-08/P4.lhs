Task: solve

  (f''(x) + 3f'(x))/2 = 1 - f(x),   f(0) = -1,   f'(0) = 3

in two ways: using power series and using the Laplace transform.

First simplify to

  f''(x) = 2 - 3*f'(x) - 2*f(x)
  f(0)   = -1
  f'(0)  = 3

a) Power series:

< fs   = integ (-1) fs'
< fs'  = integ   3  fs''
< fs'' = two - 3*fs' - 2*fs

> integ x0 x' = x0 : zipWith (/) x' [1..]

A common mistake is to guess that |two = 2:two| but that is not correct.

> two, zeros :: Num a => [a]
> two   = 2:zeros
> zeros = 0:zeros

Now we can start "hand-computing" the coefficient lists in the order
  fs!!0, fs'!!0, fs''!!0,  fs!!1, fs'!!1, fs''!!1, ...

> fs, fs', fs'' :: Fractional a => [a]
> fs   = (-1) :   3  : (-5)/2 :  3/2 : u
> fs'  =   3  : (-5) :    9/2 : u
> fs'' = (-5) :   9  : u
> u = undefined

The helper computations are
  fs!!0   == f(0) == -1
  fs'!!0  == f'(0) == 3
  fs''!!0 == 2 - 3*3    -2*(-1) == 2-9+2 == -5
  fs!!1   == (fs'!!0)/1 == 3
  fs'!!1  == (fs''!!0)/1 == -5
  fs''!!1 == 0 - 3*(-5) -2*3    == 15-6  ==  9
  fs!!2   == (fs'!!1)/2  == -5/2
  fs'!!2  == (fs''!!1)/2 == 9/2
  fs!!3   == (fs'!!2)/3  == (9/2)/3 == 3/2

Svar: take 4 fs == [-1, 3, -5/2, 3/2]

Test the original eq:

< lhs2 = take 2 (fs'' + 3*fs')/2 == ([-5,9]+3*[3,-5])/2 == [4,-6]/2 == [2,-3]
< rhs2 = take 2 ((1:zeros)-fs)   == [1,0] - [-1,3] = [2,-3]

OK!

----------------

b) Laplace:

We start again from the simplified equation:

  f''(x) = 2 - 3*f'(x) - 2*f(x)     [1]
  f(0)   = -1
  f'(0)  = 3

We need the rule for derivatives

  L f' s = - f 0 + s*L f s

which iterated gives us

  L f'' s = - f' 0 + s*L f' s = - f' 0 + s*(- f 0 + s*L f s) = - f' 0 - s*f 0 + s^2*L f s

and using our initial conditions (f(0) = -1, f'(0) = 3)

  L f' s  =   1 + s  *L f s
  L f'' s = s-3 + s^2*L f s

Thus, by applying Laplace to both sides of equation [1] we get

  L f'' s  =  L (const 2) s - 3*L f' s - 2*L f s
<=> {- L (const 2) s = 2/s -}
  s-3 + s^2*L f s  = 2/s - 3*(1 + s*L f s) - 2*L f s
<=> {- collect terms -}
  (s^2 +3*s + 2)*L f s = -s+3 + 2/s -3
<=> {- simplify -}
  L f s = (2/s - s)/(s^2 +3*s + 2)
<=> {- factor -}
  L f s = (2 - s^2)/(s*(s+1)*(s+2))

Now we make the Ansatz

  L f s = A/s + B/(s+1) + C/(s+2)

and start working out the coefficients:

  A/s + B/(s+1) + C/(s+2) == (2 - s^2)/(s*(s+1)*(s+2))
= -- multiply by (s*(s+1)*(s+2))
  A*(s+1)*(s+2) + B*s*(s+2) + C*s*(s+1) == 2 - s^2

Separate in tre equations - one each for s=0, s=-1, s=-2:
  s=0:  A*1*2 == 2                 => A ==  1
  s=-1: B*(-1)*1 == 2 - 1          => B == -1
  s=-2: C*(-2)*(-1) == 2 - 4       => C == -1

Thus we have

  L f s = 1/s-1/(s+1)-1/(s+2)

which we can recognize as the transform of

  f(x) = 1 - exp(-x) -   exp(-2*x)

Finally we check the three original equations (after computing some helpers):

  f'(x)  =   exp(-x) + 2*exp(-2*x)
  f''(x) =  -exp(-x) - 4*exp(-2*x)

  lhs = (f''(x) + 3f'(x))/2
      = (-exp(-x) - 4*exp(-2*x) + 3exp(-x) + 6*exp(-2*x))/2
      = (2*exp(-x) + 2*exp(-2*x))/2
      = exp(-x) + exp(-2*x)
      = 1 - (1 - exp(-x) - exp(-2*x))
      = rhs

  f(0) = 1-1-1 = -1
  f'(0) = 1+2  =  3

Yes!

----------------

We can also compare a) and b) [not part of the exam question]:

exps  = 1:1:1/2:1/6
expms = 1:-1:1/2:-1/6
-expms = -1:1:-1/2:1/6
expts = 1:2:2:4/3
expmts = 1:-2:2:-4/3
-expmts = -1:2:-2:4/3
one = 1:0:0:0

fs =  1:0: 0  :0
   + -1:1:-1/2:1/6
   + -1:2:-2  :4/3
   = -1:3:-5/2:3/2

Yes!

----------------------------------------------------------------

> main = print (take 4 fs)

Not needed on the exam

> instance Num a => Num [a] where
>   fromInteger = (:zeros) . fromInteger
>   (+) = zipWithLonger (+)
>   negate = map negate

> zipWithLonger op xs [] = xs
> zipWithLonger op [] ys = ys
> zipWithLonger op (x:xs) (y:ys) = op x y : zipWithLonger op xs ys
