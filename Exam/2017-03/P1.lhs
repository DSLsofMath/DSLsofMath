> module P1 where

[20pts] Consider the following text from page 169 of Mac Lane [1968]:

1:  [...] a function |z = f (x, y)| for all points |(x, y)| in some open
2:  set |U| of the cartesian |(x, y)|-plane.
3:  [...] If one holds |y| fixed, the quantity |z| remains just a
4:  function of |x|; its derivative, when it exists, is called the
5:  *partial derivative* with respect to |x|.
6:  Thus at a point |(x, y)| in |U| this derivative for |h ≠ 0| is

7:  ∂ z / ∂ x  =  f'_x (x, y)  =  \lim_{h \to 0} (f (x + h, y) - f (x, y)) / h

What are the types of the elements involved in the equation on the last line?
You are welcome to introduce functions and names to explain your reasoning.

----------------------------------------------------------------

Even though the exam question only asks for the last line we will here
analyse the text from the top.

Line 1: |z : Z|, |f : U -> Z|, |x : X|, |y : Y|, |(x, y) : (X, Y)|,

Line 2: |U : Powerset (X,Y)|, probably |X = Y = ℝ|, |Z = ℝ|

Line 3-4: For fixed |y|, the "quantity" |z| is a function of |x|. Lets
name this family of functions |g y x = f (x, y)|.

Line 5: For a fixed |y| the partial derivative of |f| with respect to
|x| is the "normal" derivative of the function |g y : X -> Z|. For all
|y| we can call this derivative |g' y = D (g y) : X -> Z|.

Line 6: Here we pick a fixed (but arbitrary) |(x, y)| and (implicitly)
introduce a value |h : H| with (presumably) |H = ℝ - {0}|.

Line 7 (the last line): From the context we know that this line gives
the value of the partial derivative at one specific point |(x, y)|.
Thus |∂ z / ∂ x| is implicitly applied to |(x, y)|. We could write
|(∂z/∂x)(x,y) : Z| and thus |(∂z/∂x) : U -> Z| if we generalise to a
function from arbitrary points. Note that |x| in |∂x| is just a label
and not the value of |x| in scope from Line 6. We could say that the
operator |∂/∂x| has type |(U->Z) -> (U->Z)|

Similarly |f'_x (x,y) : Z|, |f'_x : U -> Z|, and the |x| here is again
just a text label. The post-fix operator "prime and subscript x" has
type |(U->Z) -> (U->Z)|.

Then the limit expression: First remember that |(x,y)| is fixed, so
the only "varying variable" here is |h|. If we rewrite to use

< lim : (H -> Z) -> {p | p ∈ ℝ, Limp p H} -> Z

(from the lecture notes Weeks 2-3) we get the expression

< lim (\h -> (f (x + h, y) - f (x, y)) / h) 0

or if we want to name the anonymous function we could say

> psi :: (U -> Z) -> U -> H -> Z
> psi f (x, y) h = (f (x + h, y) - f (x, y)) / h

The limit is then |lim (psi f (x, y)) 0|.

----------------

Just for type-checking:

> data X
> data Y
> type U = (X,Y)
> type Z = X
> type H = Z
> f :: U -> Z
> f = undefined
> instance Num X
> instance Fractional X

> main = undefined
