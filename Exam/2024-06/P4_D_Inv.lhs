> {-# LANGUAGE EmptyDataDeriving #-}
> {-# LANGUAGE FlexibleContexts #-}

P2 [20p] Typing maths: Derivative of Inverse

> quoteTyping = ( (f'(y)<0, a<y, y<b)  -- note local scope of x::Y in text
>               , y==fInv(x), x==f(y)
>               , [fInv, inv f]
>               , [f', der f]
>               , [der fInv (x) , oneOver (f'(fInv(x)))]
>               )

a) [5p] Give types for $f$, $a$, $b$, $x$, $y$, $f^{-1}$, and $f'$.
        Make sure to account for the possibility that the types of $x$
        and $y$ could be different.

> f     :: Y -> X
> a, b  :: Y
> x     :: X
> y     :: Y
> fInv  :: X -> Y
> f'    :: Y -> X

b) [7p] Give (short, textual) names and types to $d/dx$ and to the
    function $\cdot^{-1}$ that takes a function to its inverse.

> der :: (a->b)->(a->b)

Simplified view: in general der f is often not defined on all of a and
can give results in another set than b.

> inv :: (a->b)->(b->a)

Not asked for, but useful for understanding:

> oneOver :: X -> Y   -- This indicates that both X and Y are subsets of REAL

oneOver = recip would force X to be the same as Y in Haskell

c) [8p] Restate the final equation twice using your new names,
    first as a direct translation, then in point-free style (with no
    mention of $x$). You may use |recip x = 1/x|.

> part2c = ( der (inv f) x == oneOver (der f (inv f x))
>          , der (inv f)   =.= (oneOver . der f . inv f)
>          )

> (=.=) :: Eq b => (a->b)->(a->b)->(a->Bool)
> (f=.=g) x = f x == g x


----------------

Not part of the exam question:

> (f,a,b,x,y,fInv,f',der,inv,oneOver) = undefined
> data X deriving (Eq, Ord)
> data Y deriving (Eq, Ord)
> instance Num X
> instance Num Y

