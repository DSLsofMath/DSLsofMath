> module AbstractFOL (And, Not, Or, Impl,
>                     andIntro, andElimL, andElimR,
>                     orIntroL, orIntroR, orElim,
>                     notIntro, notElim,
>                     implIntro, implElim) where

> data And   p  q
> data Not   p
> data Or    p  q
> data Impl  p  q

> andIntro       ::  p -> q -> And p q
> andElimL       ::  And p q -> p
> andElimR       ::  And p q -> q

> orIntroL       ::  p -> Or p q
> orIntroR       ::  q -> Or p q
> orElim         ::  Or p q -> (p -> r) -> (q -> r) -> r

> notElim        ::  Not (Not p) -> p
> notIntro       ::  (p -> And q (Not q)) -> Not p

> implIntro      ::  (p -> q) -> Impl p q
> implElim       ::  Impl p q -> p -> q


> andIntro        =  undefined
> andElimL        =  undefined
> andElimR        =  undefined

> notElim         =  undefined
> notIntro        =  undefined

> orIntroL        =  undefined
> orIntroR        =  undefined
> orElim          =  undefined

> implIntro       =  undefined
> implElim        =  undefined
