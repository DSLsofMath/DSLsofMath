> module Arithmetic where

> import FOL

> data AName  =  Zero         deriving Show
> data AFunc  =  Succ | Plus  deriving Show
> data APred  =  LE           deriving Show
> type AVar   =  String


We now give *meaning* to our language's constructs in a domain of discourse of
choice, here 'Integer'.

> evAName :: AName -> Integer
> evAName       = const 0
> evAFunc :: AFunc -> ([Integer] -> Integer)
> evAFunc Succ  = succ where succ [n] = n + 1
> evAFunc Plus  = plus where plus [a,b] = a + b
> evAPred :: APred -> ([Integer] -> Bool)
> evAPred LE    = le   where le [x, y] = x <= y


Now we can define evaluation for the arithmetic language *into the domain of
'Ingeter' values*:

> evalA :: Eq var => Env var Integer -> Term AName AFunc var -> Integer
> evalA   = \env -> eval env evAName evAFunc
> checkA :: Eq var => Env var Integer -> WFF AName AFunc var APred -> Bool
> checkA  = \env -> check env evAName evAFunc evAPred


Some basic natural number synonyms:

> zero, one, two :: Term AName AFunc AVar
> zero        =  N Zero
> one         =  F Succ [N Zero]
> two         =  F Succ [one]


A synonym for syntactic 'Plus'

> plus t0 t1   =  F Plus [t0, t1]


Write some terms and well-formed formulae now and evaluate them/check their
validity.

Here are some term examples:

> t01 = F Plus [two, two]
> t02 = F Plus [ F Plus [ zero, one ]
>              , F Succ [ F Plus [zero, zero] ] ]

> t03 = F Succ [ two ]
> t04 = F Plus [ two, one ]
> t05 = F Plus [ one, two ]


Let us now construct some well-formed formulae:

> wf01  = And (Eq t03 t04) (Eq t04 t05)
> wf021 = P LE [ V "x", V "y" ]
> wf022 = P LE [ V "x", F Succ [V "y"] ]
> wf02  = Impl wf021 wf022
> wf03  = Iff wf021 (Impl (Not (Eq (V "x") (V "y"))) wf022)
> wf04  =  wf021
>         `Iff`
>          Impl (Not (Eq (V "x") (V "y")))
>               (P LE [F Succ [V "x"], V "y"])


We can now consider, for instance, the following environments for the formulae
(check which of them will hold and that it this is indeed correct):

> e01 = [("x",1), ("y",2)]
> e02 = [("y",1), ("x",2)]



