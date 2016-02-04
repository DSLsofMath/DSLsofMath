> module Arithmetic where

> import FOL


The constructs in our language can be represented symbolically via datatypes:

> data AName  =  Zero         deriving Show
> data AFunc  =  Succ | Plus  deriving Show
> data APred  =  LE           deriving Show


We can fix the identifier type for variables and define our term and well-formed
formulae datatypes for the arithmetic FOL:

> type AVar   =  String

> type ATerm = Term AName AFunc AVar
> type AWFF  = WFF  AName AFunc AVar APred


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
'Integer' values*:

> type AEnvInt = Env AVar Integer

> evalA  :: AEnvInt -> ATerm -> Integer
> evalA   = \env -> eval env evAName evAFunc
> checkA :: AEnvInt -> AWFF -> Bool
> checkA  = \env -> check env evAName evAFunc evAPred


Some basic natural number synonyms:

> zero, one, two :: ATerm
> zero        =  N Zero
> one         =  F Succ [N Zero]
> two         =  F Succ [one]


A synonym for syntactic 'Plus'

> plus t0 t1   =  F Plus [t0, t1]


Write some terms and well-formed formulae now and evaluate them/check their
validity.

Here are some term examples:

> t01, t02, t03, t04, t05 :: ATerm
> t01 = F Plus [two, two]
> t02 = F Plus [ F Plus [ zero, one ]
>              , F Succ [ F Plus [zero, zero] ] ]
> t03 = F Succ [ two ]
> t04 = F Plus [ two, one ]
> t05 = F Plus [ one, two ]


Let us now construct some well-formed formulae:

> wf01, wf021, wf022, wf02, wf03, wf04 :: AWFF
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
