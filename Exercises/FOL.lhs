> module FOL where

--------------------------------------------------------------------------------
Types for FOL syntax

> data Term name func var  =  N name | F func [Term name func var] | V var
>                             deriving (Eq, Ord, Show)

> data WFF name func var pred = P pred [Term name func var]
>                             | Eq (Term name func var) (Term name func var)
>                             | Forall var (WFF name func var pred)
>                             | Exists var (WFF name func var pred)
>                             | And (WFF name func var pred) (WFF name func var pred)
>                             | Or (WFF name func var pred) (WFF name func var pred)
>                             | Impl (WFF name func var pred) (WFF name func var pred)
>                             | Iff (WFF name func var pred) (WFF name func var pred)
>                             | Not (WFF name func var pred)

--------------------------------------------------------------------------------
Semantics

> type Env var domain = [(var, domain)]

> eval :: Eq var
>      => Env var domain
>      -> (name -> domain)
>      -> (func -> [domain] -> domain)
>      -> Term name func var
>      -> domain

> check :: (Eq var, Eq domain)
>       => Env var domain
>       -> (name  ->  domain)
>       -> (func  ->  [domain]  ->  domain)
>       -> (pred  ->  [domain]  ->  Bool)
>       -> WFF name func var pred
>       -> Bool

--------------------------------------------------------------------------------
Implementations

> lookUp :: Eq var => var -> Env var domain -> domain
> lookUp x ((y, v):rest)        = if x == y then v else lookUp x rest

> eval env evN evF = eva where
>   eva (N name)     =  evN name
>   eva (V var)      =  lookUp var env
>   eva (F f terms)  =  evF f (map eva terms)

> check env evN evF evP = chec
>   where
>     eva = eval env evN evF
>     chec (P pred terms)    =  evP pred (map eva terms)
>     chec (Eq t0 t1)        =  eva t0  ==  eva t1
>     chec (And wff0 wff1)   =  chec wff0  &&  chec wff1
>     chec (Or wff0 wff1)    =  chec wff0  ||  chec wff1
>     chec (Impl wff0 wff1)  =  not (chec wff0)  ||  chec wff1
>     chec (Iff wff0 wff1)   =  chec (Impl wff0 wff1) && chec (Impl wff1 wff0)
>     chec (Not wff)         =  not (chec wff)
