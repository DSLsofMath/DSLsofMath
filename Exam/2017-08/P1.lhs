Problem 1 [30pts]: Algebraic structure: a DSL for semirings.

> {-# LANGUAGE GADTs #-}
> module P1 where
> import qualified Prelude
> import Prelude((==),Bool(False,True),(&&),(||),and,Integer,String)
> import Test.QuickCheck -- only for testing, not part of exam question

1a: A type class |SemiRing|.

> class SemiRing r where
>   add    ::  r -> r -> r
>   mul    ::  r -> r -> r
>   zero   ::  r
>   one    ::  r

1b: A datatype |SR v| for semiring expressions + syntactic instance.

> data SR v where
>   Add    ::  SR v -> SR v -> SR v
>   Mul    ::  SR v -> SR v -> SR v
>   Zero   ::  SR v
>   One    ::  SR v
>   Var    ::  v -> SR v

> instance SemiRing (SR v) where add=Add; mul=Mul; zero=Zero; one=One

1c: Two other instances.

> instance SemiRing Integer where add=(Prelude.+); mul=(Prelude.*); zero=0; one=1
> instance SemiRing Bool    where add=(||); mul=(&&); zero=False; one=True

Other possibilities: square matrices (of a fixed size), Maybe Integer
with max and (+), Bool with xor and (&&), modulo arithmetics, ...

1d: Type and define |eval|.

> eval :: SemiRing r => (v -> r) -> SR v -> r
> eval f = e where
>   e (Add x y)  =  add (e x) (e y)
>   e (Mul x y)  =  mul (e x) (e y)
>   e Zero       =  zero
>   e One        =  one
>   e (Var v)    =  f v

1e: Specialise |eval| to the types in 1c.

> evalI :: (v -> Integer)  -> SR v -> Integer
> evalI = eval
> evalB :: (v -> Bool)     -> SR v -> Bool
> evalB = eval

> e1, e2, e3 :: SR String
> e1 = Add (Var "x") One
> e2 = Mul e1 e1
> e3 = Add e2 e2

> v1I, v2I, v3I :: Integer
> v1B, v2B, v3B :: Bool
> aI :: String -> Integer
> aI _ = 2
> aB :: String -> Bool
> aB _ = True
> v1I = evalI aI e1
> v2I = evalI aI e2
> v3I = evalI aI e3
> v1B = evalB aB e1
> v2B = evalB aB e2
> v3B = evalB aB e3

> check1e = and [ v1I==3, v2I==9, v3I==18, v1B, v2B, v3B]

----------------

The below is not required in the answer - just included here to check
the types.

Synonyms for more readable laws:

> (+), (⋅) :: SemiRing r => r -> r -> r
> (+) = add
> (⋅) = mul

> infixl 6 +
> infixl 7 ⋅

> law1 a b c =    (a + b) + c == a + (b + c)
> law2 a     =     zero + a == a && a + zero == a
> law3 a b   =     a + b == b + a

> law4 a b c =     (a⋅b)⋅c == a⋅(b⋅c)
> law5 a     =     one⋅a == a &&  a⋅one == a

> law6 a b c =     a⋅(b + c) == (a⋅b) + (a⋅c)
> law7 a b c =     (a + b)⋅c == (a⋅c) + (b⋅c)
> law8 a     =     a⋅zero == zero && zero⋅a == zero

> law1I i = law1 (i :: Integer); law1B b = law1 (b :: Bool)
> law2I i = law2 (i :: Integer); law2B b = law2 (b :: Bool)
> law3I i = law3 (i :: Integer); law3B b = law3 (b :: Bool)
> law4I i = law4 (i :: Integer); law4B b = law4 (b :: Bool)
> law5I i = law5 (i :: Integer); law5B b = law5 (b :: Bool)
> law6I i = law6 (i :: Integer); law6B b = law6 (b :: Bool)
> law7I i = law7 (i :: Integer); law7B b = law7 (b :: Bool)
> law8I i = law8 (i :: Integer); law8B b = law8 (b :: Bool)

> main = do quickCheck law1I; quickCheck law1B
>           quickCheck law2I; quickCheck law2B
>           quickCheck law3I; quickCheck law3B
>           quickCheck law4I; quickCheck law4B
>           quickCheck law5I; quickCheck law5B
>           quickCheck law6I; quickCheck law6B
>           quickCheck law7I; quickCheck law7B
>           quickCheck law8I; quickCheck law8B
