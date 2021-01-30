> module DSLsofMath.W01.Test where
> import DSLsofMath.W01
> import DSLsofMath.CSem hiding ((/))
> import Test.QuickCheck
> import Prelude hiding (flip)

We may also need the Haskell standard library version for some testing later:

> import qualified Data.Complex as DC

> fromC (C (x , y)) = Plus (ToComplex x) (Times (ToComplex y) ImagUnit)

> instance (Num r, Arbitrary r) => Arbitrary (Complex r) where
>   arbitrary = arbitraryC arbitrary
>   shrink = shrinkC shrink

> arbitraryC :: Gen (r , r) -> Gen (Complex r)
> arbitraryC arb = C <$> arb

> shrinkC :: Num r => ((r , r) -> [(r , r)]) -> (Complex r -> [Complex r])
> shrinkC shr (C (p@(x, y))) = [C (0, 0), C (x,0), C (0, y)] ++ map C (shr p)

> instance Arbitrary ComplexE where
>   arbitrary = sized arbitraryCESized
>   shrink = shrinkCE

> arbitraryCESized :: Int -> Gen ComplexE
> arbitraryCESized n | n <= 0 = oneof [ return ImagUnit
>                                     , ToComplex <$> arbitrary
>                                     , fromC <$> arbitrary
>                                     ]
>                    | otherwise = do
>   op <- elements [Plus, Times]
>   let arbHalf = arbitraryCESized (n `div` 2)
>   op <$> arbHalf <*> arbHalf


> shrinkCE :: ComplexE -> [ComplexE]
> shrinkCE ImagUnit = [ToComplex 0]
> shrinkCE (ToComplex r) = ToComplex <$> shrink r
> shrinkCE (Times r ImagUnit) = r:shrink r
> shrinkCE (Plus  l (Times r ImagUnit)) = [l, Times r ImagUnit]
> shrinkCE (e@(Plus  l r)) = fromCD (evalE e) :
>   [l, r] ++ [Plus  l r' | r' <- shrink r]
>          ++ [Plus  l' r | l' <- shrink l]
> shrinkCE (e@(Times l r)) = fromCD (evalE e) :
>   [l, r] ++ [Times l r' | r' <- shrink r]
>          ++ [Times l' r | l' <- shrink l]

> main = do
>   quickCheck propFromCD
>   quickCheck $ expectFailure (propAssocPlus     :: REAL->REAL->REAL->Bool)
>   quickCheck $ expectFailure (propAssocTimes    :: REAL->REAL->REAL->Bool)
>   quickCheck $ expectFailure (propDistTimesPlus :: REAL->REAL->REAL->Bool)

> propAssocSmall :: Int -> Int -> Int -> Bool
> propAssocSmall m n k = propAssocAdd (fromIntegral m) (fromIntegral n) ((1/fromIntegral k) :: Double)

> instance Arbitrary ComplexD where
>   arbitrary = fmap (\(C p) -> CD p) arbitrary



> eqFun2 :: Eq c => (a -> b -> c) -> (a -> b -> c) -> (a -> b -> Bool)
> eqFun2 f g a b  =  f a b == g a b

Moved to Chapter 3

cmpEvalEnv :: Int -> Env Int String -> Bool
cmpEvalEnv = eqFun2 lookup (flip evalEnv)

testEvalEnv :: IO ()
testEvalEnv = quickCheck cmpEvalEnv
