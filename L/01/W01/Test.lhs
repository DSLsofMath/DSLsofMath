> module DSLsofMath.W01.Test where
> import DSLsofMath.W01
> import DSLsofMath.CSem
> import Test.QuickCheck

We may also need the Haskell standard library version for some testing later:

> import qualified Data.Complex as DC

> fromCS (CS (x , y)) = Plus (ToComplex x) (Times (ToComplex y) ImagUnit)

> instance (Num r, Arbitrary r) => Arbitrary (ComplexSem r) where
>   arbitrary = arbitraryCS arbitrary
>   shrink = shrinkCS shrink

> arbitraryCS :: Gen (r , r) -> Gen (ComplexSem r)
> arbitraryCS arb = CS <$> arb

> shrinkCS :: Num r => ((r , r) -> [(r , r)]) -> (ComplexSem r -> [ComplexSem r])
> shrinkCS shr (CS (p@(x, y))) = [CS (0, 0), CS (x,0), CS (0, y)] ++ map CS (shr p)

> instance Arbitrary ComplexE where
>   arbitrary = sized arbitraryCESized
>   shrink = shrinkCE

> arbitraryCESized :: Int -> Gen ComplexE
> arbitraryCESized n | n <= 0 = oneof [ return ImagUnit
>                                     , ToComplex <$> arbitrary
>                                     , fromCS <$> arbitrary
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
>   quickCheck $ expectFailure propAssocPlus
>   quickCheck $ expectFailure propAssocTimes
>   quickCheck $ expectFailure propDistTimesPlus

> propAssocSmall :: Int -> Int -> Int -> Bool
> propAssocSmall m n k = propAssocAdd (fromIntegral m) (fromIntegral n) (1/fromIntegral k)

> instance Arbitrary ComplexD where
>   arbitrary = fmap (\(CS p) -> CD p) arbitrary



> eqFun2 :: Eq c => (a -> b -> c) -> (a -> b -> c) -> (a -> b -> Bool)
> eqFun2 f g a b  =  f a b == g a b

> cmpEvalEnv :: Int -> Env Int String -> Bool
> cmpEvalEnv = eqFun2 lookup (flip evalEnv)

> testEvalEnv :: IO ()
> testEvalEnv = quickCheck cmpEvalEnv
