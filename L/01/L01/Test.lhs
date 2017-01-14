> module DSLsofMath.L01.Test where
> import DSLsofMath.L01
> import Test.QuickCheck

> instance (Num r, Arbitrary r) => Arbitrary (ComplexS r) where
>   arbitrary = arbitraryCS arbitrary
>   shrink = shrinkCS shrink

> arbitraryCS :: Gen (r , r) -> Gen (ComplexS r)
> arbitraryCS arb = CS <$> arb

> shrinkCS :: Num r => ((r , r) -> [(r , r)]) -> (ComplexS r -> [ComplexS r])
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
> shrinkCE (e@(Plus  l r)) = fromCS (evalE e) :
>   [l, r] ++ [Plus  l r' | r' <- shrink r]
>          ++ [Plus  l' r | l' <- shrink l]
> shrinkCE (e@(Times l r)) = fromCS (evalE e) :
>   [l, r] ++ [Times l r' | r' <- shrink r]
>          ++ [Times l' r | l' <- shrink l]

> main = do
>   quickCheck propFromCS
>   quickCheck propAssocPlus
>   quickCheck propAssocTimes
>   quickCheck propDistTimesPlus

> propAssocSmall :: Int -> Int -> Int -> Bool
> propAssocSmall m n k = propAssocAdd (fromIntegral m) (fromIntegral n) (1/fromIntegral k)
