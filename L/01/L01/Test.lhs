> module DSLsofMath.L01.Test where
> import DSLsofMath.L01
> import Test.QuickCheck

> instance Arbitrary ComplexD where
>   arbitrary = arbitraryCD
>   shrink = shrinkCD

> arbitraryCD :: Gen ComplexD
> arbitraryCD = CD <$> arbitrary

> shrinkCD :: ComplexD -> [ComplexD]
> shrinkCD (CD (p@(x, y))) = [CD (0, 0), CD (x,0), CD (0, y)] ++ map CD (shrink p)


> instance Arbitrary ComplexE where
>   arbitrary = sized arbitraryCESized
>   shrink = shrinkCE

> arbitraryCESized :: Int -> Gen ComplexE
> arbitraryCESized n | n <= 0 = oneof [ return ImagUnit
>                                     , ToComplex <$> arbitrary
>                                     , fromCD <$> arbitrary
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
>   quickCheck propAssocPlus
>   quickCheck propAssocTimes
>   quickCheck propDistTimesPlus

> propAssocSmall :: Int -> Int -> Int -> Bool
> propAssocSmall m n k = propAssocAdd (fromIntegral m) (fromIntegral n) (1/fromIntegral k)
