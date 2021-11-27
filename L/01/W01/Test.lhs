> module DSLsofMath.W01.Test where
> import DSLsofMath.W01
> import DSLsofMath.ComplexSem (ComplexD (CD))
> import DSLsofMath.ComplexSyn
> import DSLsofMath.CSem hiding ((/))
> import Test.QuickCheck
> import Prelude hiding (flip)

We may also need the Haskell standard library version for some testing later:

> import qualified Data.Complex as DC

> fromC (C (x , y)) = Add (ToComplex x) (Mul (ToComplex y) I2)

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
> arbitraryCESized n | n <= 0 = oneof [ return I2
>                                     , ToComplex <$> arbitrary
>                                     , fromC <$> arbitrary
>                                     ]
>                    | otherwise = do
>   op <- elements [Add, Mul]
>   let arbHalf = arbitraryCESized (n `div` 2)
>   op <$> arbHalf <*> arbHalf


> shrinkCE :: ComplexE -> [ComplexE]
> shrinkCE I2 = [ToComplex 0]
> shrinkCE (ToComplex r) = ToComplex <$> shrink r
> shrinkCE (Mul r I2) = r:shrink r
> shrinkCE (Add  l (Mul r I2)) = [l, Mul r I2]
> shrinkCE (e@(Add  l r)) = fromCD (evalE e) :
>   [l, r] ++ [Add  l r' | r' <- shrink r]
>          ++ [Add  l' r | l' <- shrink l]
> shrinkCE (e@(Mul l r)) = fromCD (evalE e) :
>   [l, r] ++ [Mul l r' | r' <- shrink r]
>          ++ [Mul l' r | l' <- shrink l]

> main = do
>   quickCheck propFromCD
>   quickCheck $ expectFailure (propAssocAdd    :: REAL->REAL->REAL->Bool)
>   quickCheck $ expectFailure (propAssocMul    :: REAL->REAL->REAL->Bool)
>   quickCheck $ expectFailure (propDistMulAdd  :: REAL->REAL->REAL->Bool)

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
