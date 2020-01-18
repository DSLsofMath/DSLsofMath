{-# LANGUAGE StandaloneDeriving #-}
-- We copy the definition here, so we can add a variable constructor
-- without modifying the lecture files (since this is another exercise).
-- For simplicity, we stick to strings as variables for this exercise.
import DSLsofMath.W01 (Env)
import Data.Maybe ( fromMaybe )
import Test.QuickCheck ( Arbitrary (arbitrary), Gen, sized, oneof )

-- We add a constructor for variables, so that simplification can't
-- just evaluate the expression completely
data ComplexSyn r = FromCartesian r r
                  | ComplexSyn r  :+:  ComplexSyn r
                  | ComplexSyn r  :*:  ComplexSyn r
                  | Var String
  deriving (Eq, Show)

-- This function implements a single evaluation step:
simplifyStep :: (Num r, Eq r) => ComplexSyn r -> ComplexSyn r
simplifyStep (FromCartesian 0 0 :+: a) = a
simplifyStep (a :+: FromCartesian 0 0) = a
simplifyStep (FromCartesian 1 0 :*: a) = a
simplifyStep (a :*: FromCartesian 1 0) = a
simplifyStep (FromCartesian 0 0 :*: a) = FromCartesian 0 0
simplifyStep (a :*: FromCartesian 0 0) = FromCartesian 0 0
simplifyStep (a :*: b) = simplifyStep a :*: simplifyStep b
simplifyStep (a :+: b) = simplifyStep a :+: simplifyStep b
simplifyStep x = x

-- We can then iterate this, until the expression no longer changes:
simplify :: (Num r, Eq r) => ComplexSyn r -> ComplexSyn r
simplify exp | exp == exp'  = exp
             | otherwise    = simplify exp'
  where exp' = simplifyStep exp

-- Exercise to the reader: Implement more simplification steps and then check that
-- they don't modify the semantics of the expression

-- We can define an evaluation function taking a mapping for variables
-- and returns a complex number represented as a pair:
eval :: (Num r, Eq r) => Env String (r, r) -> ComplexSyn r -> (r, r)
eval env exp =
  case exp of
    Var x -> fromMaybe (error $ "No such variable: " ++ x) $ lookup x env
    FromCartesian r i -> (r, i)
    e1 :+: e2 -> let (r1, i1) = eval env e1
                     (r2, i2) = eval env e2
                 in (r1 + r2, i1 + i2)
    e1 :*: e2 -> let (a, b) = eval env e1
                     (c, d) = eval env e2
                 in (a*c - b*d, a*d + b*c)

-- A property to check that simplifying preserves the semantics:
simplifyPreservesSemantics :: (Eq r, Arbitrary r, Num r) => ComplexSyn r -> Bool
simplifyPreservesSemantics exp = eval [] exp == eval [] (simplify exp)

simplifyPreservesSemanticsInteger :: ComplexSyn Integer -> Bool
simplifyPreservesSemanticsInteger = simplifyPreservesSemantics

-- We define the size of an expression to check that we actually simplify expressions:
exprSize :: ComplexSyn r -> Integer
exprSize (FromCartesian a b) = 1
exprSize (Var x) = 1
exprSize (a :*: b) = exprSize a + exprSize b
exprSize (a :+: b) = exprSize a + exprSize b

simplifySimplifies :: (Eq r, Arbitrary r, Num r) => ComplexSyn r -> Bool
simplifySimplifies expr = exprSize (simplify expr) <= exprSize expr

----------------
-- Writing generators is not part of the course.

-- We define a QuickCheck generator for sized expression to avoid
-- non-terminating generators; also, we make it slightly more likely
-- to generate values with a 0 as the imaginary part:
expr :: (Num r, Arbitrary r) => Int -> Gen (ComplexSyn r)
expr 0 = oneof [ FromCartesian <$> arbitrary <*> arbitrary
               , FromCartesian <$> arbitrary <*> pure 0 ]
expr n = oneof [ FromCartesian <$> arbitrary <*> arbitrary
               , (:*:) <$> subexp <*> subexp
               , (:+:) <$> subexp <*> subexp ]
  where subexp = expr (n `div` 2)

instance (Arbitrary r, Num r) => Arbitrary (ComplexSyn r) where
  arbitrary = sized expr
