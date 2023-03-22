{-# LANGUAGE GADTs #-}
module DSLsofMath_Exam_202303_P1_Algebra_Field where
import qualified Prelude
import Prelude(Bool(..), (&&), (/=), Double, String, (==), map, id)
-- a)
class Field f where
    add     :: f -> f -> f
    mul     :: f -> f -> f
    zero    :: f
    one     :: f
    negate  :: f -> f
    recip   :: f -> f

-- b)
data F v where
    Add     :: F v -> F v -> F v
    Mul     :: F v -> F v -> F v
    Zero    :: F v
    One     :: F v
    Negate  :: F v -> F v
    Recip   :: F v -> F v
    Var     :: v -> F v

instance Field (F v) where
    add     = Add
    mul     = Mul
    zero    = Zero
    one     = One
    negate  = Negate
    recip   = Recip

-- c)
instance Field Double where
    add     = (Prelude.+)
    mul     = (Prelude.*)
    zero    = 0
    one     = 1
    negate  = Prelude.negate
    recip   = Prelude.recip

xor :: Bool -> Bool -> Bool
xor = (/=)
instance Field Bool where
    add     = xor
    mul     = (&&)
    zero    = False
    one     = True
    negate  = id
    recip   = id

-- Other possibilities include rational and complex numbers, but not integers.

-- d)
eval :: Field f => (v -> f) -> F v -> f
eval env = e where
    e (Add x y)   = add  (e x)  (e y)
    e (Mul x y)   = mul  (e x)  (e y)
    e Zero        = zero
    e One         = one
    e (Negate x)  = negate  (e x)
    e (Recip x)   = recip   (e x)
    e (Var v)     = env v

-- e)

evalD :: (v -> Double)  ->  (F v -> Double)
evalD = eval
evalB :: (v -> Bool)    ->  (F v -> Bool)
evalB = eval

e1, e2, e3 :: F String
e1 = Mul (Var "x") (Recip (Var "x"))      -- |x/x|
e2 = Recip (Add (Add One One) One)        -- |1/3|
e3 = Add (Var "y") (Negate (Var "z"))     -- |y-z|

envD :: String -> Double
envD "x" = 3.14
envD "y" = 7
envD "z" = 2

envB :: String -> Bool
envB "x" = True
envB "y" = False
envB "z" = False

tests = (  map (evalD envD)  [e1,e2,e3]  ==  [1, 1 Prelude./ 3, 5]
        ,  map (evalB envB)  [e1,e2,e3]  ==  [True, True, False] )
