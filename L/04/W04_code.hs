{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}   -- , RebindableSyntax
module DSLsofMath.W04 where
import Prelude hiding (Monoid, even, Num(..), recip, pi, sin, cos, exp, (^), (+))
import qualified Prelude (pi)
import Numeric.Natural (Natural)
import DSLsofMath.FunExp hiding (eval, eval')
import qualified DSLsofMath.FunExp as FunExp
import DSLsofMath.Algebra
-- import DSLsofMath.Derive (derive)
-- import DSLsofMath.W03 (derive)
type ℝ = REAL
type Nat    =  Natural     -- imported from |Numeric.Natural|

class Monoid a where
    unit  ::  a
    op    ::  a -> a -> a

instance Additive Nat        where zero  = 0;  (+) = (+)
instance Multiplicative Nat  where one   = 1;  (*) = (*)

newtype ANat      =  A Nat          deriving (Show, Eq)

instance Monoid ANat where
  unit            =  A zero
  op (A m) (A n)  =  A (m + n)

newtype MNat      =  M Nat          deriving (Show, Eq)

instance Monoid MNat where
  unit            =  M one
  op (M m) (M n)  =  M (m * n)

(/) :: MulGroup a => a -> a -> a
a / b = a * recip b

type Field a = (Ring a, MulGroup a)

data E = Add E E | Mul E E | Con Integer deriving Eq
e1, e2 :: E                             -- | 1 + 2 * 3 |
e1 = Add (Con 1) (Mul (Con 2) (Con 3))  -- | 1 +(2 * 3)|
e2 = Mul (Add (Con 1) (Con 2)) (Con 3)  -- |(1 + 2)* 3 |

even (Add x y)  =  evenAdd (even x) (even y)
even (Mul x y)  =  evenMul (even x) (even y)
even (Con c)    =  evenCon c

evenAdd :: Bool -> Bool -> Bool
evenMul :: Bool -> Bool -> Bool
evenCon :: Integer -> Bool

evenAdd = (==)
evenMul = (||)
evenCon = (0==).(`mod` 2)

isPrimeAdd :: Bool -> Bool -> Bool
isPrimeAdd = error "Can this be done?"
isPrime (Add x y)  =  isPrimeAdd (isPrime x) (isPrime y)

isPrime _ = error "isPrime: Not a homomorphism"

foldE ::  (s -> s -> s) -> (s -> s -> s) -> (Integer -> s) -> (E -> s)
foldE add mul con = rec
  where  rec (Add  x y)  = add  (rec x)  (rec y)
         rec (Mul  x y)  = mul  (rec x)  (rec y)
         rec (Con  i)    = con  i

evalE1 :: E -> Integer
evalE1 = foldE (+) (*) id

evalE2 :: Ring a => E -> a
evalE2 = foldE (+) (*) fromInteger

idE :: E -> E
idE = foldE Add Mul Con

class IntExp t where
  add  ::  t -> t -> t
  mul  ::  t -> t -> t
  con  ::  Integer -> t

foldIE :: IntExp t => E -> t
foldIE = foldE add mul con

instance IntExp Integer  where  add  = (+);   mul  = (*);   con  = id
instance IntExp E        where  add  = Add;   mul  = Mul;   con  = Con

evalE' :: E -> Integer
evalE' = foldIE

idE' :: E -> E
idE' = foldIE

foldList :: (a -> s -> s) -> s -> ([a] -> s)
foldList cons nil = rec
  where  rec (x : xs)  = cons x (rec xs)
         rec []        = nil

checktypes = [foldr, foldList]

pretty :: E -> String

pretty (Add x y)  = prettyAdd (pretty x) (pretty y)
pretty (Mul x y)  = prettyMul (pretty x) (pretty y)
pretty (Con c)    = prettyCon c

prettyAdd :: String -> String -> String
prettyMul :: String -> String -> String
prettyCon :: Integer -> String

instance IntExp String where
  add = prettyAdd
  mul = prettyMul
  con = prettyCon

pretty' :: E -> String
pretty' = foldIE


prettyAdd xs ys  = xs ++ "+" ++ ys
prettyMul xs ys  = xs ++ "*" ++ ys
prettyCon c      = show c

p1, p2 :: String
p1 = pretty e1     -- gives |"1+2*3"|
p2 = pretty e2     -- also  |"1+2*3"|, but should be |"(1+2)*3"|

trouble :: Bool
trouble = p1 == p2

prTop :: E -> String
prTop e =  let (pTop, _, _) = prVersions e
           in pTop

type ThreeVersions = (String, String, String)

paren :: String -> String
paren s = "(" ++ s ++ ")"

prVersions :: E -> ThreeVersions
prVersions = foldE prVerAdd prVerMul prVerCon


prVerAdd :: ThreeVersions -> ThreeVersions -> ThreeVersions
prVerAdd (_xTop, xInA, _xInM) (_yTop, yInA, _yInM) =
  let s = xInA ++ "+" ++ yInA  -- use |InA| because we are ``in |Add|''
  in (s, paren s, paren s)     -- parens needed except at top level

prVerMul :: ThreeVersions -> ThreeVersions -> ThreeVersions
prVerMul (_xTop, _xInA, xInM) (_yTop, _yInA, yInM) =
  let s = xInM ++ "*" ++ yInM  -- use |InM| because we are ``in |Mul|''
  in (s, s, paren s)           -- parens only needed inside |Mul|

prVerCon :: Integer -> ThreeVersions
prVerCon i  | i < 0      =  (s,  ps,  ps)     -- parens needed except at top level
            | otherwise  =  (s,  s,   s)      -- parens never needed
  where  s = show i; ps = paren s

seven :: IntExp a => a;  seven = add (con 3) (con 4)

testI  :: Integer;       testI  = seven
testE  :: E;             testE  = seven
testP  :: String;        testP  = seven

check :: Bool
check = and  [  testI  ==  7
             ,  testE  ==  Add (Con 3) (Con 4)
             ,  testP  ==  "3+4"
             ]

class Generate a where
  generate :: G -> a

data FreeMonoid g   =  Unit
                    |  Op (FreeMonoid g) (FreeMonoid g)
                    |  Generator g deriving Show
instance Monoid (FreeMonoid g) where  unit = Unit;  op = Op

evalM :: Monoid a => (G -> a) -> (FreeMonoid G -> a)

evalM  _   Unit           =  unit
evalM  f   (Op e1 e2)     =  op (evalM f e1) (evalM f e2)
evalM  f   (Generator x)  =  f x

type G = ()
instance Generate        FunExp where  generate () = X

instance Additive        FunExp where  (+)  = (:+:);  zero  = Const 0
instance Multiplicative  FunExp where  (*)  = (:*:);  one   = Const 1

instance AddGroup  FunExp where negate  = Negate -- multiplication by (-1) is also possble (is Negate redundant?)
instance MulGroup  FunExp where recip   = Recip
instance Transcendental FunExp where pi = Const (Prelude.pi); exp = Exp; sin = Sin; cos = Cos

evalIncomplete (e1 :*: e2)  =  evalIncomplete e1 * evalIncomplete e2
evalIncomplete (e1 :+: e2)  =  evalIncomplete e1 + evalIncomplete e2

-- evalIncomplete (Exp e)      =  exp (evalIncomplete e) --
evalIncomplete _ = error "Implemented elsewhere"

type OneVarExp a = (Generate a, Ring a)
evalIncomplete :: FunExp -> (OneVarExp a => a)

varX :: OneVarExp a => a
varX = generate ()
twoX :: OneVarExp a => a
twoX = two * varX

type Func = REAL -> REAL
testFE  :: FunExp;           testFE  = twoX
testFu  :: Func;             testFu  = twoX

instance Generate Func where
  generate () = id

newtype Free c g = Free (forall a. c a => (g -> a) -> a)

embed :: g -> Free c g
embed g = Free (\generate -> generate g)

instance Monoid (Free Monoid g)  where
  unit = Free (\_ -> unit)
  Free f `op` Free g = Free (\x -> f x `op` g x)

extract :: Free Monoid g -> [g]
extract (Free f) = f (\g -> [g])

instance Monoid [a] where
  unit = []
  op = (++)

example :: Free Monoid Int
example = embed 1 `op` embed 10 `op` unit `op` embed 11

-- |>>> extract example|
-- |[1,10,11]|

type DummyFunc = ℝ -> ℝ
eval'  ::  FunExp -> Func
eval'  =   FunExp.eval . derive

type FD a = (a -> a, a -> a)

evalD ::  FunExp  ->  FD REAL
evalD     e       =   (FunExp.eval e, eval' e)

evalDExp ::  FD REAL  ->  FD REAL
evalDExp     (f, f')  =   (exp f, exp f * f')

instance Additive a                      => Additive (FD a)        where
  zero  = zeroFD;  (+)  = addFD
instance (Additive a, Multiplicative a)  => Multiplicative (FD a)  where
  one   = oneFD;   (*)  = mulFD

zeroFD  ::    Additive a                     => FD a
oneFD   :: (  Additive a, Multiplicative a)  => FD a
zeroFD  = (const zero,  const zero)
oneFD   = (const one,   const zero)

addFD   ::    Additive a                     => Dup a -> Dup a -> Dup a
mulFD   :: (  Additive a, Multiplicative a)  => Dup a -> Dup a -> Dup a

addFD  (f, f') (g, g')  =  (f  +  g,  f'      +       g'  )
mulFD  (f, f') (g, g')  =  (f  *  g,  f' * g  +  f *  g'  )

type Dup a      = (a, a)
type DummyFD a  = (a -> a, a -> a)

applyFD ::  a ->  FD a     ->  Dup a
applyFD     c     (f, f')  =   (f c, f' c)

(*?) :: Ring a =>  Dup a -> Dup a -> Dup a
(x, x') *? (y, y')  =  (x * y, x' * y + x * y')

oneDup :: Ring a => Dup a
oneDup = (one, zero)
instance Ring a => Multiplicative (Dup a) where
  one  = oneDup
  (*)  = (*?)

f :: Transcendental a => a -> a
f x = sin x + two * x

fe :: FunExp
fe = f X

drv f = FunExp.eval (derive (f X))

drvFD f x = snd (applyFD x (f (id, const 1)))

f1 :: FD REAL -> FD REAL
f1  = f

drvP f x  =  snd (f (var x))

f2 :: (REAL, REAL) -> (REAL, REAL)
f2  = f

instance Additive a => Additive (Dup a) where
  zero = zeroDup;  (+) = addDup

zeroDup :: Additive a => Dup a
zeroDup = (zero, zero)

addDup :: Additive a => Dup a -> Dup a -> Dup a
addDup (x,x') (y,y') = (x+y,x'+y')

instance AddGroup a => AddGroup (Dup a) where
  negate = negateDup

negateDup :: AddGroup a => Dup a -> Dup a
negateDup (x, x') = (negate x, negate x')

instance (AddGroup a, MulGroup a) => MulGroup (Dup a) where
  recip = recipDup

recipDup :: (AddGroup a, MulGroup a) => Dup a -> Dup a
recipDup (x, x') = (y, y')
  where  y   = recip x
         y'  = negate (y*y) * x'

instance Transcendental a => Transcendental (Dup a) where
  pi   = piDup;  sin  = sinDup;  cos  = cosDup;  exp  = expDup

piDup :: Transcendental a => Dup a
piDup = (pi, zero)

sinDup, cosDup, expDup :: Transcendental a => Dup a -> Dup a
sinDup  (x,x') =  (sin x,           cos x   * x')
cosDup  (x,x') =  (cos x, negate (  sin x)  * x')
expDup  (x,x') =  (exp x,           exp x   * x')

var :: Multiplicative a => a -> Dup a
var x = (x, one)

