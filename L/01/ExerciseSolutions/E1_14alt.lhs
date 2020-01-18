\begin{code}
module E1_SemiRing where
import Data.Maybe
import Test.QuickCheck
\end{code}

Solution for exercise E1_SemiRing:

\begin{code}
data SR v  = Var v           -- | Variables of type |v|
           | SR v :+: SR v   -- | Addition
           | Zero            -- | Identity element for |:+:|
           | SR v :*: SR v   -- | Multiplication
           | One             -- | Identity element for |:*:|
           deriving (Read, Show, Eq, Ord)

-- ^ Type synonym for environment mapping variables of type v to
-- values of type a.
type Env v a = [(v, a)]

-- ^ We now define an evaluator taking an expression and environment
-- and mapping it to any numeric type (for example Integer, Double)
evalSR :: (Eq v, Num a) => SR v -> Env v a -> a
evalSR exp env =
  case exp of
    Var v      ->  fromMaybe (error "no such variable") $ lookup v env
    Zero       ->  0
    One        ->  1
    e1 :+: e2  ->  evalSR e1 env  +  evalSR e2 env
    e1 :*: e2  ->  evalSR e1 env  *  evalSR e2 env
\end{code}

Now we can use QuickCheck to test if our evaluator respects the semiring laws:
We use integers for simplicity and to avoid ambiguous types later on when
running the tests.

In general, any instance of |Num| should work though if they satisfy
the type class laws for |Num|. Note that |Double| does not satisfy
this, due to rounding issues.

\begin{code}
-- ^ Checks that :+: is commutative for integers.
checkComm :: Integer -> Integer -> Bool
checkComm a b =
    evalSR (Var "a" :+: Var "b") env  ==
    evalSR (Var "b" :+: Var "a") env
  where env = [("a", a), ("b", b)]

-- ^ Checks that given operation is associative
checkAssoc ::  (SR String -> SR String -> SR String)
               -> Integer -> Integer -> Integer -> Bool
checkAssoc op a b c =
    evalSR ((Var "a" `op` Var "b") `op` Var "c") env  ==
    evalSR (Var "a" `op` (Var "b" `op` Var "c")) env
  where env = [("a", a), ("b", b), ("c", c)]

-- ^ Checks that given element is left identity for given operation
checkIdentityL ::  SR String -> (SR String -> SR String -> SR String)
                   -> Integer -> Bool
checkIdentityL elem op a = evalSR (elem `op` Var "a") env == a
  where env = [("a", a)]

-- ^ Same for right identity.
checkIdentityR ::  SR String -> (SR String -> SR String -> SR String)
                   -> Integer -> Bool
checkIdentityR elem op a = evalSR (elem `op` Var "a") env == a
  where env = [("a", a)]

-- ^ Checks that :*: left-distributes over :+:
checkDistribL :: Integer -> Integer -> Integer -> Bool
checkDistribL a b c =
    evalSR (va :*: (vb :+: vc)) env ==
    evalSR ((va :*: vb) :+: (va :*: vc)) env
  where  env = [("a", a), ("b", b), ("c", c)]
         va = Var "a"
         vb = Var "b"
         vc = Var "c"

-- ^ Checks that :*: right-distributes over :+:
checkDistribR :: Integer -> Integer -> Integer -> Bool
checkDistribR a b c =
    evalSR ((va :+: vb) :*: vc) env  ==
    evalSR ((va :*: vc) :+: (vb :*: vc)) env
  where  env = [("a", a), ("b", b), ("c", c)]
         va = Var "a"
         vb = Var "b"
         vc = Var "c"

-- ^ Checks that 0 is an annihilating element for :*:
checkZeroAnnihilatesL :: Integer -> Bool
checkZeroAnnihilatesL a = evalSR (Zero :*: Var "a") env == evalSR Zero env
  where env = [("a", a)]

checkZeroAnnihilatesR :: Integer -> Bool
checkZeroAnnihilatesR a = evalSR (Var "a" :*: Zero) env == evalSR Zero env
  where env = [("a", a)]

checkAll :: IO ()
checkAll = do
  check "* is associative" $ checkAssoc (:*:)
  check "+ is associative" $ checkAssoc (:+:)
  check "+ is commutative" checkComm
  check "0 is left identity for +" $ checkIdentityL Zero (:+:)
  check "0 is right identity for +" $ checkIdentityR Zero (:+:)
  check "1 is left identity for *" $ checkIdentityL One (:*:)
  check "1 is right identity for *" $ checkIdentityR One (:*:)
  check "* left-distributes over +" checkDistribL
  check "* right-distributes over +" checkDistribR
  check "Multiplying by 0 annihilates (left)" checkZeroAnnihilatesL
  check "Multiplying by 0 annihilates (right)" checkZeroAnnihilatesR
  where check msg test = putStrLn msg >> quickCheck test

main = checkAll
\end{code}
