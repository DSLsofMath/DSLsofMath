module DSLsofMath.FOL where

type VarT = String
data RatT = RV VarT | FromI Integer | RPlus RatT RatT | RDiv RatT RatT
  deriving Show

evalRat :: RatT -> (VarT -> RatSem) -> RatSem
type RatSem = Rational

evalRat = error "evalRat: todo"

type PSym = String
data FOL  =  Implies FOL FOL | And FOL FOL | Or FOL FOL | Not FOL
          |  FORALL  VarT  FOL    |  EXISTS  VarT  FOL
          |  PName PSym [RatT]    |  Equal  RatT  RatT
  deriving Show

eval0 :: PSym -> [RatSem] -> Bool
eval0 "Equal"     [t1,t2]  =  t1 == t2
eval0 "LessThan"  [t1,t2]  =  t1 < t2
eval0 "Positive"  [t1]     =  t1 > 0

eval :: FOL -> (VarT -> RatSem) -> Bool
eval formula env = ev formula
  where  ev (PName n args)  = eval0 n (map (flip evalRat env) args)
         ev (Equal a b)     = evalRat a env == evalRat b env
         ev (And p q)       = ev p  &&  ev q
         ev (Or  p q)       = ev p  ||  ev q
