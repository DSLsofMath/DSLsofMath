{-# LANGUAGE GADTs #-}
-- Algebraic structure: Boolean Algebra
module P1 where
import Test.QuickCheck -- not needed on the exam

-- (a)
class BoolAlg b where
  meet    :: b -> b -> b
  join    :: b -> b -> b
  nott    :: b -> b
  bottom  :: b
  top     :: b

-- (b)

data BA v where
  Meet    :: BA v -> BA v -> BA v
  Join    :: BA v -> BA v -> BA v
  Nott    :: BA v -> BA v
  Bottom  :: BA v
  Top     :: BA v
  V       :: v -> BA v

instance BoolAlg (BA v) where
  meet    =  Meet   
  join    =  Join   
  nott    =  Nott   
  bottom  =  Bottom 
  top     =  Top    

-- (c)

instance BoolAlg Bool where
  meet    = (&&)
  join    = (||)
  nott    = not
  bottom  = False
  top     = True

data SubAB = Empty | A | B | AB deriving (Eq, Show)

instance BoolAlg SubAB where
  meet    =  meetAB    
  join    =  joinAB    
  nott    =  nottAB    
  bottom  =  bottomAB  
  top     =  topAB     

meetAB Empty  _      = Empty
meetAB _      Empty  = Empty
meetAB x      AB     = x
meetAB AB     y      = y
meetAB A      A      = A
meetAB A      B      = Empty
meetAB B      A      = Empty
meetAB B      B      = B

joinAB Empty  y      = y
joinAB x      Empty  = x
joinAB x      AB     = AB
joinAB AB     y      = AB
joinAB A      A      = A
joinAB A      B      = AB
joinAB B      A      = AB
joinAB B      B      = B

nottAB   Empty = AB
nottAB   A     = B
nottAB   B     = A
nottAB   AB    = Empty

bottomAB = Empty

topAB = AB

-- (d)

eval :: BoolAlg b => (v->b) -> BA v -> b
eval v = ev where
  ev (Meet x y)  = meet (ev x) (ev y)
  ev (Join x y)  = join (ev x) (ev y)   
  ev (Nott x)    = nott (ev x)
  ev (Bottom)    = bottom
  ev (Top)       = top     
  ev (V n)       = v n

-- (e)
evalAB :: (v->SubAB) -> BA v -> SubAB
evalAB = eval

e1, e2, e3 :: BA Int
e1 = V 1
e2 = Meet e1 (Join e1 (Nott e1)) -- should be equiv. to e1
e3 = Meet e2 (Nott e2) -- should be equiv to 0

aF :: Int -> SubAB
aF = const B

ev :: BA Int -> SubAB
ev = evalAB aF

test :: Bool
test = map ev [e1, e2, e3] ==
              [B, B, Empty]

-- ----------------------------------------------------------------
-- Not part of the exam question:

propAssoc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
propAssoc op x y z = op (op x y) z == op x (op y z)
propComm :: Eq a => (a -> a -> a) -> a -> a -> Bool
propComm  op x y   = op x y == op y x
propDist :: Eq t => (t -> t -> t) -> (t -> t -> t) -> t -> t -> t -> Bool
propDist   op1 op2 x y z  = propDistL op1 op2 x y z && propDistR op1 op2 x y z 
propDistL  op1 op2 x y z = op1 (op2 x y) z == op2 (op1 x z) (op1 y z)
propDistR  op1 op2 x y z = op1 x (op2 y z) == op2 (op1 x y) (op1 x z)

--  |∨| and |∧| are associative, commutative and distribute over each other;
propAssocMeet, propAssocJoin, propDistJoinMeet, propDistMeetJoin
  :: (Eq a, BoolAlg a) => a -> a -> a -> Bool
propAssocMeet = propAssoc meet
propAssocJoin = propAssoc join
propDistJoinMeet = propDist join meet
propDistMeetJoin = propDist meet join

propCommMeet, propCommJoin :: (Eq a, BoolAlg a) => a -> a -> Bool
propCommMeet  = propComm meet
propCommJoin  = propComm join

-- |a ∨ 0 = a| and |a ∧ 1 = a|; |a ∨ ¬a = 1| and |a ∧ ¬a = 0|.
propJoinBottom, propMeetTop, propJoinNot, propMeetNot
  :: (Eq a, BoolAlg a) => a -> Bool
propJoinBottom a = join a bottom == a
propMeetTop a =    meet a top == a
propJoinNot a = join a (nott a) == top
propMeetNot a = meet a (nott a) == bottom

propAssocMeetAB, propAssocJoinAB, propDistJoinMeetAB, propDistMeetJoinAB
  :: SubAB -> SubAB -> SubAB -> Bool
propAssocMeetAB     = propAssocMeet
propAssocJoinAB     = propAssocJoin   
propDistJoinMeetAB  = propDistJoinMeet
propDistMeetJoinAB  = propDistMeetJoin

propCommMeetAB, propCommJoinAB :: SubAB -> SubAB -> Bool
propCommMeetAB  =  propCommMeet
propCommJoinAB  =  propCommJoin

propJoinBottomAB, propMeetTopAB, propJoinNotAB, propMeetNotAB :: SubAB -> Bool
propJoinBottomAB  = propJoinBottom 
propMeetTopAB     = propMeetTop   
propJoinNotAB     = propJoinNot   
propMeetNotAB     = propMeetNot   

main = do
  quickCheck propAssocMeetAB
  quickCheck propAssocJoinAB    
  quickCheck propCommMeetAB     
  quickCheck propCommJoinAB     
  quickCheck propDistJoinMeetAB
  quickCheck propDistMeetJoinAB 
  quickCheck propJoinBottomAB   
  quickCheck propMeetTopAB      
  quickCheck propJoinNotAB      
  quickCheck propMeetNotAB      

instance Arbitrary SubAB where arbitrary = elements [Empty,A,B,AB]
