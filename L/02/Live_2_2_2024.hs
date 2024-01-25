-- Lecture 2.2: First order logic (FOL)
-- ====================================

type VarName = String
type PredName = String

data Dom where
  Zero :: Dom
  Succ :: Dom -> Dom
  Plus :: Dom -> Dom -> Dom
  Var  :: VarName -> Dom
  deriving Eq
  
eval :: Dom -> (VarName -> Integer) -> Integer
eval Zero _         = 0
eval (Succ e) table = eval e table + 1
eval (Plus e1 e2) table =
  (eval e1 table) + (eval e2 table)
eval (Var name) table = table name

data FOL where
  T :: FOL    -- True
  F :: FOL    -- False
  Not :: FOL -> FOL
  And, Or, Implies :: FOL -> FOL -> FOL
  FOLVar :: VarName -> FOL
  Forall, Exists :: VarName -> FOL -> FOL
  Pred :: PredName -> [Dom] -> FOL
  deriving Eq

{-
data FOL = T | F | Not FOL | And FOL FOL | Or FOL FOL  | FOLVar VarName
-}

x = Var "x"
y = Var "y"


example1 =
  Forall "x"
  (Forall "y"
        (Pred "=" [Plus x y,
                   Plus y x]))

{-  
eval :: FOL -> (VarNames -> Bool) -> Bool
eval (Forall x p) _ = does not work, because Dom is infinite
-}

check :: Proof -> FOL -> Bool
check (AndIntro proofA proofB) (And a b) =
  check proofA a && check proofB b
check (OrIntroL proofA) (Or a b)      = check proofA a
check (OrIntroR proofB) (Or a b)      = check proofB b
check (ImpliesIntro f)  (Implies a b) = check (f (Assume a)) b
check (Assume a)        b             = a == b
check (AndElimL (And a b)) c          = a == c
check (AndElimR (And a b)) c          = b == c

a = FOLVar "a"
b = FOLVar "b"

exampleFOL1 :: FOL
exampleFOL1 = Implies (And a b) (And b a)

exampleProof1 :: Proof
exampleProof1 = ImpliesIntro (\ (Assume aAndB) ->
                                 AndIntro (AndElimR aAndB)
                                          (AndElimL aAndB))

data Proof where
  AndIntro :: Proof -> Proof -> Proof
  AndElimL :: FOL  -> Proof           -- And a b -> a
  AndElimR :: FOL  -> Proof           -- And a b -> b
  OrIntroL :: Proof -> Proof 
  OrIntroR :: Proof -> Proof
  ImpliesIntro :: (Proof -> Proof) -> Proof
  Assume  :: FOL -> Proof     -- private/do not use

-- prepared show instances, not written during the lecture
  
instance Show FOL where
--  show (Con b) = show b
  show (Or e1 e2) = "(" ++ show e1 ++ " or " ++ show e2 ++ ")"
  show (And e1 e2) = "(" ++ show e1 ++ " and " ++ show e2 ++ ")"
  show (Implies e1 e2) = "(" ++ show e1 ++ " => " ++ show e2 ++ ")"
  show (Not e) = "not (" ++ show e ++ ")"
  show (FOLVar e) = "variable " ++ show e
  show (Forall x e) = "(Forall " ++  show x ++ ". " ++ show e ++ ")"
  show (Exists x e) = "(Exists " ++  show x ++ ". " ++ show e ++ ")"
  show (Pred "=" [x,y]) = show x ++ "=" ++ show y 
  show (Pred "<" [x,y]) = show x ++ "<" ++ show y 
  show (Pred name args) = name ++ " " ++ foldr (\ arg stringSoFar -> show arg ++ " " ++ stringSoFar) "" args



instance Show Dom where
  show (Var x) = x
  show (Plus x y) = show x ++ "+" ++ show y
  show (Succ x) = "(Succ " ++ show x ++ ")"
  show Zero = "0"
