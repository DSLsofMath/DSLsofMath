\begin{code}
{-# LANGUAGE GADTs #-}
module Live_2_1 where
\end{code}

Propositional Calculus = Prop = satslogik (svenska)
  (from Chapter 2 in the book)
\begin{code}
data Prop  =  Con      Bool
           |  Not      Prop
           |  And      Prop  Prop
           |  Or       Prop  Prop
           |  Implies  Prop  Prop
           |  Name     Name
type Name = String

p1, p2, p3, p4 :: Prop
p1 = And  (Name "a")  (Not (Name "a"))
p2 = Or   (Name "a")  (Not (Name "a"))
p3 = Implies  (Name "a")  (Name "b")
p4 = Implies  (And a b)   (And b a)
  where a = Name "a"; b = Name "b"


type Syn = Prop
type Sem' = Bool
type Sem = Tab -> Sem'
type Tab = Name -> Bool
eval :: Syn -> Sem
eval (Con b)          = conS b
eval (Not      e)     = notS  (eval e)
eval (And      e1 e2) = andS  (eval e1) (eval e2)
eval (Or       e1 e2) = orS   (eval e1) (eval e2)
eval (Implies  e1 e2) = impS  (eval e1) (eval e2)
eval (Name n)         = nameS n

-- eval "byter C mot cS" för alla C i {Con, Not, And, Or, Implies}

conS' :: Bool -> Sem'      -- Sem'=Bool
conS' = id
notS' :: Sem' -> Sem'      -- Bool -> Bool
notS' = not
andS' :: Sem' -> Sem' -> Sem'
andS' = (&&)
orS'  :: Sem' -> Sem' -> Sem'
orS' = (||)
impS' :: Sem' -> Sem' -> Sem'
impS' = (==>)

-- andS ::      Sem'  ->         Sem'  ->         Sem'
andS :: (Tab -> Sem') -> (Tab -> Sem') -> (Tab -> Sem')
andS = lift2 andS'
orS  = lift2 orS'
impS = lift2 impS'
conS :: Bool -> Tab -> Bool
conS = lift0
notS = lift1 notS'
nameS :: Name -> Sem
nameS n tab = tab n

-- generalisera!

lift0 ::  a            -> (t -> a)
lift1 :: (a -> b)      -> (t -> a) -> (t -> b)
lift2 :: (a -> b -> c) -> (t -> a) -> (t -> b) -> (t -> c)

lift0 nuOp      = \_ -> nuOp
lift1 unOp  f   = \x -> unOp  (f x)
lift2 binOp f g = \x -> binOp (f x) (g x)

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> x = x

testTab "a" = True
testTab "b" = False
testTab _   = False

test1 = eval p1 testTab
test2 = eval p2 testTab
test3 = eval p3 testTab
test4 = eval p4 testTab
\end{code}
