\begin{code}
module Live_2_1 where
\end{code}

Propositional Calculus = Prop
  (from Chapter 2 in the book)
\begin{code}
data Prop  =  Con      Bool
           |  Not      Prop
           |  And      Prop  Prop
           |  Or       Prop  Prop
           |  Implies  Prop  Prop
           |  Name     Namn
type Namn = String

p1, p2, p3, p4 :: Prop
p1 = And  (Name "a")  (Not (Name "a"))
p2 = Or   (Name "a")  (Not (Name "a"))
p3 = Implies  (Name "a")  (Name "b")
p4 = Implies  (And a b)   (And b a)
  where a = Name "a"; b = Name "b"

type Syn = Prop
type Sem = Bool  -- first attempt

eval :: Syn -> Sem
eval (Con b)          = conS b
eval (Not      e)     = notS  (eval e)
eval (And      e1 e2) = andS  (eval e1) (eval e2)
eval (Or       e1 e2) = orS   (eval e1) (eval e2)
eval (Implies  e1 e2) = impS  (eval e1) (eval e2)
eval (Name n)         = nameS n

-- eval "replaces contructor C by function cS" for all C i the type Prop

conS :: Bool -> Sem
notS :: Sem -> Sem
andS :: Sem -> Sem -> Sem
orS  :: Sem -> Sem -> Sem
impS :: Sem -> Sem -> Sem

(conS, notS, andS, orS, impS) = error "TODO: implement semantic functions"

(==>) :: Bool -> Bool -> Bool
(==>) = error "TODO: (==>)"

nameS :: Namn -> Sem
nameS = error "TODO: nameS"







-- generalise!

lift0 ::  a            -> (t -> a)
lift1 :: (a -> b)      -> (t -> a) -> (t -> b)
lift2 :: (a -> b -> c) -> (t -> a) -> (t -> b) -> (t -> c)

lift0 = error "TODO: lift0"
lift1 = error "TODO: lift1"
lift2 = error "TODO: lift2"


\end{code}
testTab "a" = True
testTab "b" = False
testTab _   = False

test1 = eval p1 testTab
test2 = eval p2 testTab
test3 = eval p3 testTab
test4 = eval p4 testTab
