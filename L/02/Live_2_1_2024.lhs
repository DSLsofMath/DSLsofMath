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
  deriving (Show)
type Namn = String

p1, p2, p3, p4 :: Prop
p1 = And  (Name "a")  (Not (Name "a"))
p2 = Or   (Name "a")  (Not (Name "a"))
p3 = Implies  (Name "a")  (Name "b")
p4 = Implies  (And a b)   (And b a)
  where a = Name "a"; b = Name "b"

type Syn = Prop
type Sem = Table -> Bool  -- second attempt
type Table = String -> Bool
type SimpleSem = Bool  -- first attempt

eval :: Syn -> Sem
eval (Con b)          = conS b
eval (Not      e)     = notS  (eval e)
eval (And      e1 e2) = andS  (eval e1) (eval e2)
eval (Or       e1 e2) = orS   (eval e1) (eval e2)
eval (Implies  e1 e2) = impS  (eval e1) (eval e2)
eval (Name n)         = nameS n

-- eval "replaces contructor C by function cS" for all C i the type Prop

-- conS :: Bool -> Sem
conS :: Bool -> Table -> Bool
conS b _tab = b
-- notS :: Sem -> Sem
-- notS :: (Table -> Bool) -> (Table -> Bool)
notS :: (Table -> Bool) -> Table -> Bool
-- notS f tab = not (f tab)  -- same as the below
notS f = not . f 
-- andS :: Sem -> Sem -> Sem
andS :: (Table -> Bool) -> (Table -> Bool) -> (Table -> Bool)
andS f g = lift2 (&&) f g 
orS  :: Sem -> Sem -> Sem
orS = lift2 (||)
impS :: Sem -> Sem -> Sem
impS = lift2 (==>)

mytab "a" = False
mytab "b" = True
mytab _   = False

conS' :: Bool -> SimpleSem
conS' = id
notS' :: SimpleSem -> SimpleSem
notS' = not
andS' :: SimpleSem -> SimpleSem -> SimpleSem
andS' = (&&)
orS'  :: SimpleSem -> SimpleSem -> SimpleSem
orS' = (||)
impS' :: SimpleSem -> SimpleSem -> SimpleSem
impS' = (==>)

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> x = x

nameS :: Namn -> Sem
nameS n tab = tab n







-- generalise!

lift0 ::  a            -> (t -> a)
lift1 :: (a -> b)      -> (t -> a) -> (t -> b)
lift2 :: (a -> b -> c) -> (t -> a) -> (t -> b) -> (t -> c)

lift0 = error "TODO: lift0"
lift1 = error "TODO: lift1"
lift2 op f g = h
  where h tab = op (f tab) (g tab) 



\end{code}
testTab "a" = True
testTab "b" = False
testTab _   = False

test1 = eval p1 testTab
test2 = eval p2 testTab
test3 = eval p3 testTab
test4 = eval p4 testTab
