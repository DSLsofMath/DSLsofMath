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
           |  Name     Name
type Name = String

p1, p2, p3, p4 :: Prop
p1 = And  (Name "a")  (Not (Name "a"))
p2 = Or   (Name "a")  (Not (Name "a"))
p3 = Implies  (Name "a")  (Name "b")
p4 = Implies  (And a b)   (And b a)
  where a = Name "a"; b = Name "b"

type Syn = Prop
type SimpleSem = Bool  -- first attempt
type Tab = Name -> SimpleSem  -- String -> Bool
type Sem = Tab -> SimpleSem

eval :: Syn -> Sem
eval (Con b)          = conS b
eval (Not      e)     = notS  (eval e)
eval (And      e1 e2) = andS  (eval e1) (eval e2)
eval (Or       e1 e2) = orS   (eval e1) (eval e2)
eval (Implies  e1 e2) = impS  (eval e1) (eval e2)
eval (Name n)         = nameS n

-- eval "replaces contructor C by function cS" for all C i the type Prop

conS :: Bool -> Sem  -- Sem=Tab->Bool
conS c = lift0 (conS' c)
notS :: Sem -> Sem
notS = lift1 notS'
andS :: Sem -> Sem -> Sem
andS = lift2 andS'
orS  :: Sem -> Sem -> Sem
orS = lift2 orS'
impS :: Sem -> Sem -> Sem
impS = lift2 impS'
-- nameS :: Name -> Sem
-- nameS :: Name -> Tab -> Bool
-- nameS :: Name -> (Name -> SimpleSem) -> Bool
nameS :: String -> (String -> Bool) -> Bool
nameS n tab = tab n

conS' :: Bool -> SimpleSem  -- SimpleSem=Bool
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


-- Q: How can you prove that Prop -> Bool cannot work as semantics?
{- A: Proof sketch:
let  p = Name "a" 
then for any function
  e : Prop -> Bool
we have
  e p = True
or
  e p = False
but a correct semantics should follow the assignment function:
with aF = const False  -- so that a=F
we should return True and with
     aT = const True   -- so that a=T
we should return False.

No function e can return both True and False for the same input p,
thus we need to change the type.
-}

-- generalise!

lift0 ::  a            -> (t -> a)
lift1 :: (a -> b)      -> (t -> a) -> (t -> b)
lift2 :: (a -> b -> c) -> (t -> a) -> (t -> b) -> (t -> c)
-- a=b=c=Bool then lift2 (&&) :: (t -> a) -> (t -> b) -> (t -> c)
--                where t = Tab -> Bool
lift0 op0     = \tab -> op0
lift1 op1 f   = \tab -> op1 (f tab)
lift2 op2 f g = \tab -> op2 (f tab) (g tab)


testTab "a" = True
testTab "b" = False
testTab _   = False

test1 = eval p1 testTab == False
test2 = eval p2 testTab == True
test3 = eval p3 testTab == False
test4 = eval p4 testTab == True
\end{code}
