\begin{code}
module Live_2_1 where
\end{code}

Propositional Calculus = Prop
  (from Chapter 2 in the book)
\begin{code}
data Prop  =  And  Prop  Prop
           |  Or   Prop  Prop
           |  Not  Prop
           |  Imp  Prop  Prop
           |  Con  Bool
           |  Nam  NameT
  deriving (Show)
type NameT = String

p1, p2, p3, p4 :: Prop
p1 = And  (Nam "a")  (Not (Nam "a"))
p2 = Or   (Nam "a")  (Not (Nam "a"))
p3 = Imp  (Nam "a")  (Nam "b")
p4 = Imp  (And a b)   (And b a)
  where a = Nam "a"; b = Nam "b"

type Syn  = Prop
type Sem1 = Bool  -- first attempt - does not work
type Tab = [(NameT,Bool)]  -- the boolean value for each variable name
type Sem2 = Tab -> Bool  -- second attempt -- works?

-- eval :: Syn -> (Tab -> Bool)
eval :: Syn -> Sem2
eval (And  e1 e2)  = andS2  (eval e1) (eval e2)
eval (Or   e1 e2)  = orS2   (eval e1) (eval e2)
eval (Not  e)      = notS2  (eval e)
eval (Imp  e1 e2)  = impS2  (eval e1) (eval e2)
eval (Con b)       = conS2 b
eval (Nam n)       = nameS2 n

-- eval "replaces constructor C by function cS" for all C i the type Prop

conS :: Bool -> Sem1  -- Prop ~ Sem1
notS :: Sem1 -> Sem1
andS :: Sem1 -> Sem1 -> Sem1
orS  :: Sem1 -> Sem1 -> Sem1
impS :: Sem1 -> Sem1 -> Sem1

conS2 :: Bool -> Sem2  -- Prop ~ Sem2
notS2 :: Sem2 -> Sem2
andS2 :: Sem2 -> Sem2 -> Sem2
orS2  :: Sem2 -> Sem2 -> Sem2
impS2 :: Sem2 -> Sem2 -> Sem2


-- Bool -> Sem2 = Bool -> (Tab -> Bool)
conS2 b _tab = b
-- Sem2 -> Sem2 = (Tab -> Bool) -> (Tab -> Bool) = (Tab -> Bool) -> Tab -> Bool
notS2 p tab = not (p tab)
-- Sem2 -> Sem2 -> Sem2 = (Tab->Bool) -> (Tab->Bool) -> (Tab->Bool)
andS2 p1 p2 = \tab -> p1 tab && p2 tab
orS2  p1 p2 = \tab -> p1 tab || p2 tab
impS2 p1 p2 = \tab -> p1 tab ==> p2 tab

{-
Con :: Bool -> Prop
Not :: Prop -> Prop
And :: Prop -> Prop -> Prop
Or  :: Prop -> Prop -> Prop
Imp :: Prop -> Prop -> Prop
-}
(conS, orS, impS) = error "TODO: implement semantic functions"

notS = not
-- notS False = True
-- notS True = False
-- Bool -> Bool -> Bool
andS = (&&)

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> b = b


-- nameS -- impossible

myLookup :: Tab -> NameT -> Bool
myLookup [] n = error ("Variable "++n++" not found")
myLookup ((a,b):rest) n | n == a = b
                        | otherwise = myLookup rest n

nameS2 :: NameT -> (Tab -> Bool)
nameS2 n tab = myLookup tab n







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

Some pure set theory (relevant for A1).
\begin{code}
newtype Set = S [Set] deriving Show
m0 = error "TODO"

sing :: Set -> Set
sing = error "TODO"

m1 = error "TODO"

m :: Int -> Set
m = error "TODO"

cardSem :: Set -> Int
cardSem = error "TODO"

cardSyn :: M -> Int
cardSyn = error "TODO"

data M -- TODO
\end{code}
