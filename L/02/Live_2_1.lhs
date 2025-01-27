\begin{code}
module Live_2_1 where
\end{code}

Propositional Calculus = Prop
  (from Chapter 2 in the book)
\begin{code}
data Prop  =  And  Prop  Prop
           |  Or   Prop  Prop
           |  Imp  Prop  Prop
           |  Not  Prop
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

eval :: Syn -> Sem1
eval (Con b)       = conS b
eval (Not  e)      = notS  (eval e)
eval (And  e1 e2)  = andS  (eval e1) (eval e2)
eval (Or   e1 e2)  = orS   (eval e1) (eval e2)
eval (Imp  e1 e2)  = impS  (eval e1) (eval e2)
eval (Nam n)       = nameS n

-- eval "replaces contructor C by function cS" for all C i the type Prop

conS :: Bool -> Sem1
notS :: Sem1 -> Sem1
andS :: Sem1 -> Sem1 -> Sem1
orS  :: Sem1 -> Sem1 -> Sem1
impS :: Sem1 -> Sem1 -> Sem1

(conS, notS, andS, orS, impS) = error "TODO: implement semantic functions"

(==>) :: Bool -> Bool -> Bool
(==>) = error "TODO: (==>)"

nameS :: NameT -> Sem1
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


