\begin{code}
module Live_2_1_2023 where
\end{code}

Propositional Calculus = Prop
  (from Chapter 2 in the book)
\begin{code}
newtype Set = S [Set] deriving Show
m0 = S []

sing :: Set -> Set
sing x = S [x]

m1 = S [S []] -- sing m0 == S [m0]

m :: Int -> Set
m 0 = m0
m i = sing (m (i-1))
cardSem :: Set -> Int
cardSem = error "TODO"

cardSyn :: M -> Int
cardSyn = error "TODO"

data M -- TODO



-- Satslogik syntax
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
-- type Sem = Tab -> Bool  -- second attempt
type Sem = (Namn->Bool) -> Bool  -- second attempt
type Tab = Namn -> Bool
eval :: Syn -> Sem
eval (Con b)          = conS b
eval (Not      e)     = notS  (eval e)
eval (And      e1 e2) = andS  (eval e1) (eval e2)
eval (Or       e1 e2) = orS   (eval e1) (eval e2)
eval (Implies  e1 e2) = impS  (eval e1) (eval e2)
eval (Name n)         = nameS n

-- eval "replaces contructor C by function cS" for all C i the type Prop

conS :: Bool -> Sem  -- Bool -> (Tab -> Bool)
-- conS b _tab = b
-- conS b = \_tab -> b
-- conS b = const b
conS = const

notS :: Sem -> Sem -- (Tab -> Bool) -> (Tab -> Bool)
notS evale = not . evale
andS :: Sem -> Sem -> Sem
-- andS f g = lift2 (&&) f g
andS = lift2 (&&)
orS  :: Sem -> Sem -> Sem
impS :: Sem -> Sem -> Sem

(orS, impS) = error "TODO: implement semantic functions"

(==>) :: Bool -> Bool -> Bool
(==>) = error "TODO: (==>)"

nameS :: Namn -> Sem
nameS = error "TODO: nameS"







-- generalise!

lift0 ::  a            -> (t -> a)
lift1 :: (a -> b)      -> (t -> a) -> (t -> b)

lift2 :: (a -> b -> c) -> (t -> a) -> (t -> b) -> (t -> c)
lift2 op f g = \x -> op (f x) (g x)   

lift0 = error "TODO: lift0"
lift1 = error "TODO: lift1"
 -- op :: a -> b -> c
 -- f :: t -> a
 -- g :: t -> b
 -- x :: t


\end{code}
testTab "a" = True
testTab "b" = False
testTab _   = False

test1 = eval p1 testTab
test2 = eval p2 testTab
test3 = eval p3 testTab
test4 = eval p4 testTab
