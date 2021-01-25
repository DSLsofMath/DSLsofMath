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
\end{code}

TODO eval :: Syn -> Sem







































\begin{spec}
type Nam = String
data PC where
  And   :: PC -> PC -> PC
  Or    :: PC -> PC -> PC
  Impl  :: PC -> PC -> PC
  Not   :: PC       -> PC
  Name  :: Nam      -> PC
  Con   :: Bool     -> PC

e3 :: PC
e3 = And (Name "a") (Not (Name "a"))
e3' = Con False

eval :: PC -> Sem
eval (And x y)  = ands  (eval x) (eval y)
eval (Or  x y)  = ors   (eval x) (eval y)
eval (Impl x y) = impls (eval x) (eval y)
eval (Not x )   = nots  (eval x)
eval (Name n)   = names n -- lookup n in the table
eval (Con  c)   = cons c


-- type Table = [(Nam, Bool)] -- translate names to Booleans
type Table = Nam -> Bool
type Sem = Table -> Bool  -- (Nam -> Bool) -> Bool
impls :: Sem -> Sem -> Sem
impls f g = \l -> impl (f l) (g l)
    -- f :: Sem = Table -> Bool
    -- l :: Table = Nam -> Bool, _ :: Bool

[ors, ands, nots] = error "TODO: ors, ands, nots"

        -- PC -> PC -> PC
impl :: Bool -> Bool -> Bool
impl False  x = True
impl True   x = x


names :: Nam -> Sem   -- Sem = Table -> Bool
names n = \l -> l n    -- l :: Table = Nam -> Bool
-- names n l = l n    -- l :: Table = Nam -> Bool

cons :: Bool -> Sem
cons b = \_ -> b

--  And   :: PC -> PC -> PC
--  Or    :: PC -> PC -> PC

--  Not   :: PC       -> PC
--  Name  :: Nam      -> PC
--  Con   :: Bool     -> PC


\end{spec}
