\begin{code}
{-# LANGUAGE GADTs #-}
module Live_2_1 where
import DSLsofMath.AbstractFOL
\end{code}

Propositional Calculus = PropCalc = PC = satslogik (svenska)

\begin{spec}
type Nam = String
data PC where
  And  :: PC -> PC -> PC
  Or   :: PC -> PC -> PC
  Impl :: PC -> PC -> PC
  Not  :: PC -> PC
  N    :: String -> PC    -- motsvarar Var :: String -> E från förra veckan
  C    :: Bool -> PC      -- motsvarar Con :: Integer -> E från förra veckan

type Sem' = Bool
type Sem = Tab -> Bool
type Tab = String -> Bool
eval :: PC -> Sem
eval (And  x y) = ands (eval x) (eval y)
eval (Or   x y) = ors  (eval x) (eval y)
eval (Impl x y) = impls(eval x) (eval y)
eval (Not  x)   = nots (eval x)
eval (N    n)   = names n
eval (C    c)   = cons c

pc1 :: PC
pc1 = And (C False) (C True)
pc2 :: PC
pc2 = Impl  (And (N "P") (N "Q"))
            (And (N "Q") (N "P"))
pc3 :: PC
pc3 = Impl (N "P") (C False)

ands  :: Sem -> Sem -> Sem
ors   :: Sem -> Sem -> Sem
impls :: Sem -> Sem -> Sem
nots  :: Sem -> Sem
names :: String -> Sem -- String -> Tab -> Sem' = String -> (String -> Bool) -> Bool
cons  :: Bool -> Sem


names n tab = tab n


lift2 :: (a->a->a) -> (t->a)->(t->a)->(t->a)
lift1 :: (a->a) -> (t->a)->(t->a)
lift0 :: a -> (t->a)
lift2 op  f g = \t -> op  (f t) (g t)
lift1 uop f   = \t -> uop (f t)
lift0 nop     = \t -> nop

ands  = lift2 ands'
ors   = lift2 ors'
impls = lift2 impls'
nots  = lift1 nots'
cons  = lift0

ands' :: Sem' -> Sem' -> Sem'
ands' = (&&)

ors'   :: Sem' -> Sem' -> Sem'
ors' = (||)

impls' :: Sem' -> Sem' -> Sem'
impls' False y = True
impls' True  y = y
-- samma som (<=) :: Bool -> Bool -> Bool

nots'  :: Sem' -> Sem'
nots' = not

cons' :: Bool -> Sem'
cons' = id

\end{spec}






















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














\begin{code}
type VarT = String
data QQ where
  QV     :: VarT      -> QQ
  FromI  :: Integer   -> QQ -- 3 = 3/1
  QPlus  :: QQ -> QQ  -> QQ
  QDiv   :: QQ -> QQ  -> QQ
 deriving Show

n, m :: QQ
n = QV "n"
m = QV "m"
hej = QPlus (QDiv m n) (FromI 1)

f1, f2 :: FOL
f1 = PName ">" [m, n]
f2 = PName "Prime" [hej]

type PSym = String
data FOL where
  PName :: PSym -> [QQ]      -> FOL
  Equal :: QQ   -> QQ        -> FOL
  -- |Equal r1 r2|  means   |PName "Equal" [r1,r2]|

  And      :: FOL   ->  FOL  -> FOL
  Or       :: FOL   ->  FOL  -> FOL
  Implies  :: FOL   ->  FOL  -> FOL
  Not      :: FOL            -> FOL

  FORALL   :: VarT  ->  FOL  -> FOL
  EXISTS   :: VarT  ->  FOL  -> FOL
 deriving Show

commPlus :: FOL
commPlus = FORALL "x" (FORALL "y"
             (Equal  (QPlus (QV "x") (QV "y"))
                     (QPlus (QV "y") (QV "x"))))
\end{code}

A more mathematical view of |commPlus| would be:

   ∀ x. ∀ y. (x + y) == (y + x)

where x, y ∈ QQ

Note the difference between the return type of |==| and |+|:
\begin{spec}
  (==) : QQ -> QQ -> FOL
  (+)  : QQ -> QQ -> QQ
\end{spec}
