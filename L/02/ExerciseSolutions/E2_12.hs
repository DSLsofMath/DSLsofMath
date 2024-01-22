-- implement de-Morgan dualisation
-- eval (doNot e) == eval (Not e)
-- import PropositionalLogic_code

data Prop  =  Implies  Prop  Prop  |  And      Prop  Prop  |  Or       Prop  Prop
           |  Not      Prop        |  Name     Name        |  Con      Bool
  deriving (Eq, Show)
type Name = String

-- This is a bonus exercise and you are better off trying it yourself.
-- Spoiler alert! Scroll down for solution.




































doNot :: Prop -> Prop
doNot (And p q) = Or  (doNot p) (doNot q)
doNot (Or  p q) = And (doNot p) (doNot q)
doNot (Con b)   = Con (not b)
doNot (Name n)  = Not (Name n)
doNot (Not p)   = case doNot p of
  Not e -> e
  Con b -> Con (not b)
  e     -> doNot e
doNot (Implies p q) = doNot (Or (Not p) q)

-- Some test examples:
x = Name "x"
y = Name "y"
e1 = And x y
e2 = Or e1 (Or x y)
e3 = And (Not x) y
e4 = Not (Not (And (Not x) (Not y)))
e5 = Implies x x
