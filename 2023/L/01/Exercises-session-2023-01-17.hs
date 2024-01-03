-- Felix
-- ex 1.1
data Exp = Con Integer
         | Var String
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp



-- 1.
-- a1 = 2 + 2
a1 :: Exp
a1 = Plus (Con 2) (Con 2)

-- a2 = a1 + (7 * 9)
a2 = Plus a1 (Times (Con 7) (Con 9))

-- 2. create eval :: Exp -> Integer

eval :: Exp -> Integer
eval (Con k)      = k
eval (Var name)   = varVal name
eval (Plus e1 e2)= (eval e1) + (eval e2)
eval (Minus e1 e2)= (eval e1) - (eval e2)
eval (Times e1 e2)= (eval e1) * (eval e2)


-- 3. c2 = (x-5) * (y+2)
c2 = Times (Minus (Var "x") (Con 5)) (Plus (Var "y") (Con 2))

-- c1 =  (x-5) * (y+2) * z
--c1 = Times (Times part1 part2) part3
c1 = Times part1 (Times part2 part3)
  where part1 = Minus (Var "x") (Con 5)
        part2 = Plus (Var "y") (Con 2)
        part3 = Var "z"

varVal :: String -> Integer
varVal "x" = 3
varVal "y" = 5
varVal "z" = -1








{-
ex 1-5

isoR :: (Bool -> t) -> (t,t)
isoR f = (f True, f False)

isoL :: (t,t) -> (Bool -> t)
isoL (a,b) = f
             where
               f True = a
               f False = b

f :: Bool -> Integer
f True = 5
f False = 7

isoR (f) == (5,7)

isoL (5,7) == some function which maps True to 5 and False to 7 == f
-}
