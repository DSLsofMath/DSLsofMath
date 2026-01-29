Types for FOL syntax
\begin{code}
module FOL where
\end{code}

1. A general syntax for terms (data Term) in some domain
2. A general syntax for first-order logic predicates (data FOL, using Term)

\begin{code}
type VName = String
type FSym = String
data TermA = VarA VName | FuncA FSym [TermA] deriving Show
tA0, tA1, tA2 :: TermA
tA0 = FuncA "Zero" []
tA1 = FuncA "Succ" [tA0]
tA2 = FuncA "Add" [tA1, tA1]
tA4 = add tA2 tA2
add x y = FuncA "Add" [x,y]

type Z = Integer
type EnvA = VName -> Z

evalA :: TermA -> EnvA -> Z
evalA = error "TODO" -- see evalB below
myEnv :: EnvA
myEnv "x" = 17
myEnv "y" = 38
myEnv _   = 0
testA0 = evalA tA0 myEnv
testA1 = evalA tA1 myEnv
testA2 = evalA tA2 myEnv
checkA = [testA0, testA1, testA2] == [0,1+0,(1+0)+(1+0)]
\end{code}

Generalise: from VName to any type v for variable names.

\begin{code}
type TermA' = TermB String
data TermB v = VarB v | FuncB FSym [TermB v] deriving Show

type EnvB v = v -> Z
evalB :: TermB v -> EnvB v -> Z
evalB (VarB x)        env = env x
evalB (FuncB sym ts)  env = symSem sym (map foo ts)
   where foo t = evalB t env
         -- foo :: TermB v -> Z
-- ts :: [TermB v]
symSem :: FSym -> [Z] -> Z
symSem "Zero"  []      = 0
symSem "Succ"  [a]     = 1 + a
symSem "Succ"  []      = 1
symSem "Add"   [a, b]  = a + b

myEnvB "x" = 17
myEnvB "y" = 38
myEnvB _   = 0

tB0, tB1, tB2 :: TermB v
tB0 = FuncB "Zero" []
tB1 = FuncB "Succ" [tB0]
tB2 = FuncB "Add" [tB1, tB1]
tB4 = addB tB2 tB2
addB x y = FuncB "Add" [x,y]
\end{code}
tB0, tB1, tB2 :: TermB v
testB0 = evalB tB0 myEnv
testB1 = evalB tB1 myEnv
testB2 = evalB tB2 myEnv
checkB = [testB0, testB1, testB2] == [0,1+0,(1+0)+(1+0)]


--- Not done in the 2026-01-29 lecture
Generalise again: from FName to any type f

tC0, tC1, tC2 :: TermC MyOps v
testC0 = evalC tC0 myEnv
testC1 = evalC tC1 myEnv
testC2 = evalC tC2 myEnv
checkC = [testC0, testC1, testC2] == [0,1+0,(1+0)+(1+0)]
\begin{code}
\end{code}

Finally time for the logic part:

data FOL ...

Predicate logic
\begin{code}
data FOL p v = And (FOL p v) (FOL p v) | Or (FOL p v) (FOL p v) | Not (FOL p v)
           -- ... Propositional logic fragment
           | Forall v (FOL p v) | Exists v (FOL p v)
           | PName p [TermB v]
    deriving Show
data MyPreds = ParentOf | Woman | Alive
    deriving Show
\end{code}

Some example terms (parent, child) and formulas (f1, f1', ...).

\begin{code}
parent = FuncB "Gunilla" []
child  = FuncB "Patrik"  []
f1 :: FOL String v
f1 = PName "parentOf" [parent, child]

f1' :: FOL MyPreds v
f1' = PName ParentOf [parent, child]

f1Bad = PName ParentOf [parent]  -- wrong arity = wrong number of arguments

f2 :: FOL MyPreds Z
-- forall x1. exists x2. ParentOf(x1, x2)
f2 = Forall 1 (Exists 2 (PName ParentOf [VarB 1, VarB 2]))
f2' = PName ParentOf [VarB 1, VarB 2]
\end{code}
