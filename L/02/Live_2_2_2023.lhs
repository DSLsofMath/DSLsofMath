Types for FOL syntax
\begin{code}
module FOL where
\end{code}

1. A general syntax for terms (data Term)
2. A general syntax for first-order logic predicates (data FOL, using Term)

-- We start we part 1. 
\begin{code}
data Term f v  =  Func f [Term f v]
               |  V v
  deriving (Eq, Ord, Show)
\end{code}

1.1: Example of numeric terms
\begin{code}
data NumOp = Zer | Suc | Add | Mul | Neg  deriving (Show, Eq)
type NumTerm = Term NumOp

r0 = Func Zer []
r1 = Func Suc [r0]
r2 = Func Add [r1, r1]
r3 = Func Mul [V "x", r2]

type Z = Integer
evalOp :: NumOp -> [Z] -> Z
evalOp Zer = zerSem
evalOp Suc = sucSem
evalOp Add = addSem
evalOp Mul = mulSem
evalOp Neg = negSem

zerSem, sucSem, addSem, mulSem, negSem :: [Z] -> Z
(zerSem, sucSem, addSem, mulSem, negSem) = error "TODO: semantics"

testEval :: Env var Z -> NumTerm var -> Z
testEval env t = eval t env evalOp

myEnv :: String -> Z
myEnv "x" = 17
myEnv _   = 0

test :: NumTerm String -> Z
test = testEval myEnv
\end{code}

1.2 Semantics

\begin{code}
type Env key val  =  key -> val
lookUp :: var -> Env var dom -> dom
lookUp x env = env x

eval :: Term func var
     -> Env var dom
     -> Env func ([dom] -> dom)
     -> dom
eval t env evF = eva t where
  eva (V var)         =  lookUp var env
  eva (Func f terms)  =  evF f (map eva terms)
\end{code}


Now on to step 2:

2. A general syntax for first-order logic predicates (data FOL, using Term)

\begin{code}
data FOL f v p =  Pred p [Term f v]
               |  Eq (Term f v) (Term f v)

               |  Forall v (FOL f v p)
               |  Exists v (FOL f v p)
               |  And  (FOL f v p)  (FOL f v p)
               |  Or   (FOL f v p)  (FOL f v p)
               |  Impl (FOL f v p)  (FOL f v p)
               |  Not  (FOL f v p)

data NumPred = Divides | LessThan
type NumFOL v = FOL NumOp v NumPred

evenP :: NumTerm v -> NumFOL v
evenP t = Pred Divides [r2, t]

claim :: NumFOL String
claim = Forall "n" (evenP (V "n"))
\end{code}

--------------------------------------------------------------------------------
2.2 Semantics

\begin{code}
check :: Eq dom
      => FOL func var pred
      -> Env var dom
      -> Env func ([dom]  ->  dom)
      -> Env pred ([dom]  ->  Bool)
      -> Bool
check p env evF evP = chec p
  where
    eva t = eval t env evF
    chec (Pred pred terms)   =  evP pred (map eva terms)
    chec (Eq t0 t1)          =  eva t0  ==  eva t1

    chec (And  wff0 wff1)    =  chec wff0  &&   chec wff1
    chec (Or   wff0 wff1)    =  chec wff0  ||   chec wff1
    chec (Impl wff0 wff1)    =  chec wff0  ==>  chec wff1
    chec (Not wff)           =  not (chec wff)
    chec (Forall _ _)        =  error "TODO: cannot check Forall"
    chec (Exists _ _)        =  error "TODO: cannot check Exists"


(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> b = b
\end{code}
