Types for FOL syntax
\begin{code}
module FOL where
\end{code}

1. A general syntax for terms (data Term)
2. A general syntax for first-order logic predicates (data FOL, using Term)

-- We start we part 1. 
-- f is the type for "names" of functions (domain operations)
-- v is the type for "names" of variables
\begin{code}
data Term f v  =
     Func f [Term f v]
  |  V v    deriving (Eq, Ord, Show)
data NumOp = Zer | Suc | Add | Mul | Neg  deriving (Show, Eq)
type NumTerm = Term NumOp
r0 = Func Zer []           -- 0
r1 = Func Suc [r0]         -- 1
r2 = Func Add [r1, r1]     -- 2
r3 = Func Mul [V "x", r2]  -- x*2

type Z = Integer
evalOp :: NumOp -> ([Z] -> Z)
evalOp Zer = zerSem
evalOp Suc = sucSem
evalOp Add = addSem
evalOp Mul = mulSem
evalOp Neg = negSem

zerSem, sucSem, addSem, mulSem, negSem :: [Z] -> Z
zerSem [] = 0
sucSem [n] = 1+n 
addSem [x,y] = x+y
mulSem [x,y] = x*y
negSem [x] = negate x
type Env key val  =  key -> val   -- [(key,val)]
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
lookUp :: var -> Env var dom -> dom
lookUp x env = env x

eval :: Term func var
     -> Env var dom
     -> Env func ([dom] -> dom)   -- for example evalOp, when dom = Z = Integer
     -> dom
eval t env evF = eva t where
  eva (V v)           = env v  
  eva (Func f terms)  = evF f (map eva terms)
\end{code}
Some intermediate types:
       eva      ::   Term func var -> dom          
       f        ::        func                     
       terms    ::  [Term func var]                
       map eva  ::  [Term func var] -> [dom]       
       evF f    ::                     [dom] -> dom

Now on to step 2:

2. A general syntax for first-order logic predicates (data FOL, using Term)

\begin{code}
data FOL f v p = 
-- + predicates
                  Pred p [Term f v]
               |  Eq (Term f v) (Term f v)
-- Alternative to Pred if we specialise to just these two predicates:
--                  LessThan (Term f v) (Term f v)
--                  IsPrime (Term f v) 

-- + quantifiers
               |  Forall v (FOL f v p)
               |  Exists v (FOL f v p)

-- basically data Prop from lecture 2.1
               |  And  (FOL f v p)  (FOL f v p) 
               |  Or   (FOL f v p)  (FOL f v p)
               |  Impl (FOL f v p)  (FOL f v p)
               |  Not  (FOL f v p)

-- Example:
data NumPred = Divides | LessThan | IsPrime
type NumFOL v = FOL NumOp v NumPred

evenP :: NumTerm v -> NumFOL v
evenP t = Pred Divides [r2, t]

claim :: NumFOL String
claim = Forall "n" (Pred IsPrime [V "n"])
\end{code}
(The claim is a well form proposition, but not true.)

--------------------------------------------------------------------------------
2.2 Semantics

\begin{code}
check :: Eq dom
      => FOL func var pred
      -> Env var dom                      -- domain variables
      -> Env func ([dom]  ->  dom)        -- domain operators
      -> Env pred ([dom]  ->  Bool)       -- predicates "over" the domain
      -> Bool
check p env evF evP = chec p
  where
    eva t = eval t env evF
    chec (Pred pred terms)   =  evP pred (map eva terms)
    chec (Eq t0 t1)          =  eva t0  ==  eva t1

    chec (Forall _ _)        =  error "TODO: cannot check Forall" 
    chec (Exists _ _)        =  error "TODO: cannot check Exists"

    chec (And  wff0 wff1)    =  chec wff0  &&   chec wff1
    chec (Or   wff0 wff1)    =  chec wff0  ||   chec wff1
    chec (Impl wff0 wff1)    =  chec wff0  ==>  chec wff1
    chec (Not wff)           =  not (chec wff)

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> b = b
\end{code}
