A datatype for propositional proofs in Haskell

Closely following Chapter 2 in the book
  Domain-Specific Languages of Mathematics

\begin{code}
module DSLsofMath.PropositionalLogic where

data Prop = Implies Prop Prop
          | And     Prop Prop
          | Or      Prop Prop
          | Not     Prop
          | Name Name
          | Con Bool
  deriving (Eq, Show)
type Name = String
type Env = Name -> Bool
eval :: Prop -> Env -> Bool
eval (Implies p q)  env  =  eval p env  ==>  eval q env
eval (And     p q)  env  =  eval p env  &&   eval q env
eval (Or      p q)  env  =  eval p env  ||   eval q env
eval (Not     p)    env  =  not (eval p env)
eval (Name n)       env  =  env n
eval (Con t)       _env  =  t

(==>) :: Bool -> Bool -> Bool
False  ==> _   = True
True   ==> p   = p
\end{code}

Here is one way to represent proof terms for Prop.
\begin{code}
data Proof  = ImplyIntro (Proof -> Proof)
            | Assume Prop -- only used internally in checkProof
            | AndIntro Proof Proof   -- pairing
            | AndElimL Prop Proof    -- fst
            | AndElimR Prop Proof    -- snd
              -- the rest is not covered in this lecture
            | TruthIntro
            | FalseElim Proof
            | OrIntroL Proof
            | OrIntroR Proof
            | OrElim Prop Prop Proof Proof Proof
            | NotIntro Prop Proof Proof
            | NotElim Proof
            | ImplyElim Prop Proof Proof
\end{code}

\begin{code}
checkProof :: Proof -> Prop -> Bool
checkProof (AndIntro t u) (And p q)      =  checkProof t p  &&  checkProof u q
checkProof (AndElimL q t) p              =  checkProof t (And p q)
checkProof (AndElimR p t) q              =  checkProof t (And p q)
checkProof (Assume p')    p              =  p == p'
checkProof (ImplyIntro f) (Implies p q)  =  checkProof (f (Assume p)) q
\end{code}

The rest of checkProof is not covered in this lecture, but included for
completeness.
\begin{code}
checkProof TruthIntro        (Con True)   =   True
checkProof (OrIntroL t)      (Or p q)     =   checkProof t p
checkProof (OrIntroR u)      (Or p q)     =   checkProof u q
checkProof (NotIntro q t u)  (Not p)      =   checkProof t (p `Implies` q)
                                          &&  checkProof u (p `Implies` Not q)

checkProof (OrElim p q t u v)   r  =   checkProof  t  (p `Implies` r)
                                   &&  checkProof  u  (q `Implies` r)
                                   &&  checkProof  v  (Or p q)
checkProof (NotElim t)          p  =   checkProof  t  (Not (Not p))
checkProof (FalseElim t)        p  =   checkProof  t  (Con False)
checkProof (ImplyElim p t u)    q                =   checkProof t (p `Implies` q)
                                                 &&  checkProof u p
checkProof _                    _                =   False -- incorrect proof
\end{code}

Examples:
\begin{code}
a, b :: Prop
a = Name "a"
b = Name "b"

th1 :: Prop
th1 = Implies a a
pr1 :: Proof
pr1 = ImplyIntro (\x -> x)
check1 :: Bool
check1 = checkProof pr1 th1

th2 :: Prop
th2 = Implies a (And a a)
pr2 :: Proof
pr2 = ImplyIntro (\x -> AndIntro x x)
check2 :: Bool
check2 = checkProof pr2 th2

-- And is commutative
th3 :: Prop
th3 = Implies (And a b) (And b a)
pr3 :: Proof
pr3 = ImplyIntro (\xAy -> AndIntro (AndElimR a xAy) (AndElimL b xAy))
check3 :: Bool
check3 = checkProof pr3 th3
\end{code}

Exercise:
  Try to swap |AndElimL| and |AndElimR| in the above proof.
  What will happen and why?

\begin{code}
pr3bad :: Proof
pr3bad = ImplyIntro (\xAy -> AndIntro (AndElimL b xAy) (AndElimR a xAy))

check3bad :: Bool
check3bad = checkProof pr3bad th3
\end{code}

\begin{code}
instance Show Proof where show = showProof

showProof :: Proof -> String
showProof (ImplyIntro f) = "\\_x->" ++ show (f (Assume (Name "_x")))
showProof (Assume (Name n)) = n
showProof (Assume p) = "Assume "++show p
showProof (AndElimL q t) = "AndElimL " ++ show q ++ " " ++ show t
showProof (AndElimR p t) = "AndElimR " ++ show p ++ " " ++ show t
showProof (AndIntro t u) = "AndIntro " ++ show t ++ " " ++ show u
\end{code}
