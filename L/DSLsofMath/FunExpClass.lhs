data FunExp  =  Const REAL
             |  Id
             |  FunExp :+: FunExp
             |  FunExp :*: FunExp
             |  FunExp :/: FunExp
             |  Exp FunExp
             |  Sin FunExp
             |  Cos FunExp
                -- and so on
  deriving (Eq, Show)

\begin{code}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module DSLsofMath.FunExpClass where
import DSLsofMath.FunExp
class FunExpClass t where
  constF :: REAL -> t
  idF  :: t
  addF :: t -> t -> t
  mulF :: t -> t -> t
  divF :: t -> t -> t
  expF :: t -> t
  sinF :: t -> t
  cosF :: t -> t

instance FunExpClass FunExp where
  constF  = Const
  idF     = Id
  addF    = (:+:)
  mulF    = (:*:)
  divF    = (:/:)
  expF    = Exp
  sinF    = Sin
  cosF    = Cos

instance Floating a => FunExpClass (a->a) where
  constF  = constFloating
  idF     = id
  addF    = (+)
  mulF    = (*)
  divF    = (/)
  expF    = exp
  sinF    = sin
  cosF    = cos

constFloating :: Floating a => REAL -> (a->a)
constFloating c = fromRational (toRational c)

evalFE :: FunExpClass a => FunExp -> a
evalFE (Const c)  =  constF c
evalFE Id         =  idF
evalFE (f :+: g)  =  addF (evalFE f) (evalFE g)
evalFE (f :*: g)  =  mulF (evalFE f) (evalFE g)
evalFE (f :/: g)  =  divF (evalFE f) (evalFE g)
evalFE (Exp f)    =  expF (evalFE f)
evalFE (Sin f)    =  sinF (evalFE f)
evalFE (Cos f)    =  cosF (evalFE f)

testFE1 = Id :*: Id

testeval1 :: FunExpClass a => a
testeval1 = evalFE testFE1

testevalFE1 :: FunExp
testevalFE1 = testeval1

testevalFunc1 :: REAL -> REAL
testevalFunc1 = testeval1
\end{code}
