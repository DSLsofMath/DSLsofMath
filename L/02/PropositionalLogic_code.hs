{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyCase #-}
module DSLsofMath.PropositionalLogic where

data Prop  =  Implies  Prop  Prop  |  And      Prop  Prop  |  Or       Prop  Prop
           |  Not      Prop        |  Name     Name        |  Con      Bool
  deriving (Eq, Show)
type Name = String

p1, p2, p3, p4 :: Prop
p1 = And  (Name "a")  (Not (Name "a"))
p2 = Or   (Name "a")  (Not (Name "a"))
p3 = Implies  (Name "a")  (Name "b")
p4 = Implies  (And a b)   (And b a)
  where a = Name "a"; b = Name "b"

type Env = Name -> Bool
eval :: Prop -> Env -> Bool
eval (Implies p q)  env = eval p env  ==>  eval q env
eval (And p q)      env = eval p env  &&   eval q env
eval (Or  p q)      env = eval p env  ||   eval q env
eval (Not p)        env = not (eval p env)
eval (Name n)       env = env n
eval (Con t)        env = t

(==>) :: Bool -> Bool -> Bool
False  ==> _ {-"\quad"-}  = True
True   ==> p              = p

isTautology :: Prop -> Bool
isTautology p = and (map (eval p) (envs (freeNames p)))

envs :: [Name] -> [Env]
envs []      =  [error "envs: never used"]
envs (n:ns)  =  [  \n' -> if n == n' then b else e n'
                |  b  <-  [False, True]
                ,  e  <-  envs ns
                ]

freeNames :: Prop -> [Name]
freeNames = error "exercise"

checkProof :: Proof -> Prop -> Bool

checkProof TruthIntro        (Con True)   =   True
checkProof (AndIntro t u)    (And p q)    =   checkProof t p
                                          &&  checkProof u q
checkProof (OrIntroL t)      (Or p q)     =   checkProof t p
checkProof (OrIntroR u)      (Or p q)     =   checkProof u q
checkProof (NotIntro q t u)  (Not p)      =   checkProof t (p `Implies` q)
                                          &&  checkProof u (p `Implies` Not q)

checkProof (AndElimL q t)       p  =   checkProof  t  (p `And` q)
checkProof (AndElimR p t)       q  =   checkProof  t  (p `And` q)
checkProof (OrElim p q t u v)   r  =   checkProof  t  (p `Implies` r)
                                   &&  checkProof  u  (q `Implies` r)
                                   &&  checkProof  v  (Or p q)
checkProof (NotElim t)          p  =   checkProof  t  (Not (Not p))
checkProof (FalseElim t)        p  =   checkProof  t  (Con False)

checkProof (Assume p')          p                =   p == p'
checkProof (ImplyIntro f)       (p `Implies` q)  =   checkProof (f (Assume p)) q
checkProof (ImplyElim p t u)    q                =   checkProof t (p `Implies` q)
                                                 &&  checkProof u p
checkProof _                    _                =   False -- incorrect proof

data Proof  =  TruthIntro                   |  FalseElim Proof
            |  AndIntro  Proof  Proof
            |  AndElimL  Prop   Proof       |  AndElimR  Prop  Proof
            |  OrIntroL  Proof              |  OrIntroR  Proof
            |  OrElim Prop Prop Proof Proof Proof
            |  NotIntro Prop Proof Proof    |  NotElim Proof
            |  Assume Prop
            |  ImplyIntro (Proof -> Proof)  |  ImplyElim  Prop  Proof Proof

conjunctionComm :: Prop
conjunctionComm = p4

conjunctionCommProof :: Proof
conjunctionCommProof = ImplyIntro step
  where  step :: Proof -> Proof
         step evAB =  AndIntro  (AndElimR  (Name "a")  evAB  )
                                (AndElimL  (Name "b")  evAB  )

-- >>> checkProof conjunctionCommProof conjunctionComm
-- True

conjunctionCommProof2 :: Proof
conjunctionCommProof2 =
  ImplyIntro (\evAB ->
  AndIntro  (AndElimL (Name "b") evAB)
            (AndElimR (Name "a") evAB) )

-- >>> checkProof conjunctionCommProof2 conjunctionComm
-- False

conjunctionCommProof' :: Implies (And a b) (And b a)
conjunctionCommProof' = implyIntro step
  where  step :: And a b -> And b a
         step evAB =  andIntro  (andElimR  evAB)
                                (andElimL  evAB)

truthIntro  :: Truth
falseElim   :: False -> p
andIntro    :: p -> q -> And p q
andElimL    :: And p q -> p
andElimR    :: And p q -> q
orIntroL    :: p -> Or p q
orIntroR    :: q -> Or p q
orElim      :: Or p q -> (p `Implies` r) -> (q `Implies` r) -> r
notIntro    :: (p `Implies` q) `And`  (p `Implies` Not q) -> Not p
notElim     :: Not (Not p) -> p
implyIntro  :: (p -> q) -> (p `Implies` q)
implyElim   :: (p `Implies` q) -> (p -> q)

type Not p = p `Implies` False

notElim = error "not possible as such in intuitionistic logic"

type Implies p q = p -> q
implyElim   f = f
implyIntro  f = f

type And p q = (p,q)
andIntro t u = (t,u)
andElimL  = fst
andElimR  = snd

notIntro (evPimpQ, evPimpNotQ) evP =
    {-""-} -- a proof of |False| wanted

    let  evQ     = evPimpQ      evP
         evNotQ  = evPimpNotQ   evP

    in evNotQ  evQ

notIntro' :: (p -> q, p -> q -> False) -> p -> False
notIntro' (f, g) x = (g x) (f x)

type Or a b = Either a b
orIntroL  = Left
orIntroR  = Right
orElim pOrq f g = case pOrq of
  Left   p -> f  p
  Right  q -> g  q

type Truth = ()
truthIntro = ()

data False
falseElim x = case x of {}

excludedMiddle :: Not (Not (p `Or` Not p)) -- to prove this, we can ...
excludedMiddle k = -- ... assume |Not (Or p (Not p))| and prove falsity.
   k -- So, we can prove falsity if we can prove |Or p (Not p)|.
   (Right  -- We can prove in particular the right case, |Not p|
     (\evP ->  -- ... by assuming that |p| holds, and prove falsity.
        k --  Again, we can prove falsity if we can prove |Or p (Not p)|.
          (Left -- This time, we can prove in particular the left case, |p|
             evP))) -- because we assumed it earlier!

excludedMiddle' :: Not (Not (p `Or` Not p))
excludedMiddle' k = k (Right (\evP -> k (Left evP)))

test1' :: (a -> (b, c)) -> (a->b, a->c)
test1' a2bc =  ( \a -> fst  (a2bc a)
               , \a -> snd  (a2bc a) )

test2' :: (a->b, a->c) -> (a -> (b, c))
test2' fg = \a -> (fst fg a, snd fg a)

test1  ::  Implies (Implies a (And b c)) (And (Implies a b) (Implies a c))
test1  = implyIntro (\a2bc ->
             andIntro  (implyIntro (\a -> andElimL  (implyElim a2bc a)))
                       (implyIntro (\a -> andElimR  (implyElim a2bc a))))

test2  ::  Implies (And (Implies a b) (Implies a c)) (Implies a (And b c))
test2  =   implyIntro (\fg ->
             implyIntro (\a ->
               andIntro  (implyElim (andElimL  fg) a)
                         (implyElim (andElimR  fg) a)))
