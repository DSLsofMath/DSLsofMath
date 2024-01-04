\begin{code}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
\end{code}

* Live coding part of week 2, lecture 2.

Reminder of learning outcomes

* organize areas of mathematics in DSL terms
    * "develop adequate notation for mathematical concepts"
    * "perform calculational proofs"

Modelling logic with types
Using the type-checker as proof-checker
  ("poor mans proof checker")


   bevis  :: Satser i FOL

  program :: Typ

  vi kodar in logiken som typer
  och bevisen som "program"
  en sats är "sann" om typen har kardinalitet >0


data Falsity  -- No constructors - an empty set of proof terms

\begin{code}
data Truth    = Obvious
test0 :: Truth
test0 = Obvious
\end{code}

----------------
type And p q = (p,q)
\begin{code}

test00 :: And Truth Truth  -- (Truth, Truth)
test00 = andIntro test0 Obvious

swap :: And p q -> And q p   -- And is commutative
swap (bp, bq) = (bq, bp)
-- swap pq = (snd pq,  fst pq)
-- swap pq = andIntro (snd pq) (fst pq)
-- swap pq = andIntro (andElimR pq) (andElimL pq)

split  ::  Implies  (Implies a (And b c))
                    (And (Implies a b) (Implies a c))
split = implyIntro foo

foo ::  (Implies a (And b c)) -> 
        (And (Implies a b) (Implies a c))
foo bar = (part1, part2)
  where part1 = implyIntro a2b -- :: Implies a b
        a2b ba = andElimL (implyElim bar ba)  -- :: b

-- implyElim bar :: a -> And b c
-- implyElim bar ba ::   And b c

        part2 = implyIntro a2c -- :: Implies a c
        a2c ba = andElimR (implyElim bar ba)   -- :: c

split'  ::  (a -> (b, c)) ->
            (a->b, a->c)
split' bar = (fst . bar , snd . bar)


join   ::  Implies  (And (Implies a b) (Implies a c))
                    (Implies a (And b c))
join = error "TODO"
\end{code}

----------------------------------------------------------------
-- type Or
\begin{spec}
type Or a b = Either a b
data Either a b where  -- disjoint union
  Left  :: a -> Either a b   -- orIntroL
  Right :: b -> Either a b   -- orIntroR
\end{spec}


\begin{code}
orElim :: Or p q -> (p -> r) -> (q -> r) -> r
orElim (Left  bp) f g = f bp
orElim (Right bq) f g = g bq

swap2 :: Or p q -> Or q p   -- Or is commutative
swap2 (Left  bp) = Right bp
swap2 (Right bq) = Left bq
\end{code}

----------------
-- Not

type Not p = p -> Falsity

\begin{code}
exc :: ((Either p (p -> Falsity)) -> Falsity) -> Falsity
-- exc :: Not (Not (Or p (Not p)))   -- not (not (law of excluded middle))
exc = error "TODO"
\end{code}


-- *** Reference code from PropositionalLogic_code.hs ***
\begin{code}
-- truthIntro  :: Truth
falseElim   :: Falsity -> p
andIntro    :: p -> q -> And p q
andElimL    :: And p q -> p
andElimR    :: And p q -> q
orIntroL    :: p -> Or p q
orIntroR    :: q -> Or p q
-- orElim      :: Or p q -> (p `Implies` r) -> (q `Implies` r) -> r
notIntro    :: (p `Implies` q) `And`  (p `Implies` Not q) -> Not p
notElim     :: Not (Not p) -> p
implyIntro  :: (p -> q) -> (p `Implies` q)
implyElim   :: (p `Implies` q) -> (p -> q)

type Not p = p `Implies` Falsity

notElim = error "not possible as such in intuitionistic logic"

type Implies p q = p -> q
implyElim   f = f
implyIntro  f = f

type And p q = (p,q)
andIntro t u = (t,u)
andElimL  = fst
andElimR  = snd

notIntro (evPimpQ, evPimpNotQ) evP =
    let  evQ     = evPimpQ      evP
         evNotQ  = evPimpNotQ   evP
    in evNotQ  evQ

notIntro' :: (p -> q, p -> q -> Falsity) -> p -> Falsity
notIntro' (f, g) x = (g x) (f x)

type Or a b = Either a b
orIntroL  = Left
orIntroR  = Right

-- truthIntro = ()

data Falsity
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

\end{code}

test1  ::  Implies (Implies a (And b c)) (And (Implies a b) (Implies a c))
test1  = implyIntro (\a2bc ->
             andIntro  (implyIntro (\a -> andElimL  (implyElim a2bc a)))
                       (implyIntro (\a -> andElimR  (implyElim a2bc a))))

test2  ::  Implies (And (Implies a b) (Implies a c)) (Implies a (And b c))
test2  =   implyIntro (\fg ->
             implyIntro (\a ->
               andIntro  (implyElim (andElimL  fg) a)
                         (implyElim (andElimR  fg) a)))


