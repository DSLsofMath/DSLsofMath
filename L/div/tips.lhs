foo :: (a->b, a->c, a->d) -> (a->(b,c,d))
oof :: (a->(b,c,d)) -> (a->b, a->c, a->d)

foo :: TriFun a -> FunTri a   -- om a=b=c=d
oof :: FunTri a -> TriFun a   -- om a=b=c=d

foo . oof = id
oof . foo = id

----------------

type Tri a     = (a, a, a)
type TriFun a  = Tri (a->a)   -- = |(a->a, a->a, a->a)|
type FunTri a  = a -> Tri a   -- = |a -> (a, a, a)|

----------------

evalDD  :: FE -> FunTri a
evalDD2 :: FE -> TriFun a
evalDD e = \x -> (eval e x, eval' e x, eval'' e x)
evalDD2 e = (eval e, eval' e, eval'' e)
  where eval'  = eval . derive
        eval'' = eval' . derive

evalDD = foo . evalDD2

man vill visa detta:

  H2(evalDD, (:*:), mulDD)
  mulDD :: FunTri a -> FunTri a -> FunTri a

det som J har visat (början av) är

  H2(evalDD2, (:*:), mulDD2)
  mulDD2 :: TriFun a -> TriFun a -> TriFun a

Allmän egenskap hos H2 är

Från
  H2(f,OpA, OpB)  -- f :: A -> B
  H2(g,OpB, OpC)  -- g :: B -> C
följer
  H2(g.f,OpA,OpC)




Senaste exempel på tavlan var
  f = evalAll :: FunExp -> [(R->R)]
  g = map ap0 :: [(R->R)] -> [R]

----------------

  H2(evalDD, (:*:), mulDD)
=
 forall x,y. evalDD(x:*:y) = mulDD (evalDD x) (evalDD y)
=
 forall x,y. foo (evalDD2(x:*:y)) = mulDD (foo (evalDD2 x))
                                          (foo (evalDD2 y))
= -- Def. av evalDD2 eller H2(evalDD2, ...)
{-
  evalDD2(x:*:y) = mulDD2 (evalDD2 x) (evalDD2 y)
-}
=  forall x,y. foo (mulDD2 (evalDD2 x) (evalDD2 y)) =


mulDD2 (f,f',f'') (g,g',g'') = ...

foo (f,g,h) = \x -> (f x, g x, h x)
