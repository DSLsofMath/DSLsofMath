module Ex4 where
type And p q = (p,q)

andIntro:: p -> q -> And p q
andIntro p q = (p,q)

andElimL:: And p q -> p
andElimL s = fst s

andElimR:: And p q -> q
andElimR s = snd s

type Impl p q = (p -> q)

implIntro:: (p -> q) -> Impl p q
implIntro f = f

implElim:: Impl p q -> p -> q
implElim f = f

type Or p q = Either p q

orElim:: Or p q -> (p -> r) -> (q -> r) -> r
orElim (Left p) f g = f p
orElim (Right q) f g = g q

orIntroL:: p -> Or p q
orIntroL p = Left p

orIntroR:: q -> Or p q
orIntroR q = Right q

------------------------------------------------

func:: (Either (p, q) r -> (Either p r, Either q r),
       (Either p r, Either q r) -> Either (p, q) r)
func'::And (Impl (Or (And p q) r) (And (Or p r) (Or q r)))
           (Impl (And (Or p r) (Or q r)) (Or (And p q) r))
func = (f, g)
func' = func

f:: Either (p, q) r -> (Either p r, Either q r)
f (Left a) = (Left (fst a), Left (snd a))
f (Right r) = (Right r, Right r)


g:: (Either p r, Either q r) -> Either (p, q) r
g (Left p, Left q) = Left (p, q)
g (Left p, Right r) = Right r
g (Right r, Left q) = Right r
g (Right r, Right r') = Right r

----------------------------------------------------------------

next:: ((Either p q, r) -> Either (p, r) (q, r),
        Either (p, r) (q, r) -> (Either p q, r))
next = (h, i)

h:: (Either p q, r) -> Either (p, r) (q, r)
h (Left p, r) = Left (p, r)
h (Right q, r) = Right (q,r)

i:: Either (p, r) (q, r) -> (Either p q, r)
i (Left (p, r)) = (Left p, r)
i (Right (q, r)) = (Right q, r)
