\begin{code}
test =
  [ (1+)        =?= \x->1+x
  , (*2)        =?= \x->x*2
  , (1+).(*2)   =?= \x->1+x*2
  , (*2).(1+)   =?= \x->(1+x)*2
  , (+1).(^2)   =?= \x->x^2+1
  , (^2).(+1)   =?= \x->(x+1)^2
  , (a+).(b+)   =?= \x->a+(b+x) -- == (\x->(a+b)+x) == ((a+b)+)
  ]  where a = 17; b=38

type Fun = Int -> Int

infix 4 =?=

(=?=) :: Fun -> Fun -> Bool
f =?= g = all samePointwise [0..2] -- testing at 3 points enough for 2-deg. polynomials
  where samePointwise i = f i == g i

\end{code}
