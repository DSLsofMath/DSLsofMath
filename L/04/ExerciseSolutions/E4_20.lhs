E4_3: Complete the instance declarations for |Dup REAL|, deriving them
  from the homomorphism requirement for |applyFD c|.

\begin{code}
module E4_3 where
import DSLsofMath.FunNumInst

newtype FD a  = FD (a -> a, a -> a)
newtype Dup a = Dup (a, a)

applyFD ::  a ->  FD a          ->  Dup a
applyFD     c     (FD (f, f'))  =   Dup (f c, f' c)

\end{code}
The starting point is the definition of |addFD|, |mulFD|, etc.

  mulFD (FD (f,f')) (FD (g,g')) = FD (f*g, f'*g+f*g')

Here is a calculation of the multiplication case (which is also in the
lecture notes). The aim is to find an operator

  mulDup :: Dup a -> Dup a -> Dup a

that satisfies H2(applyFD c, mulFD, mulDup) (for any c).

This calculation is done in a Haskell list just to get some help from
the typechecker - it could also be done on paper.

\begin{code}
mulEqChain :: Num a => a -> FD a -> FD a -> [Dup a]
mulEqChain c  a@(FD (f, f'))   b@(FD (g, g'))  =
  let  x = f c; y = g c; x' = f' c; y' = g' c
       h = applyFD c   -- h :: FD a -> Dup a
  in
  [  h (a * b)
  ,  h (FD (f, f') * FD (g, g'))                   {- =  def. |*| for |FD a| -}
  ,  h (FD (f * g, f' * g + f * g'))               {- =  def. |h = applyFD c| -}
  ,  Dup ((f * g) c, (f' * g + f * g') c)          {- =  def. |*| and |+| for functions -}
  ,  Dup (f c * g c, f' c * g c + f c * g' c)      {- =  use shorter names -}
  ,  Dup (x   * y  ,   x' * y   +   x * y')
   -- **Define (*) on (Dup a) to fill in this step**
  ,  Dup (x  , x'  )  *  Dup (y  , y'  )           {- =  use shorter names -}
  ,  Dup (f c, f' c)  *  Dup (g c, g' c)           {- =  def. |h = applyFD c| twice -}
  ,  h (FD (f, f'))   *  h (FD (g, g'))
  ,  h a * h b
  ]
\end{code}

This is the core of the definition:

\begin{code}
(*?) :: Num t => (t, t) -> (t, t) -> (t, t)
(x, x') *? (y, y')  =  (x * y, x' * y + x * y')

-- which can be used in both mulFD and mulDup

mulFD :: Num a =>  FD a -> FD a -> FD a
mulFD (FD fs) (FD gs) = FD (fs *? gs)
-- mulFD (FD (f,f')) (FD (g,g')) = FD (f*g, f'*g+f*g')

mulDup :: Num a =>  Dup a -> Dup a -> Dup a
mulDup (Dup xs) (Dup ys)  =  Dup (xs*?ys)
-- mulDup (Dup (x, x') (Dup (y, y'))  =  Dup (x*y, x'*y + x*y')
\end{code}


Similarly we can derive |addDup| from |addFD|:

\begin{code}
addFD :: Num a => FD a -> FD a -> FD a
addFD  (FD (f, f')) (FD (g, g')) = FD (f+g, f'+g')

addEqChain :: Num a => a -> FD a -> FD a -> [Dup a]
addEqChain c  a@(FD (f, f'))   b@(FD (g, g'))  =
  let  x = f c; y = g c; x' = f' c; y' = g' c
       h = applyFD c   -- h :: FD a -> Dup a
  in
  [  h (a + b)
  ,  h (FD (f, f') + FD (g, g'))                   {- =  def. |+| for |FD a| -}
  ,  h (FD (f + g,     f' + g'))                   {- =  def. |h = applyFD c| -}
  ,  Dup ((f + g) c,  (f' + g') c)                 {- =  def. |+| for functions -}
  ,  Dup (f c + g c,  f' c + g' c)                 {- =  use shorter names -}
  ,  Dup (x   + y  ,    x' + y')
   -- **Define (+) on (Dup a) to fill in this step**
  ,  Dup (x  , x'  )  +  Dup (y  , y'  )           {- =  use shorter names -}
  ,  Dup (f c, f' c)  +  Dup (g c, g' c)           {- =  def. |h = applyFD c| twice -}
  ,  h (FD (f, f'))   +  h (FD (g, g'))
  ,  h a + h b
  ]
\end{code}

Thus we arrive at:

\begin{code}
addDup :: Num a => Dup a -> Dup a -> Dup a
addDup (Dup (x, x')) (Dup (y, y')) = Dup (x+y, x'+y')
\end{code}

TODO: add more cases.

\begin{code}
instance Num a => Num (FD a) where
  (*) = mulFD
  (+) = addFD

instance Num a => Num (Dup a) where
  (*) = mulDup
  (+) = addDup
\end{code}
