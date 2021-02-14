\begin{code}
module DSLsofMath.FunExpInst where
import DSLsofMath.FunExp
import DSLsofMath.Algebra
-- type G = ()
-- instance Generate        FunExp where  generate () = X
instance Additive        FunExp where  (+)  = (:+:);  zero  = Const 0
instance Multiplicative  FunExp where  (*)  = (:*:);  one   = Const 1
instance AddGroup        FunExp where  negate  = Negate
instance MulGroup        FunExp where  recip   = Recip
instance Transcendental  FunExp where  pi = Const (Prelude.pi); exp = Exp; sin = Sin; cos = Cos
\end{code}
