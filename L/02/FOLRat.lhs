%if False
\begin{code}
module FOLRat where
\end{code}
%endif

%if rat
\begin{code}
type VarT = String
data RatT = RV VarT | FromI Integer | RPlus RatT RatT | RDiv RatT RatT
  deriving Show
\end{code}
%endif

%if fol
%**TODO perhaps explain the notation for the list type
\begin{code}
type PSym = String
data FOL  =  PName PSym [RatT]
          |  Equal  RatT  RatT

          |  And      FOL   FOL
          |  Or       FOL   FOL
          |  Implies  FOL   FOL
          |  Not      FOL

          |  FORALL  VarT  FOL
          |  EXISTS  VarT  FOL
  deriving Show

commPlus :: FOL
commPlus = FORALL "x" (FORALL "y" (Equal  (RPlus (RV "x") (RV "y"))
                                          (RPlus (RV "y") (RV "x"))))
\end{code}
%endif
