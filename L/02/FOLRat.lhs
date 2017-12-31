%if False
\begin{code}
module DSLsofMath.FOLRat where
\end{code}
%endif

%if rat
\begin{code}
data RatT = RV String | FromI Integer | RPlus RatT RatT | RDiv RatT RatT
  deriving Show
\end{code}
%endif

%if fol
\begin{code}
data FOL  =  P String [RatT]
          |  Equal  RatT  RatT

          |  And      FOL   FOL
          |  Or       FOL   FOL
          |  Implies  FOL   FOL
          |  Not      FOL

          |  FORALL  String  FOL
          |  EXISTS  String  FOL
  deriving Show

commPlus :: FOL
commPlus = FORALL "x" (FORALL "y" (Equal  (RPlus (RV "x") (RV "y"))
                                          (RPlus (RV "y") (RV "x"))))
\end{code}
%endif
