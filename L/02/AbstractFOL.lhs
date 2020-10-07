%if abstractfol
% (  And, Or, Impl, Not, notIntro, notElim, implIntro, implElim,
%      andIntro, andElimL, andElimR,  orIntroL, orIntroR, orElim
%   )
\begin{code}
module DSLsofMath.AbstractFOL where

data And   p  q;                                     data Or    p  q
data Impl  p  q;                                     data Not   p

andIntro       ::  p -> q -> And p q;                orElim       ::  Or p q -> (p -> r) -> (q -> r) -> r
andElimL       ::  And p q -> p;                     orIntroL     ::  p -> Or p q
andElimR       ::  And p q -> q;                     orIntroR     ::  q -> Or p q

implIntro      ::  (p -> q) -> Impl p q;{-"\quad"-}  notIntro     ::  (p -> And q (Not q)) -> Not p
implElim       ::  Impl p q -> p -> q;               notElim      ::  Not (Not p) -> p

andIntro = u; orElim = u; andElimR = u; orIntroL = u; andElimL = u; orIntroR = u;
implIntro = u; notElim = u; notIntro = u; implElim = u; u = undefined;
\end{code}
%endif
%\end{code}
%\begin{code}
