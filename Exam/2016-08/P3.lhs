\begin{code}
module P3 where
import PS

-- 3a.

exps  = integ exps 1
fs    = integ fs'  1
fs'   = integ fs'' 4
fs''  = -6*fs + 5*fs' + exps
\end{code}


For testing:
\begin{code}
test = take 3 (coeff fs) == [1,4,15/2]

main = print test
\end{code}
