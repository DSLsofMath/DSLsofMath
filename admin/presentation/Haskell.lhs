\newcommand{\untypedcode}{%
\begin{code}
kvadrat x   =  x*x

twice f     =  \x -> f(f(x))

upphöjFyra  =  twice kvadrat

test        =  upphöjFyra 3

list        =  [(1+), (2*), kvadrat, upphöjFyra]
\end{code}
}

\newcommand{\typedcode}{%
\begin{code}
kvadrat     :: Num a =>  a -> a

twice       :: (a -> a) -> (a -> a)

upphöjFyra  :: Num a =>  a -> a

test        :: Double

list        :: Num a =>  [a -> a]
\end{code}
}
