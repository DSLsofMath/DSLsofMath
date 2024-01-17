Which non-square integers have an "exact" root in floating point arithmetics?

This little piece of codes searches for some examples and finds
λ> testF
(True,(3,1.7320508,3.0,0.0))
(True,(5,2.236068,5.0,0.0))
(True,(10,3.1622777,10.0,0.0))
...
λ> testD
(True,(11,3.3166247903554,11.0,0.0))
(True,(14,3.7416573867739413,14.0,0.0))
(True,(17,4.123105625617661,17.0,0.0))
...

\begin{code}
import Data.List( (\\) )
type Explore a = (Bool, (Integer, a, a, a))
explore :: (Eq a, Floating a) => Integer -> Explore a
explore n = (exact, (n, r, r2, diff))
  where  n' = fromInteger n
         r  = sqrt n'
         r2 = r^2
         diff = r2-n'
         exact = diff == 0.0

oneUp = [1..]
squares = take 1000 (map (^2) oneUp)
nonSquares = oneUp \\ squares

ps :: (Eq a, Floating a) => [Explore a]
ps = filter fst (map explore nonSquares)

printSome :: Show a => [Explore a] -> IO ()
printSome = mapM_ print . take 10 

testF = printSome (ps :: [Explore Float ])
testD = printSome (ps :: [Explore Double])

main = do
  testF
  testD
\end{code}


