-- | Some simple examples of 'Signal' usage.
module Signal.Example where
import Control.Monad (forM_)
import Signal
--------------------------------------------
-- Examples

-- | sinusoidal of given frequency
sinS :: Double -> Signal Double
sinS freq = mapT (freq*) $ mapS sin timeS

main :: IO ()
main = magic $ sinS 0.3

-- | Problem: averaging two signals?
-- @
-- averageS :: Fractional a => 
--             Signal a -> Signal a -> Signal a
-- averageS = -- ... Fill in a definition here ...
-- @
-- Hint: use the average function, mapS and $$ combinators.
average :: Fractional a =>  a -> a -> a
average x y = (x + y) / 2.0

scale :: Num a =>  Signal a -> Signal a
scale = mapS ((30*) . (1+))

-- | Discretize a signal
discretize :: Signal Double -> Signal Int
discretize = mapS round

-- | convert to "analog"
toBars :: Signal Int -> Signal String
toBars = mapS (`replicate` '#') 

displayLength = 100
-- | display the signal at a number of points
display :: Signal String -> IO ()
display ss = forM_ [0..displayLength] $ \x ->
   putStrLn (sample ss x)

-- | The display magic.
-- Note how we take advantage of function composition, 
-- types defined so far, etc.
magic :: Signal Double -> IO ()
magic = display . toBars . discretize . scale




----------------------------------------------------
-- | Answer to exercise
averageS :: Fractional a => 
            Signal a -> Signal a -> Signal a
averageS xs ys = mapS average xs $$ ys

-- | It can also be generalised to an arbitray Applicative functor
averageA :: (Fractional a, Applicative f) => 
             f a -> f a -> f a
averageA xs ys = average <$> xs <*> ys
-- | or slightly shorter
averageA' :: (Fractional a, Applicative f) => 
             f a -> f a -> f a
averageA' = liftA2 average

-- | Control.Applicative:
-- <http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Applicative.html>
