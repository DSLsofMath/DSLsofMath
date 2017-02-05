-- | A very simple library for manipulating continuous signals. Shallow embedding.
module Signal.Shallow
  ( Time
  -- | the 'Signal' type is abstract
  , Signal  
  -- * Smart constructors
  , constS, timeS
  -- * Combinators
  , ($$), mapT
  -- * Derived operation
  , mapS
  -- * Run function
  , sample
  ) where

-- * Smart constructors
constS :: a -> Signal a
timeS  ::      Signal Time
-- * Combinators
($$)   :: Signal (a -> b) -> Signal a -> Signal b
mapT   :: (Time -> Time)  -> Signal a -> Signal a
-- * Derived operation
mapS   :: (a -> b)        -> Signal a -> Signal b
-- * Run function
sample :: Signal a -> Time -> a  

type Time = Double
newtype Signal a = Sig {unSig :: Time -> a}

-- | The constant signal.
constS x = Sig (const x)

-- | The time signal
timeS = Sig id

-- | Function application lifted to signals.
fs $$ xs = Sig (\t -> unSig fs t  (unSig xs t))

-- | Mapping a function over a signal.
mapS f xs = constS f $$ xs

-- | Transforming the time.
mapT f xs = Sig (unSig xs . f)

-- | Sampling a signal at a given time point.
-- This is the /semantic function/ of our library.
sample = unSig
