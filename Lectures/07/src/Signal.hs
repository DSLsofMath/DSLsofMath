-- | A very simple library for manipulating continuous signals.
module Signal
  ( module SignalImpl
  , module Signal
  , module Control.Applicative
  ) where

import Control.Applicative
-- Alternative implementation:
-- > import Signal.Shallow as SignalImpl
import Signal.Deep as SignalImpl

-- | 'Signal' is an applicative functor
instance Functor Signal where
  fmap = mapS

instance Applicative Signal where
  pure  = constS
  (<*>) = ($$)
