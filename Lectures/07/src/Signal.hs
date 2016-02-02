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
-- | TODO: Fix Haddock Ticket #121 (could be part of lab 3 for eager students)
-- <http://trac.haskell.org/haddock/ticket/121>

-- | 'Signal' is an applicative functor
instance Functor Signal where
  fmap = mapS

instance Applicative Signal where
  pure  = constS
  (<*>) = ($$)
