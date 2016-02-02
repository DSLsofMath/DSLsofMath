-- | Combining 'Signal' and 'Shape' to obtain moving objects.
module Animate where

import System.Posix (usleep)

import Signal
import Shape
import Render
import ANSI

fps :: Double
fps = 10   -- we generate 10 fps

-- | Animate a shape valued signal.
animate :: Window -> Time -> Time -> Signal Shape -> IO ()
animate win t0 t1 ss = mapM_ display frames
  where
    ts     = samples t0 t1 (round $ (t1 - t0) * fps)
    frames = map (sample ss) ts

    display sh = do
      putStr $ ansiClearScreen ++ ansiGoto 1 1 ++ render win sh
      usleep 70000  -- sleeping removes flickering

