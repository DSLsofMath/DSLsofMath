module Plot where

import Control.Concurrent
import System.Process
import System.IO
import System.IO.Unsafe
import Data.List

import qualified Data.Vector.Storable as V

-- Low-level things to start and connect to gnuplot ----------------------------

handle :: Handle
handle = unsafePerformIO $
           do (inp,_out,_err,pid) <-
                  runInteractiveProcess "/usr/local/bin/gnuplot" [] Nothing Nothing
              hSetBinaryMode inp False
              hSetBuffering inp LineBuffering
              hPutStrLn inp "set multiplot\nplot 1\nclear"
              return inp

plotCmd :: [String] -> IO ()
plotCmd strs = do
                 -- For debugging, remove comment in next line to see gnuplot cmds in terminal
                 -- mapM_ putStrLn strs
                 mapM_ (hPutStrLn handle) strs
                 return ()

-- Downscaled version of Hatlab plotting, to avoid all Chebyshev things.
-- Still vectors, but functions do not take vector arguments (to avoid having to install hmatrix).

class Plottable a where
   plot :: [a] -> IO ()

data BasicPlot = Fun  (Double -> Double) String
               | Pts  (V.Vector Double)  (Double -> Double) String
               | Pts2 (V.Vector Double)  (V.Vector Double) String


instance Plottable BasicPlot where
  plot [] = return ()
  plot fs = do plotCmd [headers fs]
               plotCmd (map ((++"e\n") . p) fs)
    where headers (f : fs) = "clear\nplot ["++show a++":"++show b++"] "
                              ++ concat (intersperse ", " (header "'-' " f : map (header "'' ") fs))
          -- In this downscaled version (a,b) is always (-1,1)  ...
              where (a,b) = case f of
                              _      -> (-1,1)
          header str (Fun _ lab)    = str ++ " w l lw 2 t " ++ show lab
          header str (Pts _ _ lab)  = str ++ " w p pt 7 t " ++ show lab
          header str (Pts2 _ _ lab) = str ++ " w p pt 7 t " ++ show lab

          p :: BasicPlot -> String
          p (Fun f _) = plotFun xs (V.map f xs)
              where xs = linspace 1001 (-1,1)
          p (Pts xs f lab) = plotFun xs (V.map f xs)
          p (Pts2 xs ys lab) = plotFun xs ys

          plotFun xs ys =  V.ifoldr g "\n" xs
              where g i x ack = show x ++ " " ++ show (ys V.! i) ++ "\n" ++ ack


linspace n (a,b) = V.generate n (\k -> a + fromIntegral (k-1) * h)
       where h = (b-a)/fromIntegral (n-1)
