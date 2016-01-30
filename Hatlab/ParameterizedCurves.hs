{-# LANGUAGE FlexibleInstances #-}
module ParameterizedCurves where

import Plot

data Parameterized = Par {x_t :: Double -> Double, y_t :: Double -> Double, interval :: (Double, Double), label :: String}

instance Plottable (Parameterized) where
    plot [] = return ()
    plot rs = do plotCmd ["set size ratio -1\n"] 
                 plotCmd [headers rs]
                 plotCmd (map ((++"e\n") . p) rs)

        where
            headers (r : rs) = "clear\nplot ["++(show (min_v r))++":"++(show (max_v r))++"] "
                               ++ concat (map (\x -> x++", ") (header "'-' " r : map (header "'' ") rs))
            header str r = str ++ " w p pt 7 t " ++ (show (label r))

            resolution = 400

            p r = concatMap show_ $ ps r
            
            ps r = [(x_t r t, y_t r t) | t <- lspace resolution (interval r)]

            show_ (x, y) = show x ++ " " ++ show y ++ "\n"

            min_v :: Parameterized -> Double
            min_v r = minimum ((map fst (ps r))++(map snd (ps r)))

            max_v :: Parameterized -> Double
            max_v r = maximum ((map fst (ps r))++(map snd (ps r)))

lspace :: Int -> (Double, Double) -> [Double]
lspace n (a, b) = itr n (\x -> x+h) a
    where
        h = ((b-a))/(fromIntegral (n-1))
        itr n = (take n .) . iterate
