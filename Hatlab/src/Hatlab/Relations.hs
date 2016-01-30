{-# LANGUAGE FlexibleInstances #-}
module Hatlab.Relations where

import Hatlab.Plot

data Relation a = Relation (Double -> Double -> Bool) String |
                      Intersect a a |
                      Union a a |
                      Minus a a |
                      Compliment a

r f s = In (Relation f s)
r ./\. r' = In (Intersect r r')
r .\/. r' = In (Union r r')
r .\. r' = In (Minus r r')
c r = In (Compliment r)

instance Functor Relation where
    fmap f (Relation r s) = Relation r s
    fmap f (Intersect a a') = Intersect (f a) (f a')
    fmap f (Union a a') = Union (f a) (f a')
    fmap f (Minus a a') = Minus (f a) (f a')
    fmap f (Compliment a) = Compliment (f a)

type Algebra f a = f a -> a

label :: Relation String -> String
label (Relation _ s) = s
label (Intersect r1 r2) = "(intersection "++r1++" "++r2++")"
label (Union r1 r2) = "(union "++r1++" "++r2++")"
label (Minus r1 r2) = "("++r1++" minus "++r2++")"
label (Compliment r) = "(compliment "++r++")"

filter_fun :: Relation ((Double, Double) -> Bool) -> (Double, Double) -> Bool
filter_fun (Union r1 r2) p = (r1 p) || (r2 p)
filter_fun (Intersect r1 r2) p = (r1 p) && (r2 p)
filter_fun (Minus r1 r2) p = (r1 p) && (not (r2 p))
filter_fun (Relation r _) (x, y) = r x y
filter_fun (Compliment r) p = not (r p)

data Deep f = In (f (Deep f))

fold :: (Functor f) => Algebra f a -> Deep f -> a
fold g (In x) = g (fmap (fold g) x)

instance Plottable (Deep Relation) where
    plot [] = return ()
    plot rs = do plotCmd ["set size ratio -1\n"] 
                 plotCmd [headers rs]
                 plotCmd (map ((++"e\n") . p) rs)

        where
            headers (r : rs) = "clear\nplot [-2:2] "
                               ++ concat (map (\x -> x++", ") (header "'-' " r : map (header "'' ") rs))
            header str r = str ++ " w p pt 7 t " ++ show (fold label r)

            resolution = 400

            p r = concatMap show_ $
                  filter (fold filter_fun r) $
                  [(x, y) | x <- lspace resolution (-2.0, 2.0), y <- lspace resolution (-2.0, 2.0)]
                        
            show_ (x, y) = show x ++ " " ++ show y ++ "\n"

lspace :: Int -> (Double, Double) -> [Double]
lspace n (a, b) = itr n (\x -> x+h) a
    where
        h = ((b-a))/(fromIntegral (n-1))
        itr n = (take n .) . iterate
        
instance Show (Deep Relation) where
    show = fold label
