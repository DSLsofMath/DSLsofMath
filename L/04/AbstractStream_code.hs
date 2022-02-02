{-# LANGUAGE FlexibleContexts #-}
module AbstractStream where
import Prelude hiding (head, tail)

data X
data A
head  ::  X  ->  A
tail  ::  X  ->  X
cons  ::  A  ->  X  ->  X

law1     s  = {-"\qquad"-}  s  == cons  (head s) (tail s)
law2  a  s  =               s  == tail  (cons a s)
law3  a  s  =               a  == head  (cons a s)

(head, tail, cons) = undefined
law1 :: Eq X => X -> Bool
law2 :: Eq X => A -> X -> Bool
law3 :: Eq A => A -> X -> Bool

toList :: X -> [A]
toList x = head x : toList (tail x)

constS :: A -> X
constS a = ca
  where ca = cons a ca

