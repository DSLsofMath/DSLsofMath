{-# LANGUAGE GADTs #-}
module Either where
import Prelude hiding (Either(Left,Right), either)

data Either p q = Left p | Right q

either :: (p->r) -> (q->r) -> (Either p q -> r)
either l r (Left x)   =  l x
either l r (Right y)  =  r y

