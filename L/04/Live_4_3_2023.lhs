\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds   #-}
module Live_4_3 where
import Prelude hiding (  (+), (-), (*), (/), negate, recip, (^),
                         fromInteger, fromRational)
import qualified Prelude  -- 1 Prelude.+ 2
type REAL = Double
\end{code}
Domain-Specific Languages of Mathematics course
Week & chapter 4: Compositionality and Algebras
Lecture 4.3 (cont. from L3.3 about numeric classes)

Based on the book §4.1: Algebraic structure.

1. Reminder about the type classes Additive, Multiplicative.
2. Definition of classes AddGroup, Ring, MulGroup, Field
3. Homomorphisms in this context

----------------
1. Reminder about the type classes Additive, Multiplicative.

+ The common underlying structure in that of a *monoid*.
+ A monoid is a triple (A,op,e) where op:A->A->A is associative and
  has e:A as a unit.

\begin{code}
infixl 6 +
infixl 6 -

infixl 7 *
infixl 7 /

-- class    Monoid   a          where op  :: a -> a -> a; e :: a

-- "additivie monoid"
class    Additive a          where (+) :: a -> a -> a; zero :: a
-- "multiplicative monoid"
class    Multiplicative a    where (*) :: a -> a -> a; one :: a

instance Additive REAL       where (+) = (Prelude.+);  zero = 0
instance Multiplicative REAL where (*) = (Prelude.*);  one = 1

two :: (Additive a, Multiplicative a) => a
two = one + one
\end{code}

Function instances for Additive and Multiplicative:
\begin{code}
instance Additive b =>       Additive       (a->b) where (+) = addF; zero = zeroF
instance Multiplicative b => Multiplicative (a->b) where (*) = mulF; one  = oneF

-- instance Additive String where (+) = (++); zero = ""

zeroF :: Additive b =>        a -> b
oneF  :: Multiplicative b =>  a -> b
zeroF = const zero 
oneF  = const one

addF :: Additive b =>        (a -> b) -> (a -> b) -> (a -> b)
mulF :: Multiplicative b =>  (a -> b) -> (a -> b) -> (a -> b)
addF f g = \x -> f x  +  g x
mulF f g = \x -> f x  *  g x

-- Motivating example:
pythagorean :: REAL -> REAL
pythagorean = sin*sin + cos*cos
\end{code}

----------------
2. §4.1 Definition of classes AddGroup, Ring, MulGroup, Field

A *group* is a monoid with an inverse. We make one class AddGroup for
"additive monoid with inverse" and one class MulGroup for
"multiplicative monoid with inverse".

TODO class AddGroup + subtraction + instances
TODO class MulGroup + division    + instances
\begin{code}
class Additive a => AddGroup a where negate :: a -> a   -- additive inverse
-- Forall x.   x + negate x == zero
instance AddGroup REAL where negate = Prelude.negate
instance AddGroup b => AddGroup (a->b) where negate = negateF

negateF :: AddGroup b => (a->b) -> (a->b)
negateF f = \x -> negate (f x)

(-) :: AddGroup a => a -> a -> a
x - y = x + negate y

class Multiplicative a => MulGroup a where recip :: a -> a
instance MulGroup REAL where recip = Prelude.recip
(/) :: MulGroup a => a -> a -> a
x / y = x * recip y
\end{code}

A very common combination is "AddGroup + Multiplicative" - and this is
called a *ring*. And if we also add MulGroup we get a *field*.

\begin{code}
--   Ring ~=  ((+), zero, negate), ((*), one) 
type Ring  a = (AddGroup a, Multiplicative a)  -- Sv: ring 
type Field a = (Ring a, MulGroup a)            -- Sv: kropp
--   Field ~=  ((+), zero, negate), ((*), one, recip) 
--   Field ~=  (+),(-),(*),(/)

hej :: Ring a => a -> a
hej x = x*x - x

\end{code}

----------------
3. Homomorphisms in this context

Given these predicates:

H2(h,opA,opB) = forall x, y. h (opA x y) == opB (h x) (h y)
  where  h   :: A -> B
         opA :: A -> A -> A
         opB :: B -> B -> B

H1(h,fA,fB) =         forall x. h (fA x) == fB (h x)
  where  h  :: A -> B
         fA :: A -> A
         fB :: B -> B

H0(h,eA,eB) =                       h eA == eB
  where  h  :: A -> B
         eA :: A
         eB :: B

We can define:

+ Monoid homomorphism:
  MonHom(h,(A,opA,eA),(B,opB,eB)) = H2(h,opA,opB) && H0(h,eA,eB)

+ Ring homomorphism:
  RingHom(h, (A,(+A),zeroA,negateA,(*A),oneA),
             (B,(+B),zeroB,negateB,(*B),oneB) ) = 
    MonHom(h,(A,(+A),zeroA),(B,(+B),zeroB)) &&
    MonHom(h,(A,(*A),oneA), (B,(*B),oneB))  &&
    H1(h,negateA,negateB)
  but it can also be simplied to just
    H2(h,(+A),(+B)) && H2(h,(*A),(*B)) && H0(h,oneA,oneB)
  (the other two follow from these three).

+ Similarly for Fields, etc.
