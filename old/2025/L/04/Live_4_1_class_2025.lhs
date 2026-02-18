\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Live_4_1_class where
import Prelude hiding (  (+), (-), (*), (/), negate, recip, (^),
                         fromInteger, fromRational)
import qualified Prelude
type REAL = Double
\end{code}
Domain-Specific Languages of Mathematics course
Week & chapter 4: Compositionality and Algebras
Lecture 4.1

Based on the book §3.5 Type classes and §4.1 Algebraic structure.

1. Type classes Additive, Multiplicative.
2. Definition of classes AddGroup, Ring, MulGroup, Field
3. Homomorphisms in this context

----------------
1. Type classes Additive, Multiplicative.

+ The common underlying structure in that of a *monoid*.
+ A monoid is a triple (A,op,e) where op:A->A->A is associative and
  has e:A as a unit.


  1+2*3 =  1+(2*3)

\begin{code}
infixl 6 +
-- infixl 6 -

infixl 7 *
-- infixl 7 /

instance Additive REAL where (+) = (Prelude.+); zero = 0.0
instance Additive Bool where (+) = (||); zero = False  

class    Monoid         a    where op  :: a -> a -> a; unit :: a
class    Additive       a    where (+) :: a -> a -> a; zero :: a
class    Multiplicative a    where (*) :: a -> a -> a; one  :: a
instance Multiplicative REAL where (*) = (Prelude.*); one = 1.0

two :: (Additive a, Multiplicative a) => a
two = one + one
\end{code}

Function instances for Additive and Multiplicative:
\begin{code}
instance Additive b =>       Additive       (a->b) where (+) = addF; zero = zeroF
instance Multiplicative b => Multiplicative (a->b) where (*) = mulF; one  = oneF

zeroF :: Additive b =>        a -> b
zeroF = const zero

oneF  :: Multiplicative b =>  a -> b
oneF  = const one

addF :: Additive b =>        (a -> b) -> (a -> b) -> (a -> b)
mulF :: Multiplicative b =>  (a -> b) -> (a -> b) -> (a -> b)
addF f g = \x -> f x   +   g x
mulF f g = \x -> f x   *   g x

hej :: (Additive a, Multiplicative a) => a -> a
hej x = two*x
haj :: (Multiplicative a) => a -> a
haj y = y*y

test1 :: (Additive a, Multiplicative a) => a -> a
test1 = hej + haj

foo :: (Additive a, Multiplicative a) => a -> a -> a
foo x y = x*y + two

test2 :: (Additive a, Multiplicative a) => a -> a -> a
test2 = foo + (+)


-- Motivating example:
pythagorean :: REAL -> REAL
pythagorean = sin*sin + cos*cos
\end{code}

-- End of L4.1 (2025-02-11)

----------------
2. §4.1 Definition of classes AddGroup, Ring, MulGroup, Field

A *group* is a monoid with an inverse. We make one class AddGroup for
"additive monoid with inverse" and one class MulGroup for
"multiplicative monoid with inverse".

TODO class AddGroup + subtraction + instances
TODO class MulGroup + division    + instances
\begin{code}
class AddGroup a
class MulGroup a
\end{code}

A very common combination is "AddGroup + Multiplicative" - and this is
called a *ring*. And if we also add MulGroup we get a *field*.

\begin{code}
type Ring  a = (AddGroup a, Multiplicative a)
type Field a = (Ring a, MulGroup a)
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
