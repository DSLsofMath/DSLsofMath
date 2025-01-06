Exercises for 2016-02-17
========================

1. Complete the instance declarations for Expression (for Num,
   Fractional, and Floating).

2. Complete the instance declarations for (Double, Double), deriving
   them from the homomorphism requirement for apply.

3. We now have three different ways of computing the derivative of a
   function such as f x = sin x + exp (exp x) at a given point, say
   pi.

   a.  Find e :: Expressions such that eval e = f and use eval'.

   b.  Find an expression of type FD Double and use apply.

   c.  Apply f directly to the appropriate (x, x') and use snd.
