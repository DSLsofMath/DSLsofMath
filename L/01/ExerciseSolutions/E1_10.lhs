E1.10: Read the full chapter and complete the definition of the
instance for |Num| for the datatype |ComplexSyn|. Also add a
constructor for variables to enable writing expressions like |(Var
"z") :*: ToComplex 1|.

Code from the book:
\begin{code}
instance Num a => Num (ComplexSyn a) where
   (+) = (:+:); (*) = (:*:)
   fromInteger = fromIntegerCS
   negate = negateCS
   abs = absCS; signum = signumCS

toComplexSyn :: Num a => a -> ComplexSyn a
toComplexSyn x = ToComplexCart x 0

fromIntegerCS :: Num r =>  Integer -> ComplexSyn r
fromIntegerCS = toComplexSyn . fromInteger

negateCS :: Num a => ComplexSyn a -> ComplexSyn a
negateCS = ((-1) :*:)
\end{code}

New code: adding absolute value and sign. Step 1 is typing: while we
are used to abs returning a real (non-negative) number, the |Num|
class requires the return type of |abs| to equal the input type:
|abs :: Num a => a -> a|. Thus we will need to return a complex number
(with no imaginary part) from |abs|.

Step 2 is semantics. It is not immediately clear what |abs| and
|signum| should do for complex numbers, but we can try with this
specification:

     z == abs z * signum z
  && re (abs z) >= 0
  && im (abs z) == 0
  && abs (signum z) == 1

This means that we "factor" the complex number |z| into a real part
(|abs z|) and the "direction" (|signum z|). For real numbers there are
just two directions: +1 and -1, but for complex numbers the direction
could anywhere on the unit circle (including the special cases +-1 and
+-i). This can be satisfied by the following implementation on the
semantic side:

modulusSquaredC :: Ring r => Complex r -> r
modulusSquaredC (C (x, y)) = x^+2 + y^+2
abs = toC . sqrt . modulusSquaredC
signum z = z / abs z

Unfortunately, the syntactic side does not have access to these
operations and thus the easiest way is to just add two new
constructors (|Abs| and |Signum|) and the new constructor for
variables (|Var|):

\begin{code}
absCS = Abs
signumCS = Signum
data ComplexSyn r  =  Var String               -- New
                   |  Abs     (ComplexSyn r)   -- New
                   |  Signum  (ComplexSyn r)   -- New
                   |  ToComplexCart r r
                   |  ComplexSyn r  :+:  ComplexSyn r
                   |  ComplexSyn r  :*:  ComplexSyn r
\end{code}

