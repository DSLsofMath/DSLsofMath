E5_3: Prove that, with the definition of |x = [0, 1]| we really have

    as = a0 + a1 * x + a2 * x^2 + ... + an * x^n

The notation may be confusing here, because we are so used to thinking
of |x| as a variable. In this exercise, |x| is just a constant list
denoting the polynomial |p x = x|: the constant is zero and the
coefficient in front of the variable |x| is one.

The task is to show that for any polynomial |q = eval as|, where |as =
[a0, a1, ..., an]|, we can express it as the usual sum of |ai*xi^i|.

The core lemma is that multiplying by this "syntactic x" = [0,1]"
works as shifting the coefficients one step.

  lemma : Forall as.  as*[0,1] == 0:as
  lemma as = -- semi-formal proof:
      as*[0,1]
    = -- def. of (*) for polynomials
      (a0*0):((tail as*[0,1])+([a0]*tail [0,1]))
    = -- simplify
      0 : ((tail as*[0,1])+[a0*1])
    = -- induction
      0 : ((0:tail as)+[a0])
    = -- (+) for polynomials
      0 : (a0:tail as)
    = -- simplify
      0 : as

Now we can prove the main theorem by structural induction over
non-empty lists |as|:

  P as = {- informally -}     as == a0 + a1 * x + a2 * x^2 + ... + an * x^n

Structural induction over non-empty lists says:

    ( (Forall a. P [a]) &   -- base case
      (Forall a. Forall as. P as => P (a:as))) -- step case
  => (Forall as. P as)

The base case is easy:

  Forall a0. P [a0]
= -- def. of P
  Forall a0. [a0] == fromInteger a0
= -- def. of fromInteger c = [c]
  True

and the step case uses the core lemma:

  step : Forall a. Forall as. P as => P (a:as)

  step a as indhyp = -- semi-formal proof follows:
    -- Note that indhyp :      as  == a0 + a1 * x + a2 * x^2 + ... + a_{n-1} * x^(n-1)
    -- and we want to prove (a:as) == a  + a0 * x + a1 * x^2 + ... + a_{n-1} * x^n
    --   (note the indexing)
    -- We compute:

      (a:as)
    = -- (+) for polynomials
      [a] + (0:as)
    = -- lemma (and x=[0,1])
      [a] + as*x
    = -- indhyp
      [a] + (a0 + a1 * x + a2 * x^2 + ... + a_{n-1} * x^(n-1))*x
    = -- distributivity on the right
      [a] + (a0*x + a1*x^2 + a2*x^3 + ... + a_{n-1} * x^n)
    = -- fromInteger for polynomials
      a  + a0 * x + a1 * x^2 + ... + a_{n-1} * x^n
