Summary of L4.1

* |data IE| for integer expressions
* from |eval| to |foldE|
* |class IntExp|
* black-board: homomorphisms
* black-board: def. of eval'

----------------

Summary of L4.2: [no live coding this time]

* More about homomorphisms (see definitions below)
* show that eval' is not a homomorphism
    * not H2(eval',(:*:),(*))
* tupling transform
* show that evalD is a homomorphism
* compositional semantics means we can work without syntax trees
* Teaser: an unusual stream & the fundamental theorem of (integral) calculus
* TODO: |apply c| is a |Num|-homomorphism
* TODO: |applyFD c| as well


type FunSem = REAL -> REAL

Homomorphism2(h,op1,op2) = forall x, y. h(op1 x y) == op2 (h x) (h y)
  where  h   :: A1 -> A2
         op1 :: A1 -> A1 -> A1
         op2 :: A2 -> A2 -> A2

Homomorphism1(h,op1,op2) = forall x. h(op1 x) == op2 (h x)
  where  h   :: A1 -> A2
         op1 :: A1 -> A1
         op2 :: A2 -> A2

Example:

Homomorphism2(eval,(:+:),(+)) = forall x, y. eval(x:+:y) == (eval x)+(eval y)
  where  eval  :: FunExp -> FunSem
         (:+:) :: FunExp -> FunExp -> FunExp
         (+)   :: FunSem -> FunSem -> FunSem

Homomorphism1(eval,Exp,exp) = forall e. eval(Exp e) == exp (eval e)
  where  eval  :: FunExp -> FunSem
         Exp   :: FunExp -> FunExp
         exp   :: FunSem -> FunSem
