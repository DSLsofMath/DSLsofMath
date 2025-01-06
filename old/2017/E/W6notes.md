Student questions:

1. I first defined a type for Ring-expressions as below. Does it
   matter if you include Const or if you skip it and instead have
   variable (with assignments)? I think it is the "designer's choice"
   and that it doesn't match in practise – is that correct?

```haskell
data Expr a  = Variable String
             | Const a
             | Expr a :+: Expr a
             | Expr a :*: Expr a
             | Neg (Expr a)
```

Answer 1: Generally speaking the abstract syntax should contain (only)
the operations of the structure / algebra / signature in question and
often also variables. So for Ring I would expect `(+)`, `negate`, `0`,
`(*)`, `1`, and `Var` but no `Const` case.

See W6/ExpR1.hs

You may take it one step further by noticing that we can express all
integers using repeated application of `(+)` to `1` and `0` plus use
of `negate`. Thus we could replace the constructors `0` and `1`
`FromInteger :: Integer -> Expr` but not a polymorphic embedding of
any type into a ring (as `Const` tries to).

See W6/ExpR2.hs

----

Question 2: Is it correct to say that `Expr a` is abstract syntax and
that `a` is the semantics? Or should one say that the `eval`-function
combined with an `Expr a` is the semantics?

Answer 2: Yes and no. The `ExpR1` (or the `ExpR2`) type is the
abstract syntax. Then the `eval` function and its domain is the
semantics. For an example, see W6/ExpR1.hs

----

Question 3: Is the idea with the last task to make a wrapper for the general `eval` function? For example, if I have
```haskell
evalExpr :: Ring a => (String -> a) -> Expr a -> a
```
and an instance declaration for `Ring Integer` should I then do something like:
```haskell
evalExprInt :: Ring a => (String -> Integer) -> Expr Integer -> Integer
evalExprInt = evalExpr
```
If so, what is the point?

Answer 3: The type specialisation is, as you note, trivial as long as
the infrastructure is set up correctly (abstract syntax type, type
class, class instance, class-polymorphic `eval` etc.). The "point" is
to get the infrastructure right and to show that you can give examples
of ring expressions and evaluate them "by hand". (Typical exam
questions.)
