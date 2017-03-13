data Expr a  = Variable String
             | Const a
             | Expr a :+: Expr a
             | Expr a :*: Expr a
             | Neg (Expr a)
