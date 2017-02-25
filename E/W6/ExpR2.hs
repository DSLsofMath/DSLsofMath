data ExpR = ExpR :+: ExpR
          | Negate ExpR
          | ExpR :*: ExpR
          | FromInteger Integer
          | Var String

zero, one :: ExpR
zero  = FromInteger 0
one   = FromInteger 1
