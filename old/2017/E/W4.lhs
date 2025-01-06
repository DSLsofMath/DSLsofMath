Exercises for W4
================

1. Based on the [lecture notes](../L/03/W03.lhs), complete all the
   instance and datatype declarations and definitions in the files
   [FunNumInst.lhs](../L/DSLsofMath/FunNumInst.lhs),
   [FunExp.lhs](../L/DSLsofMath/FunExp.lhs),
   [Derive.lhs](../L/DSLsofMath/Derive.lhs),
   [EvalD.lhs](../L/DSLsofMath/EvalD.lhs), and
   [ShallowD.lhs](../L/DSLsofMath/ShallowD.lhs).

2. Write a function

> simplify  ::  FunExp -> FunExp

   to simplify the expression resulted from |derive|.  For example

> simplify (Const 0 :*: Exp Id)   =  Const 0
> simplify (Const 0 :+: Exp Id)   =  Exp Id
> simplify (Const 2 :*: Const 1)  =  Const 2
