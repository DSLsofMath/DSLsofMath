
data PropCalc where
  N        :: Name -> PropCalc
  C        :: Bool -> PropCalc
  And      :: PropCalc -> PropCalc -> PropCalc
  Or       :: PropCalc -> PropCalc -> PropCalc
  Implies  :: PropCalc -> PropCalc -> PropCalc
  Not      :: PropCalc -> PropCalc
