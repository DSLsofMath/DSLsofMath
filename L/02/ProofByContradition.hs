-- What would it take to do the proof that sqrt 2 is irrational in Haskell? A sketch.

data ZZ where
  GCD,(:*:) :: ZZ -> ZZ -> ZZ
  One :: ZZ
  Two :: ZZ
type Pos = ZZ
data (==) (a::ZZ) (b::ZZ) where
data GCD  (a::ZZ) (b::ZZ)  where
data R x where
  Decomposition :: (forall (a::ZZ). forall (b::Pos). Not ((b ':*: x) == a `And` ('GCD a b == 'One))) -> R x
data Sqrt2Rational where
  ConstructionS2 :: forall r. ((r ':*: r) == 'Two) `And` R r -> Sqrt2Rational

-- squareEq :: a == b -> a :*: a == b :*: b
-- squareEq = error "assumed"

proof :: Sqrt2Rational -> False
proof (ConstructionS2 (p,q)) = error "tedious"
\end{code}
