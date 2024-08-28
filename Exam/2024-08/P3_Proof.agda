-- Exam 2024-08: Problem 3: ``flavours of continuity''

{-
C(f)        =  ∀ c : X. Cat(f,c)
Cat(f,c)    =  ∀ ε > 0. ∃ δ > 0. Q(f,c,ε,δ)
Q(f,c,ε,δ)  =  ∀ x : X.  abs(x - c) < δ  ⇒  abs(f x - f c) < ε

C'(f)       =  ∃ getδ : X -> RPos -> RPos. ∀ c : X. ∀ ε > 0. Q(f,c,ε,getδ c ε)

4a: Define UC(f):

UC(f) = ∀ ε > 0. ∃ δ > 0. ∀ y : X. Q(f,y,ε,δ)

4b: Define UC'(f):

UC'(f) = ∃ newδ : RPos -> RPos. ∀ ε > 0. ∀ y : X. Q(f,y,ε,newδ ε)

The function |newδ| computes a suitable "global" |δ| from any |ε|,
which shows |Q| for any |y|.

4c: Prove |∀ f : X -> ℝ. UC'(f) => C'(f)|.

The proof is a function from a pair (newδ, puc) to a pair (getδ, pc).

proof f (newδ, puc) = (getδ, pc)
  where  getδ c ε = newδ ε
         pc c ε = puc ε c   -- (1)

The type of (1) is

  Q(f,c,ε,newδ ε)
=
  Q(f,c,ε,getδ c ε)

which is the type of |pc c ε|.

----------------

Below is a self-contained proof in Agda just to check the above. It is
not part of the exam question.

-}

postulate
  R RPos : Set
  abs : R -> RPos
  _-_ : R -> R -> R
  _<_ : RPos -> RPos -> Set

X = R  -- to avoid trouble with lack of subtyping

record Sigma (A : Set) (B : A -> Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B fst

Q : (X -> R) -> X -> RPos -> RPos -> Set
Q f c ε δ  =  forall x ->  (abs(  x  -    c)  < δ)  ->
                           (abs(f x  -  f c)  < ε)

Cat : (X -> R) -> X -> Set
Cat f c    =  forall ε ->  Sigma RPos (Q f c ε)

C : (X -> R) -> Set
C f        =  forall c ->  Cat f c

C' : (X -> R) -> Set
C' f =  Sigma  (X -> RPos -> RPos) (\getδ ->
               forall c -> forall ε -> Q f c ε (getδ c ε) )

UC : (X -> R) -> Set
UC f = forall ε -> Sigma  RPos (\δ ->
                          forall y -> Q f y ε δ)

UC' : (X -> R) -> Set
UC' f = Sigma  (RPos -> RPos) (\newδ ->
               forall ε -> forall y -> Q f y ε (newδ ε))

proof : (f : X -> R) -> UC' f -> C' f
proof f (newδ , puc) = (getδ , pc)
  where  getδ  = \c -> newδ
         pc    = \c ε -> puc ε c
