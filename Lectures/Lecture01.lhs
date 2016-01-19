{-# LANGUAGE OverloadedStrings #-}
Lecture 01: Introduction
========================

> import Data.Set

- maths as study of "ideal languages"

- philosophies of maths
    + Platonism: ideal languages are about ideal objects ...
                 and they *exist*
    + Platonism, *New Math*-style: ideal languages are about ideal objects ...
                 and they are *sets*
    + formalism: ideal language = formal system
    + intuitionism: the only ideal languages worth studying are those
                 that Brouwer likes
    + constructivism: Brouwer likes programming languages (and he was
                 right)

- we start with logic (logic comes from logos, among other things
  "word")
    + this puts within a long tradition: "In the beginning was the
    Word, and the Word was with God, and the Word was God." John 1:1
    + modern mathematics is characterised by rigour, formality, logic:
    cue Mac Lane:

     >> "Proof in Mathematics is both a means to understand why some
     > result holds and a way to achieve precision. As to precision,
     > we have now stated an absolute standard of rigor: A
     > Mathematical proof is rigorous when it is (or could be) written
     > out in the first order predicate language $\mathcal L (\in)$ as
     > a sequence of inferences from the axioms ZFC, each inference
     > made according to one of the stated rules. Here 'written out'
     > need not mean only in the parsimonious language $\mathcal L
     > (\in)$; all sorts of symbols for derived concepts may appear,
     > since each occurrence of such a symbol can be replaced by its
     > (suitably precise) definition. To be sure, practically no one
     > actually bothers to write out such formal proofs. In practice,
     > a proof is a sketch, in sufficient detail to make possible a
     > routine translation of this sketch into a formal proof. When a
     > proof is in doubt, its repair is usually just a partial
     > approximation of the fully formal version. What is at hand is
     > not the *practice* of absolute rigor, but a *standard* of
     > absolute rigor."
     > [...]
     > For the concept of rigor we make a historical
     > claim : That rigor is absolute and here to stay. The future may
     > see additional axioms for sets or alternatives to set theory or
     > perhaps new more efficient ways of recording (or discovering)
     > proofs, but the notion of a rigorous proof as a series of
     > formal steps in accordance with prescribed rules of inference
     > will remain.


    + we want to understand this in the rest of the lecture (and to
    introduce functional programming, and to set the stage for things to
    come)

First-Order Languages
---------------------

- FOL:  A First-Order Language consists of
    - Syntax
        - names (constants)          c0, c1, ...
        - predicate symbols          P0, P1, ...
        - function symbols           f0, f1, ...
        - variables                  x0, x1, ...      xi is *not* in D
        - identity (equality)        =
        - logical connectives        ∧, ∨, ¬, →, ←→

        Notes:
            + in a given FOL there might be no names, function
              symbols, or predicates (except for =), but all FOLs have
              variables and identity
            + predicates and function symbols have *arities*
            + names can be considered *nullary* function symbols

         The elements of an FOL are combined to construct *sentences*.

    - Semantics
        - The sentences are about elements of a *domain of discourse*, D
        - interpretation (denotation, translation)
            + example: interpretName : Name → D
                - Note: interpretName may be neither injective nor surjective
            + example: interpretFunc : Func → (D → D) ∪ (D² → D) ∪ (D³ → D) ...
                - if f has arity n, interpretFunc f : D^n → D

    - Examples
        - an FOL for arithmetic:
            - a name ("zero")
                - alternatively, we could have several (as in decimal notation)
            - a function symbol ("succ", for successor)
                - or more ("+", "*", etc.)
            - no predicates
                - or several ("≤", "<", "divides", etc.)

        - an FOL for set theory
            - no names
                - or a name for the empty set
            - no function symbols
                - or ∪, ∩, P
            - one predicate: ∈
                - or several ($\subseteq$, etc.)

    - Atomic terms
        - defined recursively
        - names are atomic terms
        - if f has arity n, and t1, ..., tn are atomic terms, then f t1 ... tn is an atomic term

- Note: parsimonious set theory has no atomic terms!

An extended example: arithmetic
-------------------------------

Names and function symbols for arithmetic

> data AName  =  Zero        deriving (Eq, Show)
> data AFunc  =  Succ | Plus deriving (Eq, Show)

>{-
> data AtomicTerm name func =  N name | F func [AtomicTerm name func]
>                              deriving Show
>
> interpAtomicTerm :: AtomicTerm AName AFunc -> Integer
> interpAtomicTerm (N Zero)          = 0
> interpAtomicTerm (F Succ [t])      = 1 + interpAtomicTerm t
> interpAtomicTerm (F Plus [t0, t1]) = interpAtomicTerm t0 + interpAtomicTerm t1
> -}

> one        =  F Succ [N Zero]
> two        =  F Succ [one]
> testTerm0  =  F Plus [two, two]

     - For most purposes, we need *terms*

> data Term name func var  =  N name | F func [Term name func var] | V var
>                             deriving Show

A type for the arithmetic variables:

> type AVar  =  String
>
> testTerm1 = F Plus [V "x", testTerm0]
>

     We can only interpret terms with variables  in an *environment*:

> type Env var domain = [(var, domain)]

     We *look up* the value of a variable in an environment:

> lookUp x ((y, v):rest) = if x == y then v else lookUp x rest
>
> evalA env (N Zero)          = 0
> evalA env (V x)             = lookUp x env
> evalA env (F Succ [t])      = 1 + evalA env t
> evalA env (F Plus [t0, t1]) = evalA env t0  +  evalA env t1

     - Atomic terms are terms with no variables inside them

> termVars :: Ord var => Term name func var -> Set var
> termVars  = undefined


    - Well-formed formulas:

> data WFF name func var pred = P pred [Term name func var]
>                             | Eq (Term name func var) (Term name func var)
>                             | Forall var (WFF name func var pred)
>                             | Exists var (WFF name func var pred)
>                             | And (WFF name func var pred) (WFF name func var pred)
>                             | Or (WFF name func var pred) (WFF name func var pred)
>                             | Equiv (WFF name func var pred) (WFF name func var pred)
>                             | Impl (WFF name func var pred) (WFF name func var pred)
>                             | Not (WFF name func var pred)
>                             deriving (Show)

> freeVars :: Ord var => WFF name func var pred -> Set var
> freeVars (Eq t1 t2)      =  union (termVars t1) (termVars t2)
> freeVars (Forall x wff)  =  difference (freeVars wff) (singleton x)

     - Arithmetical predicate:

> data APred  =  LE          deriving (Eq, Show)

    - A *sentence* is a WFF with no free variables (cf. atomic term)

> type AWFF = WFF AName AFunc AVar APred

This theory is good, but what if we want to talk about multiplication? Or
do mathematics in general.

Enter set theory:

> data SName
> instance (Show SName) where show = show
> data SFunc
> instance (Show SFunc) where show = show
 
> type SVar = String
 
> type STerm = Term SName SFunc SVar

The only terms in Set are variables! 
 
> data SPred = ElementOf deriving (Show)
> type SWFF = WFF SName SFunc SVar SPred

Some objects can only be defined via a property, such as:

 + The empty set: “The only set X such that, for every x, x does not belong to x” 
 + The union of two sets A and B: “The only set X such that, for every x,
   x ∈ X iff x ∈ A ∨ x ∈ B

We extend:
 + WFF with an ∃! operator.
 + Terms with an ε-operator that gives us the witness of an ∃! expression.

 <https://en.wikipedia.org/wiki/Epsilon_calculus#Hilbert_notation>

> data WFF' name func var pred = P' pred [Term' name func var pred]
>                              | Eq' (Term' name func var pred) (Term' name func var pred)
>                              | Forall' var (WFF' name func var pred)
>                              | Exists' var (WFF' name func var pred)
>                              | And' (WFF' name func var pred) (WFF' name func var pred)
>                              | Or' (WFF' name func var pred) (WFF' name func var pred)
>                              | Impl' (WFF' name func var pred) (WFF' name func var pred)
>                              | Equiv' (WFF' name func var pred) (WFF' name func var pred)
>                              | Not' (WFF' name func var pred)
>                              | ExistsUnique var (WFF' name func var pred)
>                               deriving (Show)

> data Term' name func var pred = N' name | F' func [Term' name func var pred] | V' var
>                               | Epsilon (WFF' name func var pred)
>                               deriving (Show)
 
> type STerm' = Term' SName SFunc SVar SPred
> type SWFF'  = WFF'  SName SFunc SVar SPred

> comprehension :: SVar -> SVar -> SWFF' -> SWFF'
> comprehension set element prop = ExistsUnique set (Forall' element ((P' ElementOf [V' element, V' set]) `Equiv'` prop))

Here are some examples of comprehensions. 'v' is the variable that will be used
to name the set; being able to choose the name will be useful later: 

> emptySet v = Epsilon (comprehension v "y" (Not' (Eq' (V' "y") (V' "y"))))
> unionSet a b v = Epsilon (comprehension v "y" (Or' (P' ElementOf [V' "y", a]) (P' ElementOf [V' "y", b])))

Beware of variable capture!

Question: if we have a SWFF', can we convert it into a SWFF ? 

Yes! If ∃!x.P(x), then, for every Q(x),    Q(∃!x.P(x)) <==> ∀x.P(x) → Q(x) 

> noEpsS0 :: SWFF' -> SWFF
> noEpsS0 (P' ElementOf [a,b]) = P ElementOf [noEpsSTerm0 a, noEpsSTerm0 b]
> noEpsS0 (Eq' a b) = Eq (noEpsSTerm0 a) (noEpsSTerm0 b)
> noEpsS0 (Forall' var wff) = Forall var (noEpsS0 wff)
> noEpsS0 (Exists' var wff) = Exists var (noEpsS0 wff)
> noEpsS0 (ExistsUnique var wff) =
> -- We avoid substitution by keeping the value of var in a "y"
>   let wff0 = noEpsS0 wff in
>   Exists var (wff0 `And` (Forall "y"
>     ((Eq (V "y") (V var)) `Impl` (Forall var (wff0 `Impl` (Eq (V "y") (V var)))))))
> noEpsS0 (And' w1 w2) = And (noEpsS0 w1) (noEpsS0 w2)
> noEpsS0 (Or' w1 w2) = Or (noEpsS0 w1) (noEpsS0 w2)
> noEpsS0 (Impl' w1 w2) = Impl (noEpsS0 w1) (noEpsS0 w2)
> noEpsS0 (Equiv' w1 w2) = Equiv (noEpsS0 w1) (noEpsS0 w2)
> noEpsS0 (Not' w) = Not (noEpsS0 w)

> noEpsSTerm0 :: STerm' -> STerm
> noEpsSTerm0 = undefined

But this ^^ will never work

Let's try something different:
 
> noEpsSTerm :: STerm' -> (STerm -> SWFF) -> SWFF
> noEpsSTerm (V' var) q = q (V var) 
> noEpsSTerm (Epsilon (ExistsUnique var p)) q = Forall var (Impl (noEpsS p) (q (V var)))
 
> noEpsS :: SWFF' -> SWFF
> noEpsS (P' ElementOf [a,b]) = noEpsSTerm a $ \a' -> noEpsSTerm b $ \b' -> P ElementOf [a', b']
> noEpsS (Eq' a b) = noEpsSTerm a $ \a' -> noEpsSTerm b $ \b' -> Eq a' b'
> noEpsS (Forall' var wff) = Forall var (noEpsS wff)
> noEpsS (Exists' var wff) = Exists var (noEpsS wff)
> noEpsS (ExistsUnique var wff) =
>   let wff0 = noEpsS wff in
>   Exists var (wff0 `And` (Forall "y"
>     ((Eq (V "y") (V var)) `Impl` (Forall var (wff0 `Impl` (Eq (V "y") (V var)))))))
> noEpsS (And' w1 w2) = And (noEpsS w1) (noEpsS w2)
> noEpsS (Or' w1 w2) = Or (noEpsS w1) (noEpsS w2)
> noEpsS (Impl' w1 w2) = Impl (noEpsS w1) (noEpsS w2)
> noEpsS (Equiv' w1 w2) = Equiv (noEpsS w1) (noEpsS w2)
> noEpsS (Not' w) = Not (noEpsS w)

Here's an example of a WFF. By giving each set involved different names, we
avoid problems with variable capture.

> test1 = emptySet "a" `Eq'` (unionSet (emptySet "b") (emptySet "c") "d") 
> test1noEpsS = noEpsS test1

Exercise for the reader:

+ Write

  type Var = String
 
  noEpsTerm :: Term' name func Var pred -> (Term' name func var pred -> WFF name func var pred) -> WFF name func var pred
  noEps :: WFF' name func Var pred -> WFF' name func var pred 


  Hint: You can use the function:

  sequenceCont :: [(a -> b) -> b] -> (([a] -> b) -> b)
  sequenceCont [] k = k []
  sequenceCont (a:as) k = a $ \a' -> sequenceCont as $ \as' -> k (a':as')

> type Var = String
> 
> sequenceCont :: [(a -> b) -> b] -> (([a] -> b) -> b)
> sequenceCont [] k = k []
> sequenceCont (a:as) k = a $ \a' -> sequenceCont as $ \as' -> k (a':as')
  
> noEpsTerm :: Term' name func Var pred -> (Term name func Var -> WFF name func Var pred) -> WFF name func Var pred
> noEpsTerm (V' var) q = q (V var) 
> noEpsTerm (Epsilon (ExistsUnique var p)) q = Forall var (Impl (noEps p) (q (V var)))
 
> noEps :: WFF' name func Var pred -> WFF name func Var pred
> noEps (P' pred as) = sequenceCont [noEpsTerm a | a <- as] $ \as' -> P pred as'
> noEps (Eq' a b) = noEpsTerm a $ \a' -> noEpsTerm b $ \b' -> Eq a' b'
> noEps (Forall' var wff) = Forall var (noEps wff)
> noEps (Exists' var wff) = Exists var (noEps wff)
> noEps (ExistsUnique var wff) =
>   let wff0 = noEps wff in
>   Exists var (wff0 `And` (Forall "y"
>     ((Eq (V "y") (V var)) `Impl` (Forall var (wff0 `Impl` (Eq (V "y") (V var)))))))
> noEps (And' w1 w2) = And (noEps w1) (noEps w2)
> noEps (Or' w1 w2) = Or (noEps w1) (noEps w2)
> noEps (Impl' w1 w2) = Impl (noEps w1) (noEps w2)
> noEps (Equiv' w1 w2) = Equiv (noEps w1) (noEps w2)
> noEps (Not' w) = Not (noEps w)

This is equivalent to noEpsS for sets:

> test1noEps = noEps test1


Assignment:

Translate arithmetic (ℕ, 0, +, S) into set theory.
In other words, write a function AWFF -> SWFF.
