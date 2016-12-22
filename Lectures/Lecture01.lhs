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
        - variables                  x0, x1, ...      xi is *not* in the domain D
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
