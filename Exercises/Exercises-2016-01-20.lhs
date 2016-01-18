Exercises for the session 2016-01-20
------------------------------------

1. During the lecture, we defined the following datatype for atomic terms:

> data AtomicTerm name func  =  N name | F func [AtomicTerm name func]
>                               deriving Show

We then interpreted atomic terms for the FOL of arithmetic:

< interpAtomicTerm :: AtomicTerm AName AFunc -> Integer
< interpAtomicTerm (N Zero)           =  0
< interpAtomicTerm (F Succ [t])       =  interpAtomicTerm t + 1
< interpAtomicTerm (F Plus [t0, t1])  =  interpAtomicTerm t0 + interpAtomicTerm t1

This implementation obscures somewhat the fact that the functions of
the language are interpreted in terms of functions in the domain.  In
general, if D is the domain of discourse of an FOL with Name and Func,
then

< interpName   ::  Name -> D -- names are associated to elements of the domain

< interpFunc   ::  Func -> ([D] -> D) -- function symbols are associated to functions
  
For example, a function f of arity n will be translated to a function
which takes lists of length n of domain elements to domain elements.

Fill in the definitions for the following two functions

> interpAName      ::  AName -> Integer
> interpAName       =  undefined
> interpAFunc      ::  AFunc -> ([Integer] -> Integer)
> interpAFunc S     =  succ where succ [n] = n + 1
> interpAFunc Plus  =  undefined

Then define a more general interpreter of atomic terms

> interpAtomic ::  (name -> domain) -> (func -> [domain] -> domain) -> AtomicTerm name func -> domain
> interpAtomic  =  undefined

such that

< interpAtomic interpAName interpAFunc = interpAtomicTerm

2. Similarly, we implemented the interpretation of terms as

< evalA env (N Zero)           =  0
< evalA env (V x)              =  lookUp x env
< evalA env (F Succ [t])       =  evalA env t + 1
< evalA env (F Plus [t0, t1])  =  evalA env t0 + evalA env t1

Implement the general function

> eval :: Env var domain -> (name -> domain) -> (func -> [domain] -> domain) -> Term name func var -> domain
> eval  = undefined

such that

< eval env evalAName evalAFunc = evalA env

where evalAName and evalAFunc are the translations of interpAName and interpAFunc to the type Term.

3. Complete the definition of the WFF type and of the functions termVars and freeVars; implement isSentence.

4. If we interpret Eq and LE as == and <= on the Integer type, then a
given arithmetical sentence expresses a statement that is true of
Haskell integers.  Checking whether this is the case is difficult for
sentences containing quantifiers, and too simple for sentences with no
quantifiers (and therefore no variables).  Implement a function for
the intermediate case of WFFs with free variables, but no quantifiers:

> check :: Env AVar Integer -> WFF AName AVar APred AFunc -> Bool
> check  = undefined

Can you generalise check in the same way as eval and interpAtomic?


