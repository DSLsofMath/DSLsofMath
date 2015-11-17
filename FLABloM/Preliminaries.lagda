% -*- ispell-local-dictionary: "british"; -*-
%if imports
\begin{code}
open import Data.Product renaming (_,_ to _,,_) -- just to avoid clash with other commas
open import Algebra.Structures           using (IsSemigroup)
open import Algebra.FunctionProperties   using (LeftIdentity; Commutative)
\end{code}
%endif

This section introduces the elements of Agda necessary to
understand the upcoming sections.
%
We present both the language itself, and some definitions which are
part of the standard library.
%
Throughout the paper, we will use a literate-programming style.
%
The body of this section is a single module |Preliminaries| which
contains ``specification building blocks'' --- a number of definition
for later use.
%
\begin{code}
module Preliminaries where
\end{code}
%
In Agda code, scoping is indicated by indentation.
%
Indentation is rather hard to visualise in a paper which spans over
several pages, so we will instead give scoping hints in the text when
necessary.
%
The scope of the current module extends to the end of the section.

\subsubsection*{Propositions-as-types}
The philosophy behind Agda is that each proposition is expressed as a
type.
%
That is, proving that a proposition |P| holds means finding an
inhabitant (an element) of |P| (read as a type).
%
With this in mind, we define the type of relations over an underlying
type |A| as functions mapping two elements of type |A| to a another
type (a |Set| in Agda parlance).
\begin{code}
Rel : Set -> Set₁
Rel A  =  A -> A -> Set
\end{code}
%
Hence, an element of type |Rel A| is a binary relation on |A|.
%
For example |Rel A| will be inhabited by equivalence relations and orderings.
%
A consequence of the above definition is that our relations are
constructive:
%
to show that a pair |(x,y)| is in a relation |R| we must provide a
witness of type |R x y|.
%**PJ: perhaps provide an example (equality, or lifting Bool into Set, etc.)
(In passing, the type of relations is a so-called big set (|Set₁|),
because it contains |Set|s itself.
%
This distinction is necessary for the consistency of the logical
system.)

%\subsubsection*{Pairs exist}
As another example, we can define the existential quantifier
connective as follows:
%
%format _comma_ = "\un,\!\un"
%
\begin{spec}
record ∃ {A : Set} (P : A → Set) : Set₁ where
  constructor _comma_
  field
    proj₁ : A
    proj₂ : P proj₁
\end{spec}
%
That is, the existence of an element satisfying |P| can be written
|∃ (\x -> P x)| (or, equivalently, |∃ P|), and proving this
proposition means to find a witness |x : A| and an inhabitant |p : P
x|.
%
The |constructor| keyword here introduces the name |_comma_| as the
two-argument constructor of the record type.
%
Infix operators are declared using underscores on both sides of the
name so an infix comma (|,|) can now be used as in the following
example: |(x , p) : ∃ P|.
%
Note that only the last argument (|P|) of the |∃| symbol is written, the
first (|A|) is left for Agda to infer: we say that it is \emph{implicit}.
%
Implicit parameters are marked by placing them in braces at the
declaration site.
%
An implicit argument can be supplied explicitly at a call site, if the
programmer encloses it with braces.
%
This syntax can be useful if Agda fails to infer the argument in a
certain context.

\subsubsection*{Entire relations}
A binary relation from a set |A| to a set |B| is called \emph{entire} if
every element of |A| is related to at least one element of |B|, and we
can encode this definition as follows.
%
\begin{code}
Entire : {A B : Set} -> (_R_ : A -> B -> Set) -> Set
Entire _R_ = ∀ a -> ∃ \ b -> a R b
\end{code}
%
Here, again, the use of underscores around |R| makes it an infix
operator (in its scope).
%
Fixity is just a presentation issue, so an equivalent, but shorter,
definition is |Entire R = ∀ a -> ∃ (R a)| where |R| is prefix and |R
a| is a partial application.
%
A consequence of proving that a relation |R| is entire in our constructive
setting is that we get a function contained in |R|.
%
We can extract the function using the first field of the |∃| record.
%
\begin{code}
fun : {A B : Set} -> {_R_ : A -> B -> Set} -> Entire _R_ -> A -> B
fun ent a = proj₁ (ent a)
\end{code}
%
The proof that the function is contained in |R| can be obtained from
the second field of |∃|:
%
\begin{code}
correct :  {A B : Set} -> {_R_ : A -> B -> Set} -> (ent : Entire _R_) ->
           let f = fun ent in  ∀ {a : A} -> a R (f a)
correct ent {a} = proj₂ (ent a)
\end{code}
%           ∀ {a : A} -> a R (fun ent a)
The above pattern generalises to relations of any number of arguments.
%
In this paper we need the following version:
%
\begin{code}
Entire3 : {A B C D : Set} -> (R : A -> B -> C -> D -> Set) -> Set
Entire3 R = ∀ x y z -> ∃ (R x y z)
\end{code}
with corresponding definitions of |fun3| and |correct3|.
%if False
\begin{code}
fun3 : {A B C D : Set} -> {R : A -> B -> C -> D -> Set} -> Entire3 R -> A -> B -> C -> D
fun3 ent a b c = proj₁ (ent a b c)

correct3 :  {A B C D : Set} -> {R : A -> B -> C -> D -> Set} -> (e : Entire3 R) ->
           ∀ {a : A} {b : B} {c : C} -> R a b c (fun3 e a b c)
correct3 ent {a} {b} {c} = proj₂ (ent a b c)
\end{code}
%endif

%%module V2 where
%%  record Entire {A B : Set} (_R_ : A -> B -> Set) : Set where
%%    constructor Ent
%%    field
%%      entire : ∀ a -> ∃ \ b -> a R b
%%
%%    fun   : A -> B
%%    fun a = proj₁ (entire a)
%%
%%    correct : {a : A} -> a R (fun a)
%%    correct {a} = proj₂ (entire a)
%%  open Entire public
%%
%%module V3 where
%%  record Entire {A B : Set} (_R_ : A -> B -> Set) : Set where
%%    field
%%      fun     : A -> B
%%      correct : {a : A} -> a R (fun a)

\subsubsection*{Uniqueness} An element of |UniqueSolution _≃_ P| is a proof that the predicate |P|
has (at most) a \emph{unique} solution relative to some underlying
relation |_≃_|.
%
%PJ: checked. We always use it applied to two arguments. \todo{Check if the ``at most'' part causes trouble anywhere.}
%
\begin{code}
UniqueSolution : {A : Set} -> Rel A -> (A -> Set) -> Set
UniqueSolution _≃_ P = ∀ {x y} -> P x -> P y  ->  x ≃ y
\end{code}
%
A proof |usP : UniqueSolution _≃_ P| is thus a function which given
two hidden arguments of type |A| and two proofs that they satisfy |P|
returns a proof that they are related by |_≃_|.

\subsubsection*{Least solutions} In optimisation problems, one often wants to find the least solution with
respect to some order |_≤_|.
%
We use |LowerBound _≤_ P| for the predicate that holds for an |a : A|
iff |a| is smaller than all elements satisfying |P|.
%
If a lower bound is in the set (satisfies the predicate |P|) it is
called \emph{least}.
% \url{http://en.wikipedia.org/wiki/Partially_ordered_set} }
\begin{code}
LowerBound : {A : Set} -> Rel A -> (A -> Set) -> (A -> Set)
LowerBound _≤_ P  a = ∀ z -> (P z -> a ≤ z)

Least   : {A : Set} -> Rel A -> (A -> Set) -> (A -> Set)
Least   _≤_ P  a =  P a  ×  LowerBound _≤_ P a
\end{code}
%**Note that being least is stronger than being minimal
Note that a proof |alP : Least _≤_ P a| is a pair of a proof that |a|
is in |P| and a function |albP : LowerBound _≤_ P a|.
%
And, in turn, |albP| is a function that takes any |z : A| (with a proof that
|z| is in |P|) to a proof that |a ≤ z|.

%\todo{Perhaps introduce parametrised modules - done via the record.}

\subsubsection*{Records and modules}
The upcoming proof makes extensive use of
\emph{records}, which we review now in detail.
Agda record types contain fields and helper definitions.
%
Fields refer to data which is stored in the record, while helper
definitions provide values which can be computed from such
data.
%
Because Agda treats proofs (of propositions) as data, one can require
the fields to satisfy some laws, just by adding (proofs of) those laws as fields.
%
Our first record type example, |∃ P|, has two fields; one element
|proj₁| and a proof that it satisfies the property |P|.
%
As a more complex record type example we use the following
(simplified) version of |IsCommutativeMonoid| from
|Algebra.Structures| in the standard library.
%
The record type is parametrised over a carrier set, a relation, a
binary operation and its identity element:
%
\savecolumns
%
%if False
To avoid exporting this record type (it should be imported from the stdlib).
\begin{code}
module Hidden where
\end{code}
%endif
%PJ: added underscores around ≈ to help Agda-newbies to parse the expressions. (To avoid the reading "IsSemigroup equivales _∙_".)
\begin{code}
  record IsCommutativeMonoid  {A : Set} (_≈_ : Rel A)
                              (_∙_ : A -> A -> A) (ε : A) : Set₁ where
    field
      isSemigroup  : IsSemigroup   _≈_     _∙_
      identityˡ    : LeftIdentity  _≈_  ε  _∙_
      comm         : Commutative   _≈_     _∙_
\end{code}
%
The fields capture the requirements of being a commutative monoid, in
terms of three other properties.
%
Here, the first field (|isSemigroup|) is also of record type; it is in
fact common in Agda to define deeply nested record structures.

In Agda every record type also doubles as a module parametrised over
a value of that type.
%
%See log.txt:IsSemiGroup
For example, within the scope of the above record, given a value |isNP
: IsSemigroup {ℕ} _≈_ _+_|, the phrase |IsSemigroup isNP| is
meaningful in a context where Agda expects a module.
%
It denotes a module containing a declaration for each field and helper
definition of the |IsSemigroup| record type.
%
Consequently, within the scope of the above record, one can access the
(nested) fields of |isSemiGroup| in the module |IsSemigroup
isSemiGroup|.
%
In fact, it is very common for a record type to re-export all the
definitions of inner records. This can be done with the following
declaration (still inside the |record|):
%
\restorecolumns
%
\begin{spec}
    open IsSemigroup isSemigroup public
\end{spec}
|open| means that the new names are brought into scope for later
definitions inside the record type (module) and
%
|public| means the new names are also exported (publicly visible).
%
This means that the user can ignore the nesting when fetching nested fields (but not when constructing them).
%

\subsubsection*{Equality proofs}
We finish this section with an example of a helper definition, which also serves as an introduction to \emph{equality-proof notation}.
A helper definition of |IsCommutativeMonoid| is the right identity
proof |identityʳ|, which is derivable from commutativity and left
identity.
%
We can define it as follows (still inside the |record IsCommutativeMonoid|):
%
% RightIdentity ≈ ε _∙_
\restorecolumns
\begin{spec}
    identityʳ : ∀ x ->  (x ∙ ε) ≈ x
    identityʳ x =
      begin
        x ∙ ε
      ≈⟨ comm x ε ⟩
        ε ∙ x
      ≈⟨ identityˡ x ⟩
        x
      ∎
\end{spec}
%
The above is merely the composition by transitivity of |(comm x ε)| and
|(identityˡ x)|, but by using special purpose operators the user can keep
track of the intermediate steps in the proof in a style close to pen-and-paper proofs.
%


% Perhaps cite Agda instance arguments - not for the instances but for a discussion of records and modules? http://dl.acm.org/citation.cfm?id=2034796
