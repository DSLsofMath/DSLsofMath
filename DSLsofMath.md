# Course description

Software engineering involves modelling very different domains (e.g., business
processes, typesetting, natural language, etc.) as software systems.  The main
idea of this course is that this kind of modelling is also important when
tackling classical mathematics.  In particular, it is useful to introduce
abstract datatypes to represent mathematical objects, to specify the
mathematical operations performed on these objects, to pay attention to the
ambiguities of mathematical notation and understand when they express
overloading, overriding, or other forms of generic programming.  We shall
emphasise the dividing line between syntax (what mathematical expressions look
like) and semantics (what they mean).  This emphasis leads us to naturally
organise the software abstractions we develop in the form of domain-specific
languages, and we will see how each mathematical theory gives rise to one or
more such languages, and appreciate that many important theorems establish
"translations" between them.

Mathematical objects are immutable, and, as such, functional programming
languages are a very good fit for describing them.  We shall use Haskell as our
main vehicle, but only at a basic level, and we shall introduce the elements of
the language as they are needed.  The mathematical topic treated have been
chosen either because we expect all students to be familiar with them (for
example, limits of sequences, continuous functions, derivatives) or because they
can be useful in many applications (e.g., analytic functions, Laplace
transforms).
