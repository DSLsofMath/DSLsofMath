% TODO (DaHe): Newpage before exam exercise?

% TODO (DaHe): I have only included the parts of the exam questions that I think
% have to do with chapter 1, with the intent that the remaining parts will be
% covered in later chapters. Should we restate the whole question when the other
% parts are added, or just refer back to E1? ("In exercise 1.3, we looked at
% implementing the datatype SR v for the laguage of semiring expressions. We
% will now revisit this blablabla")
%
\begin{exercise}
\textit{From exam 2017-08-22}

  A semiring is a set |R| equipped with two binary operations |+| and
  |⋅|, called addition and multiplication, such that:
\begin{itemize}

\item |(R, +, 0)| is a commutative monoid with identity element |0|:

>     (a + b) + c = a + (b + c)
>     0 + a = a + 0 = a
>     a + b = b + a

\item |(R, ⋅, 1)| is a monoid with identity element |1|:

>     (a⋅b)⋅c = a⋅(b⋅c)
>     1⋅a = a⋅1 = a

\item Multiplication left and right distributes over |(R, +, 0)|:

>     a⋅(b + c) = (a⋅b) + (a⋅c)
>     (a + b)⋅c = (a⋅c) + (b⋅c)
>     a⋅0 = 0⋅a = 0

\end{itemize}

  \begin{enumerate}

  \item Define a datatype |SR v| for the language of semiring
    expressions (with variables of type |v|) and define a |SemiRing|
    instance for it. (These are expressions formed from applying the
    semiring operations to the appropriate number of arguments, e.g.,
    all the left hand sides and right hand sides of the above
    equations.)

% TODO (DaHe): explain that "assignment function" is the same as varVal in
% previous exercise?
  \item Give a type signature for, and define, a general evaluator for
      |SR v| expressions on the basis of an assignment function.

  \end{enumerate}
\end{exercise}

\begin{exercise}
\textit{From exam 2016-03-15}

A *lattice* is a set `L` together with two operations `∨`
and `∧` (usually pronounced "sup" and "inf") such that
\begin{itemize}

\item `∨` and `∧` are associative:

> ∀ x, y, z ∈ L       (x ∨ y) ∨ z = x ∨ (y ∨ z)
> ∀ x, y, z ∈ L       (x ∧ y) ∧ z = x ∧ (y ∧ z)

\item `∨` and `∧` are commutative:

> ∀ x, y ∈ L           x ∨ y = y ∨ x
> ∀ x, y ∈ L           x ∧ y = y ∧ x

\item `∨` and `∧` satisfy the *absorption laws*:

> ∀ x, y ∈ L           x ∨ (x ∧ y) = x
> ∀ x, y ∈ L           x ∧ (x ∨ y) = x
\end{itemize}

\begin{enumerate}

\item Define a datatype for the language of lattice expressions.

\item Define a general evaluator for `Lattice` expressions on the
basis of an assignment function.
\end{enumerate}
\end{exercise}

\begin{exercise}
\textit{From exam 2016-08-23}

An *abelian monoid* is a set `M` together with a constant
(nullary operation) `0 ∈ M` and a binary operation `⊕ : M → M → M`
such that:
\begin{itemize}

\item `0` is a unit of `⊕`

> ∀ x ∈ M           x ⊕ 0 = x  and 0 ⊕ x = x

\item `⊕` is associative

> ∀ x, y, z ∈ M     x ⊕ (y ⊕ z) = (x ⊕ y) ⊕ z

\item `⊕` is commutative

> ∀ x, y ∈ M        x ⊕ y = y ⊕ x

\end{itemize}

\begin{enumerate}

\item Define a datatype `AbMonoidExp` for the language of abelian
    monoid expressions.  (These
    are expressions formed from applying the monoid operations to the
    appropriate number of arguments, e.g., all the left hand sides and
    right hand sides of the above equations.)

\item Define a general evaluator for `AbMonoidExp` expressions on the
    basis of an assignment function.

\end{enumerate}
\end{exercise}
