%TODO (DaHe): I have only included the parts of the exam questions that I think
% have to do with chapter 1, with the intent that the remaining parts will be
% covered in later chapters. Should we restate the whole question when the other
% parts are added, or just refer back to E1? ("In exercise 1.3, we looked at
% implementing the datatype SR v for the laguage of semiring expressions. We
% will now revisit this blablabla" Answer: refer back.)

%TODO: Add an exercise to implement some property as a boolean functions (for example some from the semiring properties.

%\newpage
\begin{exercise} \label{exc:e1_semiring}
  \fromExam{2017-08-22}

  A semiring is a set |R| equipped with two binary operations |+| and
  |⋅|, called addition and multiplication, such that:
  \begin{itemize}

  \item |(R, +, 0)| is a commutative monoid with identity element |0|:

    \begin{spec}
      (a + b) + c = a + (b + c)
      0 + a = a + 0 = a
      a + b = b + a
    \end{spec}

  \item |(R, ⋅, 1)| is a monoid with identity element |1|:

    \begin{spec}
      (a⋅b)⋅c = a⋅(b⋅c)
      1⋅a = a⋅1 = a
    \end{spec}

  \item Multiplication left and right distributes over |(R, +, 0)|:

    \begin{spec}
      a⋅(b + c) = (a⋅b) + (a⋅c)
      (a + b)⋅c = (a⋅c) + (b⋅c)
      0⋅a = a⋅0 = 0
    \end{spec}

  \end{itemize}

  \begin{enumerate}

  \item Define a datatype |SR v| for the language of semiring
    expressions (with variables of type |v|).
%
    These are expressions formed from applying the semiring
    operations to the appropriate number of arguments, e.g., all the
    left hand sides and right hand sides of the above equations.

  \item \lnOnly{(Was not part of the exam)} Implement the expressions from the laws.

  \item Give a type signature for, and define, a general evaluator for
    |SR v| expressions on the basis of an assignment function.
%
    An ``assignment function'' is a mapping from variable names to values.
  \end{enumerate}
\end{exercise}

\begin{exercise} \label{exc:e1_lattice}
  \fromExam{2016-03-15}

  A \emph{lattice} is a set |L| together with two operations |∨| and
  |∧| (usually pronounced ``sup'' and ``inf'') such that
  \begin{itemize}

  \item |∨| and |∧| are associative:

    \begin{spec}
      Forall (x, y, z ∈ L)      ((x ∨ y) ∨ z = x ∨ (y ∨ z))
      Forall (x, y, z ∈ L)      ((x ∧ y) ∧ z = x ∧ (y ∧ z))
    \end{spec}

  \item |∨| and |∧| are commutative:

    \begin{spec}
      Forall (x, y ∈ L)         (x ∨ y = y ∨ x)
      Forall (x, y ∈ L)         (x ∧ y = y ∧ x)
    \end{spec}

  \item |∨| and |∧| satisfy the \emph{absorption laws}:

    \begin{spec}
      Forall (x, y ∈ L)         (x ∨ (x ∧ y) = x)
      Forall (x, y ∈ L)         (x ∧ (x ∨ y) = x)
    \end{spec}
  \end{itemize}

  \begin{enumerate}
  \item Define a datatype for the language of lattice expressions.

  \item Define a general evaluator for |Lattice| expressions on the
    basis of an assignment function.
  \end{enumerate}
\end{exercise}

\begin{exercise} \label{exc:e1_abMon}
  \fromExam{2016-08-23}

  An \emph{abelian monoid} is a set |M| together with a constant
  (nullary operation) |0 ∈ M| and a binary operation |⊕ : M → M → M|
  such that:
  \begin{itemize}

  \item |0| is a unit of |⊕|

    \begin{spec}
      Forall (x ∈ M)           (0 ⊕ x = x ⊕ 0 = x)
    \end{spec}

  \item |⊕| is associative

    \begin{spec}
      Forall (x, y, z ∈ M)     (x ⊕ (y ⊕ z) = (x ⊕ y) ⊕ z)
    \end{spec}

  \item |⊕| is commutative

    \begin{spec}
      Forall (x, y ∈ M)        (x ⊕ y = y ⊕ x)
    \end{spec}

  \end{itemize}

  \begin{enumerate}

  \item Define a datatype |AbMonoidExp| for the language of abelian
    monoid expressions.
    %
    (These are expressions formed from applying the monoid operations
    to the appropriate number of arguments, e.g., all the left hand
    sides and right hand sides of the above equations.)

  \item Define a general evaluator for |AbMonoidExp| expressions on
    the basis of an assignment function.

  \end{enumerate}
\end{exercise}
