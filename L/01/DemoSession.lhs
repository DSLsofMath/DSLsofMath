% Notes for first week demo session by Rachel
\documentclass{article}

\newcommand{\week}{1}

\usepackage{amsmath}
\usepackage{parskip}
\usepackage{tikz}
\usetikzlibrary{positioning}
\usepackage{listings}
\lstnewenvironment{code}{}{}
\lstnewenvironment{code_tex}{}{} % no lhs

\newcommand{\ex}[1]{\section*{Exercise #1}}
\newcommand{\subex}[1]{\subsection*{Part #1}}

\tikzset{level/.style={sibling distance={2.5cm/#1}}}

\begin{document}

\ex{1.1}

Recall the datatype from the exercise

\begin{code_tex}
data Exp = Con Integer
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         deriving (Eq, Show)
\end{code_tex}

\subex{1}
It's benificial to start out with thinking about what these expressions really mean.

One way of doing this is by adding more parethesis to the expressions.
\begin{align*}
  a_1 &= 2 + 2 \\
  a_2 &= a_1 + (7 * 9) \\
  a_3 &= (8 * (2 + 11)) - ((3 + 7) * (a_1 + a_2))
\end{align*}

Another approach is to draw the expressions as trees, where a node is an operation, and its children are its operands.
\begin{center}
\begin{tikzpicture}
  \node(atree){+}
    child {node {2}}
    child {node {2}};
  \node[above of=atree]{$a_1$};

  \node(btree)[right=3cm of atree]{+}
    child {node {$a_1$}}
    child {node {$*$}
      child {node {7}}
      child {node {9}}
    };
  \node[above of=btree]{$a_2$};

  \node(ctree)[right=4cm of btree]{-}
    child {node {*}
      child {node {8}}
      child {node {+}
        child {node {2}}
        child {node {11}}
    }}
    child {node {$*$}
      child {node {+}
        child {node {3}}
        child {node {7}
      }}
      child {node {+}
        child {node {$a_1$}}
        child {node {$a_2$}
      }}
    };
  \node[above of=ctree]{$a_3$};
\end{tikzpicture}
\end{center}

This is nice cause it lets us easily see what the outermost operation is at each point, which is a bit harder in infix expressions. Generally it can be benifical to think about expressions as trees, rather than strings of symbols/text.

\newpage
We can now translate these into Haskell by replacing the operands with their corresponding constructor by taking the left tree as the first argument and the right as the second. Numbers are translated using the \texttt{Con} constructor.
\begin{code}
a1, a2, a3 :: Exp
a1 = Plus (Con 2) (Con 2)
a2 = Plus a1 (Times (Con 7) (Con 9))
a3 = Minus (Times (Con 8) (Plus (Con 2) (Con 11))) 
           (Times (Plus (Con 3) (Con 7)) (Plus a1 a2))
\end{code}

\subex{2}

For this part we will pattern watch on the \texttt{Exp} type and use a lot of recursion. Each operation is evaluated by first evaluating its operands, which are also of the type \texttt{Exp}, yielding the integers corresponding to their value, and then applying the proper integer operator to these values.
\begin{code_tex}
eval :: Exp -> Integer
eval (Con i) = i
eval (Plus e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Times e1 e2) = eval e1 * eval e2
\end{code_tex}

This is a very general pattern which you will see a lot in the course. You have some data structure, where each constructor has some corresponding function in another type. Then evaluation can be done by evaluating all the constructors arguments and applying the corresponding function to them. In this case \texttt{Plus} corresponds to $+$, \texttt{Minus} to $-$, etc.

\subex{3}

First we add a constructor to the \texttt{Exp} type to allow us to represent variables.
\begin{code}
data Exp = Con Integer
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Var String
         deriving (Eq, Show)
\end{code}

We can now define $c_1$ as an \texttt{Exp}.
\begin{code}
c1 :: Exp
c1 = Times (Times (Minus (Var "x") (Con 15)) 
                  (Plus (Var "y") (Con 12)))
           (Var "z")
\end{code}
\textbf{Note:} we could have put the parenthesis on the multiplication the other way around and ended up with a different Haskell expression. This expression would be equally valid as multiplication is associative, meaning that $a * (b * c) = (a * b) * c$.

In order to evaluate $c_1$ we need to assign values to variables, we do this by defining a function \texttt{varVal}, which pattern matches on our strings. 
\begin{code}
varVal :: String -> Integer
varVal "x" = 5
varVal "y" = 8
varVal "z" = 13
\end{code}

This function then lets us extend eval to handle $c_1$, and other expressions including variables.
\begin{code}
eval :: Exp -> Integer
eval (Con i) = i
eval (Plus e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Var s) = varVal s
\end{code}

We can then evaluate $c_1$ and see that it evaluates to $-2600$.

\ex{1.2}
\textbf{TODO}

\ex{1.7}
\textbf{TODO}

\ex{1.14}
\textbf{TODO?}

\end{document}

