\documentclass{beamer}
\usepackage[utf8x]{inputenc}
%include polycode.fmt

%format REAL                    =  "\mathbb{R}"
% %format deriv = D
% %format integ = I
%format == = "\doubleequals"
% the `doubleequals' macro is due to Jeremy Gibbons
\def\doubleequals{\mathrel{\unitlength 0.01em
  \begin{picture}(78,40)
    \put(7,34){\line(1,0){25}} \put(45,34){\line(1,0){25}}
    \put(7,14){\line(1,0){25}} \put(45,14){\line(1,0){25}}
  \end{picture}}}
\begin{document}

\begin{frame}{Streams as an abstract datatype}

The API for streams:

> head  :  X  ->  A
> tail  :  X  ->  X
> cons  :  A  ->  X  ->  X

> law1 s  = {-"\qquad"-}  s == cons (head s) (tail s)

% > law2 a s  =   s == tail (cons a s)
% > law3 a s  =   a == head (cons a s)

\small Credits: [Calculus in coinductive form, Pavlović \& Escardó, 1998]
\end{frame}

\begin{frame}{An unusual stream}

Consider |X = REAL -> REAL|, and |A = REAL|.

> head  f    = f 0           -- value of |f| at |0|
> tail  f    = deriv f       -- derivative of |f|
> cons a f   = a + integ f   -- start at |a|, integrate |f| from |0|

Then the law becomes

< law1c f =
<   f  ==  cons (head f) (tail f)
<      ==  (head f) + integ (tail f)
<      ==  f 0  +  integ (deriv f)

\only<2>{
  or, in traditional notation:

\[
  f(x) = f(0) + \int_0^x f'(t) \text{d}t
\]

The fundamental law of calculus!}

Credits: [Calculus in coinductive form, Pavlović \& Escardó, 1998]
\end{frame}

\end{document}


< law2c a f  =
<   f  ==  tail (cons a f)
<      ==  deriv (a + integ f)
<      ==  deriv (integ f)


< law3c a f  =
<   a  ==  head (cons a f)
<   a  ==  head (a + integ f)
<   a  ==  a + (integ f) 0
<   0  ==  integ f 0
