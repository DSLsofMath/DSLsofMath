% Options for packages loaded elsewhere
\PassOptionsToPackage{unicode}{hyperref}
\PassOptionsToPackage{hyphens}{url}
%
\documentclass[aspectratio=169]{beamer}
%include polycode.fmt
\usepackage{pgfpages}
\setbeamertemplate{caption}[numbered]
\setbeamertemplate{caption label separator}{: }
\setbeamercolor{caption name}{fg=normal text.fg}
\beamertemplatenavigationsymbolsempty
% Prevent slide breaks in the middle of a paragraph
\widowpenalties 1 10000
\raggedbottom
\setbeamertemplate{part page}{
  \centering
  \begin{beamercolorbox}[sep=16pt,center]{part title}
    \usebeamerfont{part title}\insertpart\par
  \end{beamercolorbox}
}
\setbeamertemplate{section page}{
  \centering
  \begin{beamercolorbox}[sep=12pt,center]{part title}
    \usebeamerfont{section title}\insertsection\par
  \end{beamercolorbox}
}
\setbeamertemplate{subsection page}{
  \centering
  \begin{beamercolorbox}[sep=8pt,center]{part title}
    \usebeamerfont{subsection title}\insertsubsection\par
  \end{beamercolorbox}
}
\AtBeginPart{
  \frame{\partpage}
}
\AtBeginSection{
  \ifbibliography
  \else
    \frame{\sectionpage}
  \fi
}
\AtBeginSubsection{
  \frame{\subsectionpage}
}
\usepackage{amsmath,amssymb}
\usepackage{iftex}
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage{textcomp} % provide euro and other symbols
\usepackage{lmodern}
% Use upquote if available, for straight quotes in verbatim environments
\IfFileExists{upquote.sty}{\usepackage{upquote}}{}
\usepackage{xcolor}
\usepackage{color}
\usepackage{fancyvrb}
\usepackage{longtable,booktabs,array}
\usepackage{calc} % for calculating minipage widths
\usepackage{caption}
\usepackage{graphicx}
\setlength{\emergencystretch}{3em} % prevent overfull lines
\providecommand{\tightlist}{%
  \setlength{\itemsep}{0pt}\setlength{\parskip}{0pt}}
\setcounter{secnumdepth}{-\maxdimen} % remove section numbering
\urlstyle{same}
\hypersetup{
  hidelinks,
  pdfcreator={LaTeX via pandoc}}

\author{}
\date{}

\begin{document}

\begin{frame}{Matematikens domänspecifika språk
\href{https://www.student.chalmers.se/sp/course?course_id=38990}{DAT326}
/ \href{https://kursplaner.gu.se/pdf/kurs/en/DIT983}{DIT983}}
\framesubtitle{Presentation för D2, Ti 2024-11-12 av Patrik Jansson.}
\begin{columns}
\begin{column}{0.7\textwidth}
\begin{itemize}
\tightlist
\item
  Patrik Jansson: forskare och lärare inom ämnet Funktionell Programmering

  \begin{itemize}
    \tightlist
  \item examinator och kursansvarig för kursen DSLsofMath
  \item 2011-2013: PA för D-programmet
  \item 2014-2021: ``inspektor'' för D-sektionen
  \item 2023-: Examinator för Kandidatarbetet på CSE
  \end{itemize}
\item LP3 i D2: ett första smakprov på programmets valfrihet
  \begin{itemize}
    \tightlist
  \item Vi tror på er förmåga att själva kombinera kurser till en bra
    helhet
  \item Budskap: \emph{Välj både DSLsofMath (nu i LP3) och ``Principer
      för parallell programmering'' (sedan i D3, LP1)}
  \end{itemize}
\end{itemize}
\end{column}
\begin{column}{0.3\textwidth}
\includegraphics[width=\textwidth]{../../Wisla23/DSLsofMath_book_front_cover.jpg}
\end{column}
\end{columns}
\end{frame}

\begin{frame}{Kursidé för ``Matematikens domänspecifika språk''
(DSLsofMath)}
\protect\hypertarget{kursiduxe9-fuxf6r-matematikens-domuxe4nspecifika-spruxe5k-dslsofmath}{}

\begin{columns}
\begin{column}{0.8\textwidth}
Presentera klassiska matematiska ämnen från ett datavetenskapligt
perspektiv:
\begin{itemize}
  \tightlist
\item tydligt beskriva de begrepp som introduceras,
\item vara uppmärksam på syntax, typer, och vad de betyder (semantik)
\item bygga domänspecifika språk för:
  \begin{itemize}
  \item typer, funktioner, logik, derivator, polynom, potensserier,\\
    linjär algebra, transformer
  \end{itemize}
\item implementera (Haskell-)program för dessa områden\\
  (\(\Rightarrow\) djupare förståelse)
\end{itemize}
\end{column}
\begin{column}{0.2\textwidth}
\includegraphics[width=\textwidth]{../DSL_logo/DSL_logo_500x500.png}
\includegraphics[width=\textwidth]{../../Wisla23/DSLsofMath_book_front_cover.jpg}
\end{column}
\end{columns}
\end{frame}

\begin{frame}{Historisk bakgrund och motivation för DSLsofMath}
\protect\hypertarget{historisk-bakgrund-och-motivation-fuxf6r-dslsofmath}{}
Det har under många år funnits en del problem med resultaten på kurserna
``Transformer'' samt ``Regler'' i D3. Ett av målen med den här kursen är
att se till att förbereda er i D2 så att ni kan ta er an hösten i trean
med ett gott självförtroende på mattesidan.

Ett annat återkommande önskemål från D-studenter är en
``mellan-avancerad FP-kurs''. DSLsofMath kan ses som ett naturligt steg
på vägen från grundkursen i FP till den avancerade FP-kursen (AFP).
\end{frame}

\begin{frame}{Resultatstatistik}
\protect\hypertarget{resultatstatistik}{}
\begin{itemize}
\tightlist
\item
  Resultat på DSLsofMath (DAT326, 2017-2023):

  \begin{itemize}
  \tightlist
  \item
    311 labbade varav 96\% godkända
  \item
    280 tentade varav 65\% godkända \pause
  \end{itemize}
\item
  Resultat i D3 (Transformer samt Regler) 2018
\end{itemize}

\begin{longtable}[]{@@{}lrrr@@{}}
\toprule\noalign{}
D3-kurs & Valde inte DSL & Valde DSL & Godkänd DSL \\
\midrule\noalign{}
\endhead
Transformer & 36\% & 57\% & \textbf{77\%} \\
Regler & 40\% & 45\% & \textbf{68\%} \\
\bottomrule\noalign{}
\end{longtable}

Dvs. de som tog DSL-kursen fick mycket bättre resultat hösten i D3.

\small (Totalt 145 varav `Valde inte DSL'=92, Valde=53, Godk.=34.)
\end{frame}

\begin{frame}{Aktiva studier}
\begin{itemize}
\tightlist
\item föreläsningar växlat med övningar + grupparbete.
\item Aktiva studier: det räcker inte att bara ``läsa (eller lyssna)
  och hålla med''
\item \emph{D är en bra bakgrund}: programmering och datavetenskap
  $\simeq$ matematisk problemlösning \& logik
\item plocka isär \& reda ut begrepp $\Rightarrow$ källkod
  (funktioner och \emph{typer})
\item datorn (kompilatorn) ger direkt återkoppling när något inte
  stämmer.
\end{itemize}
\end{frame}

%include Haskell.lhs

\begin{frame}[fragile]{Funktionell programmering (FP) och typer}

\begin{columns}
\begin{column}{0.55\textwidth}
\untypedcode
\end{column}
\begin{column}{0.45\textwidth}
\only<2->{\typedcode}
\end{column}
\end{columns}
\begin{itemize}
\tightlist
\item<1->
  Rena funktioner som bas: från indata till utdata
\item<2->
  Typer (|Int|, |String|, |[Int]|,
  |a -> a|, |[a -> a]|, |Poly a|, \ldots)
\item<3-> Passar bra för att modellera matematik.
% \item<3->
%   Historik: matematiska bevisverktyg och algoritmer
% \item<3->
%   Nutid: Konkurrensfördel, FP-experter eftertraktade!
% \item<3->
%   Vackert möte mellan matematik och maskin.
\end{itemize}

\end{frame}

\begin{frame}{Domänspecifika språk (DSL)}
Exempel:

\begin{itemize}
\item datum:

  \begin{itemize}
  \tightlist
  \item
    Syntax: ``2024-11-12'', ``andra tisdagen i november'', ``idag''
  \item
    Semantisk typ: |Date|, eller kanske |Date -> Date|
  \end{itemize}
\item
  excel-formler:

  \begin{itemize}
  \tightlist
  \item
    Syntax: ``SUM(A1:A9)'', ``RIGHT(LEFT(C7,4),2)'', \ldots{}
  \item
    Semantisk typ: |[[Cell]] -> Value|
  \end{itemize}
\item
  integraler:

  \includegraphics[width=5cm]{integral_scope.png}
\end{itemize}
\end{frame}

\begin{frame}{Matematikens domänspecifika språk (DSLsofMath) för andra
ämnen}
\protect\hypertarget{matematikens-domuxe4nspecifika-spruxe5k-dslsofmath-fuxf6r-andra-uxe4mnen}{}
Inspirerat av den här kursen har flera studentgrupper genomfört
kandidatarbetesprojekt under de senaste åren med följande resultat:

\begin{itemize}
\tightlist
\item
  2016: \small Programmering som undervisningsverktyg för Transformer,
  signaler och system -
  \href{https://hdl.handle.net/20.500.12380/243894}{Utvecklingen av
  läromaterialet TSS med DSL} J Jonsson, J Olsson, F Lindahl, P Ngo, C
  Rosvall
\item
  2018: Ett komplementerande läromaterial för datastudenter som lär sig
  fysik -
  \href{https://hdl.handle.net/20.500.12380/256122}{Läromaterialet Learn
  You a Physics for Great Good!} J Johansson, O Lundström, E Sjöström, B
  Werner
\item
  2020: A Computer Science Approach to Teaching Control Theory -
  Developing Learning Material Using Domain-Specific Languages S
  Hägglund, J A Fihlman, E Ohlman, F Nylander, C Josefsson, T Räjert
\item
  2022: HasLin - ett DSL för linjär algebra - Utvecklandet av ett
  matematiskt domänspecifikt språk för linjär algebra i Haskell A
  Eliasson, D Nikolaev, F Nordmark, S Sjögren, L Sundkvist
\end{itemize}

Några av er kanske också vill gå den vägen, eller ta del av deras
material.
\end{frame}

\begin{frame}{Exempel: 2018: Learn you a physics for great good
\ldots{}}
Johan Johansson, Oskar Lundström, Erik Sjöström, Björn Werner

\includegraphics[height=0.8\textheight]{LearnYouAPhysics.png}
\end{frame}

\begin{frame}{Sammanfattning}
Jag rekommenderar er att välja \emph{både} DSLsofMath \emph{och} PCP
under er utbildning, men att börja med DSLsofMath eftersom den ger er
bättre chanser att segla igenom D3 utan att gå på grund.

(De som tar DSL-kursen får mycket bättre resultat hösten i D3.)

Välkomna i januari önskar lärarlaget

Patrik, m.fl.

\vspace*{-0.8cm}\hfill
\includegraphics[height=0.4\textheight]{../DSL_logo/DSL_logo_500x500.png}
\hfill
\includegraphics[height=0.5\textheight]{../../Wisla23/DSLsofMath_book_front_cover.jpg}
\end{frame}

\begin{frame}{Exempel på tillämpning: LinAlg, matris, maskininlärning}
\includegraphics[height=0.8\textheight]{UpperTriangularChocolateTwitter.png}
\end{frame}

\begin{frame}{Extra: Domänspecifika språk (DSL) i fo.\&utv.}
\protect\hypertarget{extra-domuxe4nspecifika-spruxe5k-dsl-i-fo.utv.}{}
Exempel på lokal forsk. \& utv. som kan beskrivas i termer av DSL:

\begin{itemize}
\tightlist
\item
  Lava för att beskriva hårdvarukretsar
\item
  QuickCheck för automatisk testning
\item
  \href{http://feldspar.github.io/}{Feldspar} för digital
  signalbehandling
\item
  GF för grammatiker och språkteknologi
\item
  \ldots{}
\end{itemize}

Det finns också många företag som använder sig av, eller utvecklar egna,
domänspecifika språk.
\end{frame}

\end{document}