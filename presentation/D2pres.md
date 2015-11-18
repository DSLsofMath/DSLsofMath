# Matematikens domänspecifika språk [DAT325](https://student.portal.chalmers.se/sv/chalmersstudier/programinformation/Sidor/SokProgramutbudet.aspx?course_id=24179&parsergrp=2) / [DIT982](http://kursplaner.gu.se/english/DIT982.pdf)

(Pres. för D2, To 2015-11-19 av Patrik Jansson.)

* Patrik Jansson: forskare och lärare i FP-gruppen, avd. för programvaruteknik.
    * examinator för kursen DSLsofMath
    * XPA för D (2011-13), nu "inspektor" för D-sektionen
* kursansvarig: Cezar Ionescu, docent i "Data Science" i Oxford.
    * jag tar troligen över från 2017

* Vi tror på er förmåga att själv kombinera kurser till en bra helhet
* LP3 i D2: ett första smakprov på programmets valfrihet
    * Budskap: *Välj inte bara en av DSLsofMath och ConcProg - välj båda!*

# Kursidé för "Matematikens domänspecifika språk"

Kursens grundidé är att presentera klassiska matematiska ämnen från
ett datavetenskapligt perspektiv:

* att tydligt beskriva de begrepp som introduceras,
* vara uppmärksam på syntax och typer,
* att bygga domänspecifika språk for vissa matematiska områden: linjär algebra, lite kategoriteori, polynom och potensserier, komplexa tal och transformer
* att implementera (Haskell-)program för dessa områden (och därigenom nå en djupare förståelse)

Kursen är tänkt att ge en fördjupad matematisk förståelse för
datastudenter och en fördjupad datavetenskaplig förståelse för
matematikstudenter.

# Historisk bakgrund och motivation för DSLsofMath

Det har under många år funnits en del problem med resultaten på
kurserna "Transformer" samt "Regler" i D3. Ett av målen med den här
nya kursen är att se till att förbereda er i D2 så att ni kan ta er an
hösten i trean med ett gott självförtroende på mattesidan.

Ett annat återkommande önskemål från D-studenter är en
"mellan-avancerad FP-kurs". DSLsofMath kan ses som ett naturligt steg
på vägen från grundkursen i FP till den avancerade FP-kursen (AFP).

# Aktiva studier

* föreläsningar växlat med övningar + grupparbete.
* Aktiva studier: det räcker inte att bara "läsa (eller lyssna) och hålla med"
* *D är en bra bakgrund*: programmering och datavetenskap ~= mat. problemlösning & logik
* plocka isär & reda ut begrepp -> källkod (funktioner och typer)
* datorn (kompilatorn) ger direkt återkoppling när något inte stämmer.

# Funktionell programmering (FP) och typer

* Typer (Int, String, [Int], a -> a -> a, [a -> a], ...)
* Rena funktioner som bas: från indata till utdata
* Historik: matematiska bevisverktyg och algoritmer
* Nutid: Konkurrensfördel, FP-experter eftertraktas i näringsliv och forskning
* Vackert möte mellan matematik och maskin.

Undervisningsspråket är engelska och implementationsspråket är Haskell.

# Domänspecifika språk (DSL)

Exempel:

* datum:
    * Syntax: "2015-11-18", "tredje onsdagen i oktober", "nästa lördag"
    * Semantik: Date, eller kanske Date -> Date
* excel-formler:
    * Syntax: "SUM(A1:A9)", "RIGHT(LEFT(C7,4),2)", ...
    * Semantik: [ [ Cell ] ] -> Value
* integraler: [Behöver ritas!]

# Matematikens domänspecifika språk (DSLsofMath)

* linjär algebra:
    * vektorer, matriser, egenvärden, ...
* lite kategoriteori: funktioner och typer kan generaliseras till många olika delar av matematiken
* polynom och potensserier:
    * Syntax/representation: ändliga och oändliga listor av koefficienter
    * Semantik: funktioner (flera semantiker att välja på för samma syntax)
    * [Power series - power serious!](http://www.cs.dartmouth.edu/~doug/powser.html)
* komplexa tal (och transformer)
    * Algebraiskt: En "imaginär enhetet" i och några lagar (i*i == -1, etc)
    * Geometrisk: rotation och skalning av punkter i planet
    * Två helt olika språk med samma semantik: de komplexa talen.

# Sammanfattning

Jag rekommenderar er att välja *både* DSLsofMath *och* ConcProg under
er utbildning, men att börja med DSLsofMath eftersom den ger er bättre
chanser att segla igenom D3 utan att gå på grund.

Välkomna i januari önskar lärarlaget
  Patrik, Cezar, Irene, Victor och Adam

----------------

# Extra: Domänspecifika språk (DSL) i fo.&utv.

Exempel på lokal forsk. & utv. som kan beskrivas i termer av DSL:

* Lava för att beskriva hårdvarukretsar
* QuickCheck för automatisk testning
* [Feldspar](http://feldspar.github.io/) för digital signalbehandling
* GF för grammatiker och språkteknologi
* ...

Det finns också många företag som använder sig av, eller utvecklar
egna, domänspecifika språk.
