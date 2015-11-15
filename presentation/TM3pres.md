# Matematikens domänspecifika språk [DAT325](https://student.portal.chalmers.se/sv/chalmersstudier/programinformation/Sidor/SokProgramutbudet.aspx?course_id=24179&parsergrp=2) / [DIT982](http://kursplaner.gu.se/english/DIT982.pdf)
(Pres. for TM3, Må 2015-11-16 av Patrik Jansson.)

* Patrik Jansson, forskare & lärare i funktionell prog.-gruppen
    * avd. för programvaruteknik, inst. för Data- och informationsteknik.
* Kursansvarig: Cezar Ionescu, docent i "Data Science" i Oxford.

Presentera klassiska matematiska ämnen från ett datavetenskapligt perspektiv:

* att tydligt beskriva de begrepp som introduceras,
* vara uppmärksam på syntax och typer,
* att bygga domänspecifika språk for vissa matematiska områden: linjär algebra, lite kategoriteori, polynom och potensserier, komplexa tal
* att implementera (Haskell-)program för dessa områden (och därigenom nå en djupare förståelse)

Kursen är tänkt att ge en fördjupad matematisk förståelse för
datastudenter och en fördjupad datavetenskaplig förståelse för
matematikstudenter.

# Aktiva studier

* föreläsningar växlat med övningar + grupparbete.
* Aktiva studier: det räcker inte att bara "läsa (eller lyssna) och hålla med"
* TM är en bra bakgrund: mat. problemlösning & logik ~= programmering och datavetenskap
* plocka isär & reda ut begrepp -> källkod (funktioner och *typer*)
* datorn (kompilatorn) ger direkt återkoppling när något inte stämmer.


# Funktionell programmering (FP) och typer

* Typer (Int, String, [Int], Int -> Int, [Int -> Int], ...)
* Rena funktioner som bas: från indata till utdata
* Historik: matematiska bevisverktyg och algoritmer
* Nutid: Konkurrensfördel, FP-experter eftertraktas i näringsliv och forskning
* Vackert möte mellan matematik och maskin.

```haskell
sq :: Double -> Double
sq(x) = x^2

twice :: (Double -> Double) -> (Double -> Double)
twice f = \x -> f(f(x))

quad :: Double -> Double
quad = twice sq

test :: Double
test = quad 3

list :: [Double -> Double]
list = [(1+), (2*), sq, quad]
```


# Förkunskaper

Studenten ska ha klarat (+TM-exempel):

* en kurs i diskret matematik
    * TM1: en kombination av Mat.prog., Lin.alg&geometri, Mat.orientering, Sannolikhet, ...
* två andra kurser i matematik
    * TM1: exempelvis Linjär algebra, Analys I och II
* två kurser i datateknik
    * TM1: Programmeringsteknik + TM2: Datastrukturer
* ytterligare tre kurser (22.5hp) inom matematik, data eller IT
    * TM1: LinAlg&NumAn, Flervariabel + TM2: Komplex, ...

Rekommenderat (men ej krav): Funktionell programmering

# Andra kurser

Relation till de övriga kurserna som presenteras + kandidatarbete:

| Kurs                               | Relation    |
| ---------------------------------- | ----------- |
| Kommunikationssystem               | DSL: [Feldspar](http://feldspar.github.io/) |
| Partiella differentialekvationer   | DSL: [FEniCS](http://fenicsproject.org/), samarbete med Anders Logg |
| Algebra                            | [Algebra of Programming](http://wiki.portal.chalmers.se/agda/pmwiki.php?n=Libraries.AOPA) |
| Databaser                          | SQL, relationsalgebra |
| Kandidatarbete om DSLsofMath       | [Jag handleder!](https://github.com/DSLsofMath/DSLsofMath/blob/master/BScProj/DSLsofMath_andra_kurser.md) |

# Matematikens domänspecifika språk


* Matematik möter maskin
* Funktionell programmering ger nya verktyg
* Mötet mellan matematiken och datavetenskapen
    * det är där det händer riktigt spännande saker ...

Välkomna!
/Patrik
