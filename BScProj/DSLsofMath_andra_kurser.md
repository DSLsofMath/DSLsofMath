# Titel

Matematikens domänspecifika språk (DSLsofMath) för andra kurser

## Bakgrund:

DSLsofMath [1,2] är namnet på ett pedagogiskt projekt som leder till
en ny valfri kurs i årskurs 2-3 riktad till datavetare och matematiker
på Chalmers och GU. Kursen presenterar klassiska matematiska ämnen
från ett datavetenskapligt perspektiv: genom att specificera de
introducerade begreppen, vara uppmärksam på syntax och typer, och
slutligen genom att bygga domänspecifika språk for vissa matematiska
områden. (Exempelvis linjär algebra, Taylors formler, potensserier,
derivator.)

## Projektbeskriving:

Det här kandidatprojektet går ut på att ta fram DSLsofMath-inspirerat
kompletterande material för andra närliggande kurser som exempelvis
* "Transformer, signaler och system" samt "Reglerteknik" (som ges av S2-institutionen), eller
* "Matematisk modellering och problemlösning" samt "Ändliga automater och formella språk", eller
* andra kurser som ni känner skulle må bra av mer fokus på syntax, typer och funktioner.

Implementationsspråk är Haskell (och kanske Agda) och målet är dels
att förbättra förståelsen hos projektmedlemmarna av de kurser och
ämnen som väljs och dels att ge framtida studenter mer material att
arbeta med. Materialet som utvecklas skall finnas öppet tillgängligt
på github.

Att göra:
* Designa och implementera (ett par) DSL för det valda området
* Specificera lagar som bör gälla
* Testa de lagar som kan testas med QuickCheck
* (Ev. bevisa någon eller några lagar.)
* Rapportskrivning: i form av en "tutorial" eller föreläsningsanteckningar

## Litteraturförslag:

* [1] https://github.com/DSLsofMath/DSLsofMath
* [2] http://wiki.portal.chalmers.se/cse/pmwiki.php/FP/DSLsofMath
* [3] http://www.cse.chalmers.se/~patrikj/papers/Ionescu_Jansson_DSLsofMath_TFPIE_2015_paper_preprint.pdf

## Målgrupp:

DV, D, IT, TM

## Särskilda förkunskaper:

Funktionell programmering (Haskell) eller gott om matematik (TM-programmet eller liknande).

## Förslagslämnare:

Patrik Jansson

## Handledare:

Patrik Jansson
