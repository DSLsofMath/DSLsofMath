default: DSLsofMathBook.pdf
# default: DSLsofMathNotes.pdf

nix::
	nix-shell --run "make"
book::
	nix-shell --run "make DSLsofMathBook.pdf"
	gio open DSLsofMathBook.pdf

DSLM_A6.tex: DSLsofMathNotes.tex
	cp DSLsofMathNotes.tex DSLM_A6.tex
	patch < A6_landscape.patch

%.pdf: %.tex
	latexmk -pdf $<

DSLM_A5.tex: DSLsofMathNotes.tex
	cp DSLsofMathNotes.tex DSLM_A5.tex
	patch < A5_landscape.patch

BOOKDEPS=DSLsofMath.lhs dslm.format ref.bib \
             newunicodedefs.tex \
             00/Intro.lhs                     \
             01/W01.lhs 01/CSem.lhs 01/E1.lhs 01/E1_1.lhs 01/E1_2.lhs 01/E1_from_exams.lhs \
             02/W02.lhs                      \
                02/SetTheory.lhs	     \
                02/PropositionalLogic.lhs    \
                  02/Either.lhs		     \
                02/FOL.lhs		     \
                02/Examples.lhs		     \
                02/CalculusConcepts.lhs      \
                02/E2.lhs                    \
             03/W03.lhs 03/E3.lhs \
             04/W04.lhs 04/E4.lhs 04/AbstractStream.lhs 04/UnusualStream.lhs \
             05/W05.lhs 05/E5.lhs \
             06/W06.lhs 06/E6.lhs \
             07/W07.lhs 07/E7.lhs \
             08/W08.lhs 08/E8.lhs \
             09/W09.lhs \
             End.lhs

%.tex: %.lhs $(BOOKDEPS)
	lhs2TeX --poly $< > $@

build:
	cabal v2-build

sdist:
	cabal sdist

# The target zip is deprecated in favour of sdist
zip:
	mkdir DSLsofMath_temp
	cp -LR DSLsofMath DSLsofMath_temp/DSLsofMath
	cp DSLsofMath.cabal stack.yaml DSLsofMath_temp/DSLsofMath
#	cp TestAll.lhs DSLsofMath_temp
	cd DSLsofMath_temp && zip -r ../DSLsofMath.zip DSLsofMath
	rm -r DSLsofMath_temp

clean:
	- rm *.ptb *.pag *.aux *.bbl *.blg *.fdb_latexmk *.log \
             DSLsofMath*.tex *.pdf *.idx *.ilg *.ind *.toc *~ \
             *.vrb *.snm *.fls *.nav *.out
