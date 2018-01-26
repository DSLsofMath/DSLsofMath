NAME=DSLsofMathNotes

default: $(NAME).pdf

$(NAME).pdf: $(NAME).tex
	latexmk -pdf $(NAME).tex

$(NAME).tex: $(NAME).lhs dslm.format ref.bib \
             00/Intro.lhs                     \
             01/W01.lhs 01/E1.lhs 01/E1_1.lhs 01/E1_2.lhs 01/E1_from_exams.lhs\
             02/W02.lhs 02/E2.lhs 02/FOLRat.lhs 02/AbstractFOL.lhs \
             03/W03.lhs 03/E3.lhs \
             04/W04.lhs 04/E4.lhs 04/AbstractStream.lhs 04/UnusualStream.lhs \
             05/W05.lhs 05/E5.lhs \
             06/W06.lhs 06/E6.lhs \
             07/W07.lhs 07/E7.lhs \
             08/W08.lhs 08/E8.lhs \
             End.lhs \
             01/CSem.lhs
	lhs2TeX --poly $(NAME).lhs > $(NAME).tex

build:
	stack build

sdist:
	stack sdist --test-tarball

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
             $(NAME).tex $(NAME).pdf *.idx *.ilg *.ind *.toc *~ \
             *.vrb *.snm *.fls *.nav *.out
