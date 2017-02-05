NAME=DSLsofMathNotes

default: $(NAME).pdf

$(NAME).pdf: $(NAME).tex
	latexmk -pdf $(NAME).tex

$(NAME).tex: $(NAME).lhs dslm.format ref.bib \
             01/W01.lhs 02/W02.lhs 03/W03.lhs \
             04/W04.lhs # 05/W05.lhs 06/W06.lhs \
#            07/W07.lhs 08/W08.lhs
	lhs2TeX --poly $(NAME).lhs > $(NAME).tex

build:
	stack build

clean:
	- rm *.ptb *.pag *.aux *.bbl *.blg *.fdb_latexmk *.log \
             $(NAME).tex $(NAME).pdf *.idx *.ilg *.ind *.toc *~ \
             *.vrb *.snm *.fls *.nav *.out
