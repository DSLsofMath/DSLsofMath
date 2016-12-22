NAME=DSLsofMathNotes

default: $(NAME).pdf

$(NAME).pdf:  $(NAME).tex
	latexmk -pdf $(NAME).tex

$(NAME).tex: $(NAME).lhs dslm.format ../comp/ref.bib
	lhs2TeX --agda $(NAME).lhs > $(NAME).tex

clean:
	- rm *.ptb *.pag *.aux *.bbl *.blg *.fdb_latexmk *.log $(NAME).tex $(NAME).pdf *.idx *.ilg *.ind *.toc *~ *.vrb *.snm *.fls *.nav *.out
