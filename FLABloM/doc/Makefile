NAME    := paper
LHS2TEX := lhs2TeX
LATEXMK := latexmk

default: $(NAME).pdf

$(NAME).tex: paper.format ../*.lagda

%.tex: %.lagda
	$(LHS2TEX) --agda $< > $@

%.pdf: %.tex %.bib
	$(LATEXMK) -xelatex $<

clean:
	latexmk -C $(NAME).tex
	$(RM) $(NAME).tex $(NAME).pdf
