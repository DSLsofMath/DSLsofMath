NAME0=DSLsofMath_Wisla23
NAME1=LagrangeEqs
NAME2=LimFun_slides


default: $(NAME0).pdf $(NAME1).pdf $(NAME2).pdf

$(NAME0).pdf: $(NAME0).tex
	latexmk -pdf $(NAME0).tex

$(NAME0).tex: $(NAME0).lhs
	lhs2TeX --poly $(NAME0).lhs > $(NAME0).tex

$(NAME1).pdf: $(NAME1).tex
	latexmk -pdf $(NAME1).tex

$(NAME1).tex: $(NAME1).lhs
	lhs2TeX --poly $(NAME1).lhs > $(NAME1).tex

$(NAME2).pdf: $(NAME2).tex
	latexmk -pdf $(NAME2).tex

$(NAME2).tex: $(NAME2).lhs
	lhs2TeX --poly $(NAME2).lhs > $(NAME2).tex

clean:
	- rm *.ptb *.pag *.aux *.bbl *.blg *.fdb_latexmk *.log \
             $(NAME0).tex $(NAME1).tex $(NAME2).tex \
             $(NAME0).pdf $(NAME1).pdf $(NAME2).pdf \
             *.idx *.ilg *.ind *.toc *~ \
             *.vrb *.snm *.fls *.nav *.out
