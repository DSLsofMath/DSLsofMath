NAME=PaJa_4min_idea_FP_WM_2018

default: $(NAME).pdf

$(NAME).pdf: $(NAME).tex
	latexmk -pdf $(NAME).tex

$(NAME).tex: $(NAME).lhs
	lhs2TeX --poly $(NAME).lhs > $(NAME).tex

clean:
	-rm  *.tex *.out *.ptb *.toc *.snm *.aux *.nav *.fls *.log *.fdb_latexmk
