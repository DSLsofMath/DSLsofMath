NAME=
TEMPLATE=../aux/template.tex
BIB=../aux/ref.bib

default: $(NAME).pdf

$(NAME).pdf: $(NAME).lhs $(TEMPLATE) $(BIB)
	pandoc -s --template $(TEMPLATE) --bibliography=$(BIB) -f markdown+lhs $(NAME).lhs -t latex -o $(NAME).tex && latexmk -pdf $(NAME).tex

$(NAME).html: $(NAME).lhs $(BIB)
	pandoc -s --bibliography=$(BIB) -f markdown+lhs $(NAME).lhs -t html -o $(NAME).html 
clean:
	- rm *.aux *.bbl *.blg *.fdb_latexmk *.log *.pdf *.idx *.ilg *.ind *.toc *~ *.vrb *.snm *.fls *.nav *.out *.docx $(NAME).tex $(NAME).html
