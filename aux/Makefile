# Copy this makefile to the directory where you want to build all .lhs-files,
# change to that directory, and then either run:
#   - "make" to generate pdfs, or
#   - "make html" to generate html-files.
# without the quotation marks.

TEMPLATE=../aux/template.tex
BIB=../aux/ref.bib
OUTPUT=$(basename $(wildcard *.lhs))

default: pdf

pdf: $(addsuffix .pdf, $(OUTPUT))

html: $(addsuffix .html, $(OUTPUT))

%.pdf: %.tex
	latexmk -pdf $<

%.html: %.lhs $(BIB)
	pandoc -s --bibliography=$(BIB) --mathml -f markdown+lhs $< -t html -o $@

%.tex : %.lhs $(TEMPLATE) $(BIB)
	pandoc -s --template $(TEMPLATE) --bibliography=$(BIB) -f markdown+lhs $< -t latex -o $@

clean:
	- rm -f *.aux *.bbl *.blg *.fdb_latexmk *.log *.pdf *.idx *.ilg *.ind *.toc *~ *.vrb *.snm *.fls *.nav *.out *.docx *.tex *.html
