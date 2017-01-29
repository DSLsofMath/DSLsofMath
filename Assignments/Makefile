default: Assignment02.pdf

Assignment01.pdf: Assignment01.lhs
	pandoc --latex-engine=xelatex Assignment01.lhs -o Assignment01.pdf

# TODO: make sure ⊆ appears in the output

%.pdf: %.tex
	latexmk -pdf $<

%.tex: %.lhs
	lhs2TeX --poly $< > $@

clean:
	-rm Assignment02.tex Assignment02.pdf
