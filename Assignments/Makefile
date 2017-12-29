default: Assignment01.pdf Assignment02.pdf

%.pdf: %.tex
	latexmk -pdf $<

%.tex: %.lhs
	lhs2TeX --poly $< > $@

clean:
	-rm Assignment01.tex Assignment01.pdf
	-rm Assignment02.tex Assignment02.pdf
