default: HCAR.pdf

%.tex: %.lhs
	lhs2TeX --poly $< > $@

HCAR.tex:: DSLsofMath.tex

%.pdf: %.tex
	latexmk -pdf $<

clean:
	latexmk -C HCAR
