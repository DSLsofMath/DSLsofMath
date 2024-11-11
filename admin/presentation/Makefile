default: D2pres.pdf TM3pres.pdf

%.html: %.md
	pandoc --self-contained -S --latexmathml -t slidy $< -o $@

# %.pdf: %.md
#	pandoc -t beamer $< -o $@

%.pdf: %.tex
	latexmk -pdf $<

%.tex: %.lhs
	lhs2TeX --poly $< > $@
