default: Assignment01.pdf Assignment02.pdf Assignment01.html Assignment02.html

%.pdf: %.tex
	latexmk -pdf $<

%.tex: %.lhs
	lhs2TeX --poly $< > $@

%.html: %.lhs
	pandoc $< -f LaTeX+lhs -o $@

clean:
	-latexmk -c Assignment01.tex
	-latexmk -c Assignment02.tex
	-rm Assignment01.tex
	-rm Assignment01.pdf
	-rm Assignment01.html
	-rm Assignment02.tex
	-rm Assignment02.pdf
	-rm Assignment01.html
	-rm Assignment01.ptb
	-rm Assignment02.ptb
