default: D2pres.pdf TM3pres.pdf

%.html: %.md
	pandoc --self-contained -S --latexmathml -t slidy $< -o $@

%.pdf: %.md
	pandoc -t beamer $< -o $@
