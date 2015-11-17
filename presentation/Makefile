default: TM3pres.html

%.html: %.md
	pandoc --self-contained -S --latexmathml -t slidy $< -o $@

%.pdf: %.md
	pandoc -t beamer $< -o $@
