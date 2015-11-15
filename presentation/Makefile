default: TM3pres.html

TM3pres.html: TM3pres.md
	pandoc --self-contained -S --latexmathml -t slidy $< -o $@
