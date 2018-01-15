default: DSLsofMath.pdf

%.pdf: %.md template.tex
	pandoc --template=template.tex -o $@ $<
