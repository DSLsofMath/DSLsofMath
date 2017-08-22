default: Exam-2017-08.pdf

%.pdf: %.tex
	latexmk -pdf $<

%.tex : %.lhs
	lhs2TeX $< > $@

clean:
	- rm -f *.aux *.bbl *.blg *.fdb_latexmk *.log *.pdf *.idx *.ilg *.ind *.toc *~ *.vrb *.snm *.fls *.nav *.out *.docx *.tex *.html
