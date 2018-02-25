chapters:
	cp ../DSLsofMathNotes.pdf .
	pdftk DSLsofMathNotes.pdf cat   9-26  output DSLsofMath_Chapter_1.pdf
	pdftk DSLsofMathNotes.pdf cat  27-46  output DSLsofMath_Chapter_2.pdf
	pdftk DSLsofMathNotes.pdf cat  47-62  output DSLsofMath_Chapter_3.pdf
	pdftk DSLsofMathNotes.pdf cat  63-78  output DSLsofMath_Chapter_4.pdf
	pdftk DSLsofMathNotes.pdf cat  79-90  output DSLsofMath_Chapter_5.pdf
	pdftk DSLsofMathNotes.pdf cat  91-106 output DSLsofMath_Chapter_6.pdf
	pdftk DSLsofMathNotes.pdf cat 107-120 output DSLsofMath_Chapter_7.pdf
	pdftk DSLsofMathNotes.pdf cat 121-128 output DSLsofMath_Chapter_8.pdf
	echo "Now rename and check in if the change is big enough for a new snapshot"

txt: $(subst .pdf,.txt,*.pdf)

%.txt: %.pdf
	pdftotext $<
