chapters:
	cp ../DSLsofMathNotes.pdf .
	pdftk DSLsofMathNotes.pdf cat  11-32  output DSLsofMath_Chapter_1.pdf
	pdftk DSLsofMathNotes.pdf cat  33-52  output DSLsofMath_Chapter_2.pdf
	pdftk DSLsofMathNotes.pdf cat  53-68  output DSLsofMath_Chapter_3.pdf
	pdftk DSLsofMathNotes.pdf cat  69-84  output DSLsofMath_Chapter_4.pdf
	pdftk DSLsofMathNotes.pdf cat  85-96  output DSLsofMath_Chapter_5.pdf
	pdftk DSLsofMathNotes.pdf cat  97-112 output DSLsofMath_Chapter_6.pdf
	pdftk DSLsofMathNotes.pdf cat 113-128 output DSLsofMath_Chapter_7.pdf
	pdftk DSLsofMathNotes.pdf cat 129-136 output DSLsofMath_Chapter_8.pdf
	pdftk DSLsofMathNotes.pdf cat 137-138 output DSLsofMath_Chapter_9.pdf
	echo "Now rename and check in if the change is big enough for a new snapshot"

txt: $(subst .pdf,.txt,*.pdf)

%.txt: %.pdf
	pdftotext $<
