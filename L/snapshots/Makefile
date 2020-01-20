chapters:
	cp ../DSLsofMathNotes.pdf DSLsofMath_all.pdf
	pdftk DSLsofMath_all.pdf cat  11-34  output DSLsofMath_Chapter_1.pdf
	pdftk DSLsofMath_all.pdf cat  35-56  output DSLsofMath_Chapter_2.pdf
	pdftk DSLsofMath_all.pdf cat  57-72  output DSLsofMath_Chapter_3.pdf
	pdftk DSLsofMath_all.pdf cat  73-90  output DSLsofMath_Chapter_4.pdf
	pdftk DSLsofMath_all.pdf cat  91-104 output DSLsofMath_Chapter_5.pdf
	pdftk DSLsofMath_all.pdf cat 105-120 output DSLsofMath_Chapter_6.pdf
	pdftk DSLsofMath_all.pdf cat 121-138 output DSLsofMath_Chapter_7.pdf
	pdftk DSLsofMath_all.pdf cat 139-148 output DSLsofMath_Chapter_8.pdf
	pdftk DSLsofMath_all.pdf cat 149-150 output DSLsofMath_Chapter_9.pdf
	echo "Now rename and check in if the change is big enough for a new snapshot"

txt: $(subst .pdf,.txt,*.pdf)

%.txt: %.pdf
	pdftotext $<
