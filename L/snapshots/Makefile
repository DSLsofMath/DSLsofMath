chapters:
	cp ../DSLsofMathNotes.pdf DSLsofMath_all.pdf
	pdftk DSLsofMath_all.pdf cat  11-32  output DSLsofMath_Chapter_1.pdf
	pdftk DSLsofMath_all.pdf cat  33-54  output DSLsofMath_Chapter_2.pdf
	pdftk DSLsofMath_all.pdf cat  55-70  output DSLsofMath_Chapter_3.pdf
	pdftk DSLsofMath_all.pdf cat  71-88  output DSLsofMath_Chapter_4.pdf
	pdftk DSLsofMath_all.pdf cat  89-102 output DSLsofMath_Chapter_5.pdf
	pdftk DSLsofMath_all.pdf cat 103-118 output DSLsofMath_Chapter_6.pdf
	pdftk DSLsofMath_all.pdf cat 119-136 output DSLsofMath_Chapter_7.pdf
	pdftk DSLsofMath_all.pdf cat 137-146 output DSLsofMath_Chapter_8.pdf
	pdftk DSLsofMath_all.pdf cat 147-148 output DSLsofMath_Chapter_9.pdf
	echo "Now rename and check in if the change is big enough for a new snapshot"

txt: $(subst .pdf,.txt,*.pdf)

%.txt: %.pdf
	pdftotext $<
