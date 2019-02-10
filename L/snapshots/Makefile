chapters:
	cp ../DSLsofMathNotes.pdf DSLsofMath_all.pdf
	pdftk DSLsofMath_all.pdf cat  11-32  output DSLsofMath_Chapter_1.pdf
	pdftk DSLsofMath_all.pdf cat  33-54  output DSLsofMath_Chapter_2.pdf
	pdftk DSLsofMath_all.pdf cat  55-70  output DSLsofMath_Chapter_3.pdf
	pdftk DSLsofMath_all.pdf cat  71-88  output DSLsofMath_Chapter_4.pdf
	pdftk DSLsofMath_all.pdf cat  89-100 output DSLsofMath_Chapter_5.pdf
	pdftk DSLsofMath_all.pdf cat 101-116 output DSLsofMath_Chapter_6.pdf
	pdftk DSLsofMath_all.pdf cat 117-132 output DSLsofMath_Chapter_7.pdf
	pdftk DSLsofMath_all.pdf cat 133-140 output DSLsofMath_Chapter_8.pdf
	pdftk DSLsofMath_all.pdf cat 141-142 output DSLsofMath_Chapter_9.pdf
	echo "Now rename and check in if the change is big enough for a new snapshot"

txt: $(subst .pdf,.txt,*.pdf)

%.txt: %.pdf
	pdftotext $<
