chapters:
	cp ../DSLsofMathNotes.pdf .
	pdftk DSLsofMathNotes.pdf cat  11-32  output DSLsofMath_Chapter_1.pdf
	pdftk DSLsofMathNotes.pdf cat  33-54  output DSLsofMath_Chapter_2.pdf
	pdftk DSLsofMathNotes.pdf cat  55-70  output DSLsofMath_Chapter_3.pdf
	pdftk DSLsofMathNotes.pdf cat  71-86  output DSLsofMath_Chapter_4.pdf
	pdftk DSLsofMathNotes.pdf cat  87-98  output DSLsofMath_Chapter_5.pdf
	pdftk DSLsofMathNotes.pdf cat  99-114 output DSLsofMath_Chapter_6.pdf
	pdftk DSLsofMathNotes.pdf cat 115-130 output DSLsofMath_Chapter_7.pdf
	pdftk DSLsofMathNotes.pdf cat 131-138 output DSLsofMath_Chapter_8.pdf
	pdftk DSLsofMathNotes.pdf cat 139-140 output DSLsofMath_Chapter_9.pdf
	echo "Now rename and check in if the change is big enough for a new snapshot"

txt: $(subst .pdf,.txt,*.pdf)

%.txt: %.pdf
	pdftotext $<
