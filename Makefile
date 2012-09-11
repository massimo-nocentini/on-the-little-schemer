document:
	latex Elaborato.tex && dvips -t a4 -Ppdf Elaborato.dvi
	ps2pdf Elaborato.ps Elaborato.pdf
