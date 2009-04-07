# Requires latex, bibtex, makeindex, dvips, ps2pdf, psnup
# Thus no guarantee this will work except on my workstations ...

all: docs

docs: allpdf

allpdf: manual.pdf mosmlref.pdf 

LATEX=@pdflatex@

#helpsigs:
#	cd helpsigs; make all base 

manual.pdf: manual.tex mosml.sty
	${LATEX} manual
	bibtex manual
	${LATEX} manual
	${LATEX} manual

#index.tex texsigsigs.tex:
#	cd helpsigs; make all base

mosmlref.pdf: mosmlref.tex mosml.sty
	${LATEX} mosmlref
	${LATEX} mosmlref

#mosmllib.pdf: mosmllib.tex texsigsigs.tex
#	touch mosmllib.ind
#	${LATEX} mosmllib
#	makeindex mosmllib
#	${LATEX} mosmllib
#	${LATEX} mosmllib

CLEANFILES = *.aux *.dvi *.blg *.bbl *.blg *.log *.pdf \
	*.ind *.idx *.ilg *.toc

SUFFIXES = .aux .bbl .tex .bib .dvi .ps .pdf 


