all: pgfSweave-example.pdf pgfSweave-latex-example.pdf caching.pdf
pgfSweave-example.pdf:
	R CMD pgfsweave pgfSweave-example.Rnw
caching.pdf:
	R CMD pgfsweave caching.Rnw
pgfSweave-latex-example.pdf:
	R CMD pgfSweave --pgfsweave-only pgfSweave-latex-example.Rnw 
	latex pgfSweave-latex-example.tex 
	make -f pgfSweave-latex-example.makefile
	latex pgfSweave-latex-example.tex 
	dvipdf pgfSweave-latex-example.dvi
clean:
	rm -rf cache Rplots.* pgfSweave-example-* pgfSweave-latex-example-* caching-*
	ls | grep -v Rnw | grep -v Makefile | xargs rm
cleanforbuild: clean
