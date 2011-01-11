all: pgfSweave-example.pdf caching.pdf
pgfSweave-example.pdf:
	R CMD pgfsweave pgfSweave-example.Rnw
caching.pdf:
	R CMD pgfsweave caching.Rnw
clean:
	rm -rf cache Rplots.* pgfSweave-example-* caching-
	ls | grep -v Rnw | grep -v Makefile | xargs rm
cleanforbuild: clean
