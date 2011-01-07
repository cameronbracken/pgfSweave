all: tidy

tidy:
	R CMD pgfsweave tidy.Rnw
	R CMD pgfsweave tidy2.Rnw

clean:
	rm -rf cache Rplots.* pgfSweave-example-*
	ls | grep -v Rnw | grep -v Makefile | xargs rm

cleanforbuild: clean
