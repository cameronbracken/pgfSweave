pgfSweave-example.pdf:
	../../exec/pgfsweave-script.R pgfSweave-example.Rnw
clean:
	rm -rf cache Rplots.* pgfSweave-example-*
	ls | grep -v Rnw | grep -v Makefile | xargs rm
cleanforbuild: clean
