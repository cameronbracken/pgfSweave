pgfSweave-vignette.pdf: figsdir
	R CMD pgfsweave pgfSweave-vignette.Rnw
figsdir:
	mkdir figs
clean:
	rm -rf cache figs/* Rplots.* *.sh *.map *.aux *.tex *.log *.pdf
