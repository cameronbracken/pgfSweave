	#Must already have a version of pgfSweave installed to compile this.
	#This is a workaround for using pgfSweave to compile the vignette
	#in the CRAN version without requiring pgfSweave already.
	#
	#The idea is that before submitting, the developer compiles the source
	#of the vignette with the version of pgfSweave they have been working on 
	#by running this make file.  Then copy the tex file output to inst/doc
	#as an Rnw file (with no code chunks) so that it registers as an official 
	#vignette.  
	#
	#Requires a little finagling to change the path to the example files and 
	#get rid of the \pgfrealjobname line

# Internal values

DOC_PATH=../../doc/
EXAMPLE_PATH=../../example/
TEST_PATH=../../tests/
EXEC_PATH=../../../exec/
CHANGELOG_PATH=../../../

RM_PATH={*,$(DOC_PATH)/*}
RM_SUFFIX={sh,map,aux,tex,log,dvi,in,fdb_latexmk,out,toc,dep,dpth,figlist}
BUILD_RM_SUFFIX={log,aux,dvi,sh,map,fdb_latexmk,toc,out,dep,dpth,figlist}

#
VIGNETTE_SOURCE_PDF=pgfSweave-vignette-source.pdf

VIGNETTE_DUMMY_PDF=$(DOC_PATH)/pgfSweave.pdf
VIGNETTE_DUMMY_RNW=$(DOC_PATH)/pgfSweave.Rnw

EXAMPLE_TEX=$(EXAMPLE_PATH)/pgfSweave-example.tex
EXAMPLE_PDF=$(EXAMPLE_PATH)/pgfSweave-example.pdf

all: $(VIGNETTE_DUMMY_PDF) changelog

$(VIGNETTE_DUMMY_PDF): $(VIGNETTE_DUMMY_RNW)
	(cd $(DOC_PATH)/ ; R CMD Sweave pgfSweave.Rnw; latexmk -pdf pgfSweave.tex)

$(VIGNETTE_DUMMY_RNW): $(EXAMPLE_PDF)  $(VIGNETTE_SOURCE_PDF)
	# Copy Sweave.sty
	cp Sweave.sty $(DOC_PATH)/
	# Copy all the figures
	cp figs/*.pdf $(DOC_PATH)/figs/
	cp figs/*.pgf $(DOC_PATH)/figs/
	cp figs/*.tikz $(DOC_PATH)/figs/
	# Copy all the example file to include but dont use .Rnw so they are not 
	# recognised as another vignette 
	cp -f $(EXAMPLE_PATH)/pgfSweave-example.Rnw \
		  $(DOC_PATH)/pgfSweave-example-Rnw.in
	cp -f $(EXAMPLE_PATH)/pgfSweave-example.tex \
		  $(DOC_PATH)/pgfSweave-example-tex.in
	cp -f $(EXAMPLE_PATH)/pgfSweave-example.pdf \
		  $(DOC_PATH)/pgfSweave-example.pdf
	# Remove the pgfrealjobname command and the path to the example files 
	# since the new file will be in the same directory 
	cat pgfSweave-vignette-source.tex \
		| sed 's/\.\.\/\.\.\/example\///g' > tmp
	# Change the names of the input files
	cat tmp | sed 's/pgfSweave-example\.Rnw/pgfSweave-example-Rnw.in/g' \
			| sed 's/pgfSweave-example\.tex/pgfSweave-example-tex\.in/g' \
			> ../../doc/pgfSweave.Rnw

$(VIGNETTE_SOURCE_PDF):
	echo `pwd`
	$(EXEC_PATH)/pgfsweave-script.R pgfSweave-vignette-source.Rnw

$(EXAMPLE_PDF):
	(cd $(EXAMPLE_PATH)/ ; make)

cleanall:
	rm -rf cache figs *.pdf $(RM_PATH).$(RM_SUFFIX) \
		$(DOC_PATH)/Sweave.sty $(DOC_PATH)/pgfSweave.Rnw $(DOC_PATH)/*.pdf \
		$(DOC_PATH)/figs/* tmp $(CHANGELOG) 
	(cd $(EXAMPLE_PATH) ; make clean)
	(cd $(TEST_PATH) ; make clean)

clean:
	rm -rf cache figs *.pdf $(RM_PATH).$(BUILD_RM_SUFFIX) \
		./pgfSweave-vignette-source.tex $(DOC_PATH)/pgfSweave.tex tmp
	(cd $(EXAMPLE_PATH) ; make cleanforbuild)
	(cd $(TEST_PATH) ; make cleanforbuild)

changelog:
	git log --stat > ../../../ChangeLog
