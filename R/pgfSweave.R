pgfSweave <- function(file, compile.tex = TRUE, syntax = getOption("SweaveSyntax"), ...){
    
    #Run Sweave
    Sweave(file,driver=pgfSweaveDriver,syntax=syntax)
    
	#if available compile pgf graphics
    if(compile.tex){
        
        #Strip the extension and compile the pgf graphics separately
        
	    bn <- strsplit(basename(file),"\\.Rnw")[[1]][1]
        dn <- dirname(file)
        fn <- file.path(dn,bn)
        cmds <- readLines(paste(fn,'sh',sep='.'))
        dummy <- lapply(cmds,system)

        #set special versions of calls to latex or pdflatex
        if(is.null(match.call()$pdf))
            Sys.setenv(LATEX=paste("latex --jobname=",bn,sep=''))
        else
            Sys.setenv(PDFLATEX=paste("pdflatex --jobname=",bn,sep=''))
    
        #run texi2dvi on the tex file        
        tools::texi2dvi(paste(fn,'tex',sep='.'),...)
    }

}
