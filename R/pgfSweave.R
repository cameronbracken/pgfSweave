pgfSweave <- function(file, compile.tex = TRUE, syntax = getOption("SweaveSyntax"), np = 2,...){
    
    #Run Sweave
    Sweave(file,driver=pgfSweaveDriver,syntax=syntax)
    
    #if available compile pgf graphics
    if(compile.tex){
        
        #Strip the extension
        fn <- tools::file_path_sans_ext(file)

        # set initial calls to latex or pdflatex to generate makefile and 
        # dependency lists
        cmd <- 
        if(is.null(match.call()$pdf))
            Sys.getenv("LATEX","latex")
        else
            Sys.getenv("PDFLATEX","pdflatex")

            # Initial call to pdflatex or latex
        system(paste(cmd,fn))

            # call make to externalize graphics
        make <- Sys.which('make')
        if(file.access(make, 1) == 0)
          system(paste(make," -j ",np," -f ",fn,".makefile",sep=""))
        else 
          warning('`make` is not available, graphics will not be externalized.')

        #run texi2dvi on the tex file        
        tools::texi2dvi(paste(fn,'tex',sep='.'),...)
    }

}
