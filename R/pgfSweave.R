pgfSweave <- function(file, compile.tex = TRUE, graphics.only = FALSE, 
    pdf = T, syntax = getOption("SweaveSyntax"), np = 2,...){
    
    #Run Sweave
    Sweave(file,driver=pgfSweaveDriver,syntax=syntax)
    
    #if available compile pgf graphics
    if(compile.tex){
        
        changefile <- '.FigureChunkChanged'
        
        #Strip the extension
        fn <- tools::file_path_sans_ext(file)
        makefile <- paste(fn,".makefile",sep='')

        # set initial calls to latex or pdflatex to generate makefile and 
        # dependency lists
        cmd <- 
        if(pdf)
            Sys.getenv("PDFLATEX","pdflatex")
        else
            Sys.getenv("LATEX","latex")

            # Initial call to pdflatex or latex
        if(!file.exists(makefile)){
            message(paste('Not regenerating makefile for externalization,',
                'if your figures have changed, remove',makefile, 
                'and recompile.'))
            system(paste(cmd,fn))
        }
                    
            # call make to externalize graphics
        make <- Sys.which('make')
        if(file.access(make, 1) == 0){
            if(file.exists(makefile))
                system(paste(make," -j ",np," -f ",makefile,sep=""))
        }else{
            warning('`make` is not available, graphics will not be externalized.')
        }
          
        if(!graphics.only){
            #run texi2dvi on the tex file        
            tools::texi2dvi(paste(fn,'tex',sep='.'), pdf = pdf, ...)
        }
    }

}

installCommandLineScript <- function(){
    source(system.file(package='pgfSweave','exec','install-script.R'))
}