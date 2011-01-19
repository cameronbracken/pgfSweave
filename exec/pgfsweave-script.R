#!/usr/bin/env Rscript

usage <- "Usage: R CMD pgfsweave [options] file

A simple front-end for pgfSweave()

The options below reference the following steps 
  (1) Run Sweave using pgfSweaveDriver
  (2) Run the pgf externalization commands
  (3) Compile the resulting tex file using texi2dvi()

Default behavior (no options) is to do (1), (2) then (3) in that order.

Options:
  -h, --help                print short help message and exit
  -v, --version             print version info and exit
  -d, --dvi                 dont use texi2dvi() option pdf=T i.e. call plain 
                            latex (defalt is pdflatex)
  -n, --graphics-only       dont do (3), do (1) then (2); ignored if 
                            --pgfsweave-only is used
  -s, --pgfsweave-only      dont do (2) or (3), only do (1)

Package repositories: 
http://github.com/cameronbracken/pgfSweave (cutting edge development)
http://r-forge.r-project.org/projects/pgfsweave/ (for precompiled packages)
"

ver <- packageDescription('pgfSweave')[['Version']]

suppressPackageStartupMessages(library(getopt))

#Column 3: Argument mask of the flag. An integer. Possible values: 
# 0=no argument, 1=required argument, 2=optional argument. 
optspec <- matrix(c(
  'help'          , 'h', 0, "logical",
  'version'       , 'v', 0, "logical",
  'dvi'           , 'd', 0, "logical",
  'pgfsweave-only', 's', 0, "logical",
  'graphics-only' , 'n', 0, "logical",
  'processors'    , 'p', 1, "integer"
),ncol=4,byrow=T)

opt <- try(getopt(optspec),silent=TRUE)
if(class(opt) == 'try-error') opt <- list()
opt[names(opt)=="ARGS"] <- NULL

if( !is.null(opt$help    )) { cat(usage); q(status=1) }
if( !is.null(opt$version )) { cat(ver,'\n'); q(status=1) }
opt$dvi <- ifelse(is.null(opt$dvi), FALSE, TRUE )
opt[['pgfsweave-only']] <- ifelse( is.null(opt[['pgfsweave-only']]), FALSE, TRUE )
opt[['graphics-only']] <- ifelse( is.null(opt[['graphics-only']]), FALSE, TRUE )
opt[['processors']] <- ifelse( is.null(opt[['processors']]), 2, opt[['processors']] )
                            
args <- commandArgs(TRUE)
file <- args[length(args)]

if(length(file) == 0) { 
    cat('No input file.\n')
    cat(usage) 
    q(status=1) 
}

suppressPackageStartupMessages(library(filehash))
suppressPackageStartupMessages(library(stashR))
suppressPackageStartupMessages(library(cacheSweave))
suppressPackageStartupMessages(library(tikzDevice))
suppressPackageStartupMessages(library(pgfSweave))

cat('pgfsweave-script.R: pgfSweave version',ver,'\n')
for(i in seq_along(opt))
	cat(names(opt)[i],':',rep(' ',max(nchar(names(opt))) - nchar(names(opt)[i])),
		'   ',opt[[i]],'\n',sep='')
cat('\n')

if(opt[['graphics-only']]){
    
    pgfSweave(file, pdf=!opt$dvi, compile.tex=FALSE) 
                   
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
    
}else{
    # May be running with --pgfsweave-only or not.
    # In this case, just make a call to the R function pgfSweave() with the
    # options intact.
    
    pgfSweave(file, pdf=!opt$dvi,compile.tex = !opt[['pgfsweave-only']])

}


