######################################################################
## Copyright (C) 2011, Cameron Bracken <cameron.bracken@gmail.com>
##             Charlie Sharpsteen <source@shaprpsteen.net>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
#####################################################################

######################################################################
## Heavily relies on functions of cacheSweave but reimplements the
## Sweave driver function.

pgfSweaveDriver <- function() {
    list(
       setup = pgfSweaveSetup,
       runcode = pgfSweaveRuncode,
       writedoc = pgfSweaveWritedoc,
       finish = utils::RweaveLatexFinish,
       checkopts = utils::RweaveLatexOptions
       )
}


source('R/cacheSweaveUnexportedFunctions.R')
source('R/utilities.R')

## Add the 'pgf' and 'external', 'pdflatex', 'sanitize' option to the list
pgfSweaveSetup <- function(file, syntax,
              output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
              eval=TRUE, split=FALSE, stylepath=TRUE,
              pdf=FALSE, eps=FALSE, cache=FALSE, pgf=FALSE,
              tikz=TRUE, external=FALSE, sanitize = FALSE,
              highlight = TRUE, tidy = FALSE)
{
    out <- utils::RweaveLatexSetup(file, syntax, output=output, quiet=quiet,
                     debug=debug, echo=echo, eval=eval,
                     split=split, stylepath=stylepath, pdf=pdf,
                     eps=eps)

    ######################################################################
    ## Additions here [RDP]
    ## Add the (non-standard) options for code chunks with caching
    out$options[["cache"]] <- cache

    ## The pgfSweave options [CWB]
    out$options[["pgf"]] <- pgf
    out$options[["tikz"]] <- tikz
    out$options[["external"]] <- external
    out$options[["sanitize"]] <- sanitize
    out$options[["highlight"]] <- ifelse(echo,highlight,FALSE)
    out$options[["tidy"]] <- tidy
    out[["haveHighlightSyntaxDef"]] <- FALSE
    out[["haveRealjobname"]] <- FALSE
    ## end [CWB]

    ## We assume that each .Rnw file gets its own map file
    out[["mapFile"]] <- makeMapFileName(file)
    file.create(out[["mapFile"]])  ## Overwrite an existing file
    ## End additions [RDP]

    ## [CWB]  create a shell script with a command for each modified graphic
    out[["srcfileName"]] <-paste(tools::file_path_sans_ext(file), "tex", sep='')
    out[["jobname"]] <- basename(tools::file_path_sans_ext(file))
    ######################################################################
    
    out
}

    # This function checks for the \usepackage{Sweave} line and the
    # \tikzexternalize line
pgfSweaveWritedoc <- function(object, chunk)
{
    linesout <- attr(chunk, "srclines")

    if(length(grep("\\usepackage[^\\}]*Sweave.*\\}", chunk)))
        object$havesty <- TRUE
        
    if(is.null(object$havetikzexternalize)){
        object$havetikzexternalize <- 
        if(length(grep("\\tikzexternalize.*^", chunk)))
            TRUE
        else 
            FALSE
    }

    if(!object$havesty){
      begindoc <- "^[[:space:]]*\\\\begin\\{document\\}"
      which <- grep(begindoc, chunk)
      if (length(which)) {
        if(length(which) > 1) cat('havesty',which,'\n')
            chunk[which] <- paste("\\usepackage{", object$styfile, "}\n",
                  chunk[which], sep="")
            linesout <- linesout[c(1L:which, which, seq(from=which+1L, length.out=length(linesout)-which))]
            object$havesty <- TRUE
        }
    }

      # add tikzexternalize if it doesnt exist
    if(!object$havetikzexternalize){
        # add the lines after the \usepackage{tikz} statement
      which <- grep("^[[:space:]]*\\\\usepackage\\{.*tikz.*\\}", chunk)
      if(length(which)){
        chunk[which] <- paste(chunk[which],"\\usetikzlibrary{external}",
          "\\tikzexternalize[mode=list and make]\n", sep="\n")
        linesout <- linesout[c(1L:which, which, seq(from=which+1L, length.out=length(linesout)-which))]
        object$havetikzexternalize <- TRUE
      }
    }
     
      # always add the syntax definitions because there is no real way to
      # check if a single code chunk as the option before hand.
    if (!object$haveHighlightSyntaxDef){
      
        # get the latex style definitions from the highlight package
      tf <- tempfile()
      cat(styler('default', 'sty', styler_assistant_latex),sep='\n',file=tf)
      cat(boxes_latex(),sep='\n',file=tf,append=T)
      hstyle <- readLines(tf)


        # find where to put the style definitions
      begindoc <- "^[[:space:]]*\\\\documentclass.*$"
      which <- grep(begindoc, chunk)

        # add definitions for highlight environment
      hstyle <- c(hstyle, "\\newenvironment{Houtput}{\\raggedright}{%\n%\n}")

            # put in the style definitions after the \documentclass command
      if(length(which)) {
        chunk <- c(chunk[1:which],hstyle,chunk[(which+1):length(chunk)])
        linesout <- linesout[c(1L:which, which, seq(from = which +
            1L, length.out = length(linesout) - which))]
        object$haveHighlightSyntaxDef <- TRUE
      }
    }
    
    while(length(pos <- grep(object$syntax$docexpr, chunk)))
    {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1L]])
        cmd <- substr(chunk[pos[1L]], cmdloc,
                      cmdloc+attr(cmdloc, "match.length")-1L)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if(object$options$eval){
            val <- as.character(eval(parse(text=cmd), envir=.GlobalEnv))
            ## protect against character(0L), because sub() will fail
            if(length(val) == 0L) val <- ""
        }
        else
            val <- paste("\\\\verb{<<", cmd, ">>{", sep="")

        chunk[pos[1L]] <- sub(object$syntax$docexpr, val, chunk[pos[1L]])
    }
    
    while(length(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep=""),
                    "\\1", chunk[pos[1L]])
        object$options <- utils:::SweaveParseOptions(opts, object$options,
                                             utils:::RweaveLatexOptions)
        if (isTRUE(object$options$concordance)
              && !object$haveconcordance) {
            savelabel <- object$options$label
            object$options$label <- "concordance"
            prefix <- utils:::RweaveChunkPrefix(object$options)
            object$options$label <- savelabel
            object$concordfile <- paste(prefix, "tex", sep=".")
            chunk[pos[1L]] <- sub(object$syntax$docopt,
                                 paste("\\\\input{", prefix, "}", sep=""),
                                 chunk[pos[1L]])
            object$haveconcordance <- TRUE
        } else
            chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }

    cat(chunk, sep="\n", file=object$output, append=TRUE)
    object$linesout <- c(object$linesout, linesout)

    return(object)
}



## [RDP] This function is essentially unchanged from the original Sweave
## version, except I compute the digest of the entire chunk, write out
## information to the map file, and use 'cacheSweaveEvalWithOpt'
## instead.  Note that everything in this function operates at the
## chunk level.  The code has been copied from R 2.5.0.

## [CWB] Here are the main changes that allow for tikz graphic output and 
## highlighting and such
pgfSweaveRuncode <- function(object, chunk, options) {

  if(!(options$engine %in% c("R", "S"))){
    return(object)
  }

  if(!object$quiet){
    cat(formatC(options$chunknr, width=2), ":")
    if(options$echo){
      cat(" echo")
      if(options$highlight) cat(" highlight")
      if(options$tidy) cat(" tidy")
    }
    if(options$keep.source) cat(" keep.source")
    if(options$eval){
      if(options$print) cat(" print")
      if(options$term) cat(" term")
      cat("", options$results)
      if(options$fig){
        if(options$eps) cat(" eps")
        if(options$pdf) cat(" pdf")
        if(options$pgf) cat(" pgf")
        if(options$tikz | options$external) cat(" tikz")
        if(options$external) cat(" external")
        if(options$tikz & options$sanitize) cat(" sanitize")
      }
    }
    if(!is.null(options$label))
      cat(" (label=", options$label, ")", sep="")
      cat("\n")
  }


  chunkprefix <- RweaveChunkPrefix(options)

  if(options$split){
    ## [x][[1]] avoids partial matching of x
    chunkout <- object$chunkout[chunkprefix][[1]]
    if(is.null(chunkout)){
      chunkout <- file(paste(chunkprefix, "tex", sep="."), "w")
        if(!is.null(options$label))
          object$chunkout[[chunkprefix]] <- chunkout
    }
  }
  else  chunkout <- object$output

  saveopts <- options(keep.source=options$keep.source)
  on.exit(options(saveopts))

  SweaveHooks(options, run=TRUE)

    # remove unwanted "#line" directives added by R 2.12
  chunk <- removeLineJunk(chunk)

  ## parse entire chunk block
  chunkexps <-
    if(options$tidy){
      try(parse.tidy(text=chunk), silent=TRUE)
    }else{
      try(parse(text=chunk), silent=TRUE)
    }

  RweaveTryStop(chunkexps, options)


  ## Adding my own stuff here [RDP]
  ## Add 'chunkDigest' to 'options'
  options <- writeChunkMetadata(object, chunk, options)
  ## End adding my own stuff [RDP]

  openSinput <- FALSE
  openSchunk <- FALSE
    # for the highlighted output environment
  openHoutput <- FALSE

  if(length(chunkexps)==0)
    return(object)

  srclines <- attr(chunk, "srclines")
  linesout <- integer(0)
  srcline <- srclines[1]

  srcrefs <- attr(chunkexps, "srcref")
  if (options$expand)
    lastshown <- 0
  else
    lastshown <- srcline - 1
  thisline <- 0

  for(nce in 1:length(chunkexps)){
    ce <- chunkexps[[nce]]

    if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
        if (options$expand) {
            # expand references to other chunks
          srcfile <- attr(srcref, "srcfile")
          showfrom <- srcref[1]
          showto <- srcref[3]
        } else {
          srcfile <- object$srcfile
          showfrom <- srclines[srcref[1]]
          showto <- srclines[srcref[3]]
        }
        dce <- getSrcLines(srcfile, lastshown+1, showto)

        if(options$tidy) dce <- unmask.source(dce)

        leading <- showfrom-lastshown
        lastshown <- showto
        srcline <- srclines[srcref[3]]
        
          # Remove blank lines at head of chunk
        while (length(dce) && length(grep("^[[:blank:]]*$", dce[1]))) {
          dce <- dce[-1]
          leading <- leading - 1
        }

    } else {

      dce <- 
      if(options$tidy){
        deparse.tidy(ce, width.cutoff = 0.75 * getOption("width"))
      }else{
        deparse(ce, width.cutoff = 0.75 * getOption("width"))
      }

      leading <- 1

    }

    if(object$debug)
      cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")

    if(options$echo && length(dce)){
      if(!openSinput & !options$highlight){
        if(!openSchunk){
          cat("\\begin{Schunk}\n",file=chunkout, append=TRUE)
            linesout[thisline + 1] <- srcline
            thisline <- thisline + 1
            openSchunk <- TRUE
        }
          cat("\\begin{Sinput}",file=chunkout, append=TRUE)
          openSinput <- TRUE
      }else if(!openHoutput & options$highlight){

          cat("\\begin{Houtput}\n",file=chunkout, append=TRUE)
          linesout[thisline + 1] <- srcline
          thisline <- thisline + 1
          openHoutput <- TRUE

      }

         # Actual printing of chunk code
      if(options$highlight){

          # print highlighted output, treat blank lines specially
        if(length(grep("^[[:space:]]*$",dce)) >= 1 & length(dce) == 1){
          
            # for blank lines for which parser throws an error
          cat(translator_latex(paste(getOption("prompt"),'\n', sep="")),
            file=chunkout, append=TRUE, sep="")
          cat(newline_latex(),file=chunkout, append=TRUE)
          
        }else{

          if(nce == 1)
            cat(newline_latex(),file=chunkout, append=TRUE)

          highlight(parser.output=parser(text=dce),
            renderer=renderer_latex(document=FALSE),
            output = chunkout, showPrompts=TRUE,final.newline = TRUE)
              # highlight doesnt put in an ending newline for some reason
          cat(newline_latex(),file=chunkout, append=TRUE)

        }

      }else{
          # regular output, may be tidy'd or not
        cat("\n",paste(getOption("prompt"), dce[1:leading], sep="",
          collapse="\n"), file=chunkout, append=TRUE, sep="")
        if (length(dce) > leading)
          cat("\n", paste(getOption("continue"), dce[-(1:leading)], sep="",
            collapse="\n"), file=chunkout, append=TRUE, sep="")
            
      }
      
      linesout[thisline + 1:length(dce)] <- srcline
      thisline <- thisline + length(dce)
    }

      # do not evaluate empty expressions, these may occur when tidy=T
    if(length(ce) > 0){

        ## tmpcon <- textConnection("output", "w")
        ## avoid the limitations (and overhead) of output text connections
      tmpcon <- file()
        ## collect output to temporary file
      sink(file=tmpcon)
      err <- NULL


        # use the cacheSweave mechanism to cache objects
      if(options$eval) err <- cacheSweaveEvalWithOpt(ce, options)

      cat("\n") # make sure final line is complete
      sink()
      output <- readLines(tmpcon)
      close(tmpcon)

        ## delete empty output
      if(length(output)==1 & output[1]=="") output <- NULL

      RweaveTryStop(err, options)

      if(object$debug)
        cat(paste(output, collapse="\n"))

      if(length(output)>0 & (options$results != "hide")){
        if(openSinput){
          cat("\n\\end{Sinput}\n", file=chunkout,append=TRUE)
          linesout[thisline + 1:2] <- srcline
          thisline <- thisline + 2
          openSinput <- FALSE
        }

        if(options$results=="verbatim"){
          if(!openSchunk){
            cat("\\begin{Schunk}\n",file=chunkout, append=TRUE)
            linesout[thisline + 1] <- srcline
            thisline <- thisline + 1
            openSchunk <- TRUE
          }
          cat("\\begin{Soutput}\n",file=chunkout, append=TRUE)
          linesout[thisline + 1] <- srcline
          thisline <- thisline + 1
        }

        output <- paste(output,collapse="\n")

        if(options$strip.white %in% c("all", "true")){
          output <- sub("^[[:space:]]*\n", "", output)
          output <- sub("\n[[:space:]]*$", "", output)
          if(options$strip.white=="all")
            output <- sub("\n[[:space:]]*\n", "\n", output)
        }

        cat(output, file=chunkout, append=TRUE)
        count <- sum(strsplit(output, NULL)[[1]] == "\n")

        if (count > 0) {
          linesout[thisline + 1:count] <- srcline
          thisline <- thisline + count
        }

        remove(output)

        if(options$results=="verbatim"){
          cat("\n\\end{Soutput}\n", file=chunkout, append=TRUE)
          linesout[thisline + 1:2] <- srcline
          thisline <- thisline + 2
        }
      }
    }
  }

  if(options$highlight || options$tidy)
    cat("\n", file=chunkout, append=TRUE)

  if(openSinput){
    cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
    linesout[thisline + 1:2] <- srcline
    thisline <- thisline + 2
  }

  if(openSchunk){
    cat("\\end{Schunk}\n", file=chunkout, append=TRUE)
    linesout[thisline + 1] <- srcline
    thisline <- thisline + 1
  }

  if(openHoutput){
    cat("\\end{Houtput}\n", file=chunkout, append=TRUE)
    linesout[thisline + 1] <- srcline
    thisline <- thisline + 1
  }

  if(is.null(options$label) & options$split)
    close(chunkout)

  if(options$split & options$include){
    cat("\\input{", chunkprefix, "}\n", sep="",
      file=object$output, append=TRUE)
    linesout[thisline + 1] <- srcline
    thisline <- thisline + 1
  }

  if(options$fig && options$eval){
      
    chunkChanged <- 
      if( options$external )
        hasChunkChanged(chunk,chunkprefix,options)
      else
        TRUE
    if(chunkChanged & options$external) cat('    Re-running External Chunk:',options$label,'\n')

    if(options$eps & !options$pgf & !options$tikz){
        # Still apply graphics caching to postscript device
        # only regenerate the image if the chunk has been changed.
      if(!options$external | chunkChanged ){

          # [CWB] the useKerning option was added in R 2.9.0
          # so check for the R version and set the
          # useKerning option to false if 2.8.x or less is
          # used.
        if(getRversion() < "2.9.0")
          grDevices::postscript(file = paste(chunkprefix, "eps", sep="."),
            width=options$width, height=options$height,
            paper="special", horizontal=FALSE)
        else
          grDevices::postscript(file = paste(chunkprefix, "eps", sep="."),
            width=options$width, height=options$height,
            paper="special",horizontal=FALSE, useKerning=FALSE)

        err <- try({
          SweaveHooks(options, run=TRUE)
          eval(chunkexps, envir=.GlobalEnv)
          })
        grDevices::dev.off()
        if(inherits(err, "try-error")) stop(err)
      }
    }
    if(options$pdf & !options$pgf & !options$tikz){
      # Still apply graphics caching to pdf device
      # only regenerate the image if the chunk has been changed.
      if(!options$external | chunkChanged ){
        grDevices::pdf(file=paste(chunkprefix, "pdf", sep="."),
          width=options$width, height=options$height,
          version=options$pdf.version, encoding=options$pdf.encoding)

        err <- try({
          SweaveHooks(options, run=TRUE)
          eval(chunkexps, envir=.GlobalEnv)
          })
        grDevices::dev.off()
        if(inherits(err, "try-error")) stop(err)
      }
    }

    #############################################
    ## [CWB] Here are the new options related to graphics output
    ## pgf: uses eps2pgf with the `-m directcopy` option to create
    ##    a pgf file in which the text is interperted as a tex
    ##    string
    ##
    ## external: uses the external feature in
    ##   the TeX package pgf to include the compiled pdf file if
    ##   available
    ##

    if(options$pgf){
      if( !options$external | chunkChanged ){

          # [CWB] the useKerning option was added in R 2.9.0
          # so check for the R version and set the
          # useKerning option to false if 2.8.x or less is
          # used.
        if(getRversion() < "2.9.0")
          grDevices::postscript(file = paste(chunkprefix, "eps", sep="."),
            width=options$width, height=options$height,
            paper="special", horizontal=FALSE)
        else
          grDevices::postscript(file = paste(chunkprefix, "eps", sep="."),
            width=options$width, height=options$height,
            paper="special",horizontal=FALSE, useKerning=FALSE)

        err <- try({
          SweaveHooks(options, run=TRUE)
          eval(chunkexps, envir=.GlobalEnv)
          })
        grDevices::dev.off()
        if(inherits(err, "try-error")) stop(err)

        eps2pgf <- system.file(package='pgfSweave')
        eps2pgf <- paste('java -jar ',eps2pgf,
          '/java/eps2pgf/eps2pgf.jar -m directcopy',sep='')

        cat('Generating pgf file from:',paste(chunkprefix, "eps",sep="."),'\n')
        err <- try(system(paste(eps2pgf,paste(chunkprefix, "eps",sep="."))))

        if(inherits(err, "try-error")) stop(err)
      }
    }

    if( options$tikz ){
      if( !options$external | chunkChanged ){
        tikzDevice::tikz(file=paste(chunkprefix, "tikz", sep="."),
          width=options$width, height=options$height,
          sanitize=options$sanitize)

        err <- try({
          SweaveHooks(options, run=TRUE)
          eval(chunkexps, envir=.GlobalEnv)
          })
        grDevices::dev.off()
        if(inherits(err, "try-error")) stop(err)
        
      }
    }

      # Write the extrnalization commands
    if(options$include && options$external) {
      cat("\n\\tikzsetnextfilename{",chunkprefix,"}\n",sep="",
        file=object$output, append=TRUE)
      cat("\n\\tikzexternalfiledependsonfile{",chunkprefix,"}{",
        paste(chunkprefix, "tikz", sep="."),"}\n",sep="",
        file=object$output, append=TRUE)
        
      linesout[thisline + 1] <- srcline
      thisline <- thisline + 1
      
    }

      # Write the includegraphics command for eps or pdf 
      # only if we are not useing pgf or tikz
    if(options$include && !options$pgf && !options$tikz && !options$external) {

      cat("\\includegraphics{", chunkprefix, "}\n", sep="",
        file=object$output, append=TRUE)
      linesout[thisline + 1] <- srcline
      thisline <- thisline + 1

    }
      # input statements for tikz and pgf
    if(options$include && (options$pgf || options$tikz)) {

        #if tikz takes precident over pgf option
      suffix <- ifelse(options$tikz,'tikz','pgf')
      if(!options$external){
        cat("{\\tikzexternaldisable\n", sep="", file=object$output, append=TRUE)
        linesout[thisline + 1] <- srcline
        thisline <- thisline + 1
      }
      
      cat("\\input{", paste(chunkprefix,suffix,sep='.'),
        "}\n", sep="", file=object$output, append=TRUE)
      linesout[thisline + 1] <- srcline
      thisline <- thisline + 1
      
      if(!options$external){
        cat("}\n", sep="", file=object$output, append=TRUE)
        linesout[thisline + 1] <- srcline
        thisline <- thisline + 1
      }

    }
    ##end graphics options [CWB]
    #############################################
  }
  object$linesout <- c(object$linesout, linesout)
  return(object)
}
