
################################################################################
## [RDP]
## The major modification is here: Rather than evaluate expressions
## and leave them in the global environment, we evaluate them in a
## local environment (that has globalenv() as the parent) and then
## store the assignments in a 'stashR' database.  If an expression
## does not give rise to new R objects, then nothing is saved.
##
## For each expression ('expr'), we compute a digest and associate
## with that digest the names of the objects that were created by
## evaluating the expression.  That way, for a given cached
## expression, we know which keys to lazy-load from the cache when
## evaluation is skipped.
## end [RDP]
##
## [CWB]
## minor modification to the original function 'cacheSweaveEvalWithOpt' has 
## two outputs helping to improve the recognition of changes to code chunks
## end [CWB]
################################################################################


pgfSweaveEvalWithOpt <- function (expr, options) {
  chunkDigest <- options$chunkDigest
  
  ## 'expr' is a single expression, so something like 'a <- 1'
  res <- NULL
  chunkChanged <- TRUE

  if(!options$eval)
      return(res)
  if(options$cache) {
    cachedir <- getCacheDir()
  
    ## Create database name from chunk label and MD5
    ## digest
    dbName <- makeChunkDatabaseName(cachedir, options,chunkDigest)
    exprDigest <- mangleDigest(digest(expr))
  
    ## Create 'stashR' database
    db <- new("localDB", dir = dbName, name = basename(dbName))
  
    ## If the current expression is not cached, then
    ## evaluate the expression and dump the resulting
    ## objects to the database.  Otherwise, just read the
    ## vector of keys from the database
  
    if(!dbExists(db, exprDigest)){
      keys <- try({
        evalAndDumpToDB(db, expr, exprDigest)
        }, silent = TRUE)
    }
    else{
      keys <- dbFetch(db, exprDigest)
      chunkChanged <- FALSE
    }
    
  
    ## If there was an error then just return the
    ## condition object and let Sweave deal with it.
    if(inherits(keys, "try-error"))
      return(list(err=keys,chunkChanged=chunkChanged))
  
    dbLazyLoad(db, globalenv(), keys)
  }
  else {
      ## If caching is turned off, just evaluate the expression
      ## in the global environment
      res <- try(.Internal(eval.with.vis(expr, .GlobalEnv,baseenv())),silent=TRUE)
      
      if(inherits(res, "try-error"))
        return(list(err=res,chunkChanged=chunkChanged))
      if(options$print | (options$term & res$visible))
        print(res$value)
  }
  list(err=res,chunkChanged=chunkChanged)
}

makeExternalShellScriptName <- function(Rnwfile) {
    shellfile <- sub("\\.Rnw$", "\\.sh", Rnwfile)

    ## Don''t clobber
    if(identical(shellfile, Rnwfile))
        shellfile <- paste(Rnwfile, "sh", sep = ".")
    shellfile
}

## to replace the default parse()
parse2 <- function(text, ...) {
    tidy.res = formatR::tidy.source(text = text, out = FALSE, keep.blank.line = 
        ifelse(is.null(getOption('keep.blank.line')), TRUE,
            getOption('keep.blank.line')))
    options(begin.comment = tidy.res$begin.comment, 
        end.comment = tidy.res$end.comment)
    base::parse(text = tidy.res$text.mask)
}

## to replace the default deparse()
deparse2 <- function(expr, ...) {
    gsub(sprintf("%s = \"|%s\"", getOption("begin.comment"),
        getOption("end.comment")), "", base::deparse(expr, ...))
}

  # from the limma package on bioconductor
removeExt  <- function (x) 
{
    x <- as.character(x)
    n <- length(x)
    if (length(grep("\\.", x)) < n) 
        return(x)
    ext <- sub("(.*)\\.(.*)$", "\\2", x)
    if (all(ext[1] == ext)) 
        return(sub("(.*)\\.(.*)$", "\\1", x))
    else return(x)
}