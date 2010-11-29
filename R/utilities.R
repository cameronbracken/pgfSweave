
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

## preserve comments in the R code and at the same time make them 'well-formatted'
tidy.source <- function(source = "clipboard", keep.comment = TRUE,
    keep.blank.line = TRUE, begin.comment, end.comment, output = TRUE,
    width.cutoff = 60L, ...) {
    if (source == "clipboard" && Sys.info()["sysname"] == "Darwin") {
        source = pipe("pbpaste")
    }
    tidy.block = function(block.text) {
        exprs = base::parse(text = block.text)
        n = length(exprs)
        res = character(n)
        for (i in 1:n) {
            dep = paste(base::deparse(exprs[i], width.cutoff),
                collapse = "\n")
            res[i] = substring(dep, 12, nchar(dep) - 1)
        }
        return(res)
    }
    text.lines = readLines(source, warn = FALSE)
    if (keep.comment) {
        identifier = function() "pgfSweaveCommentIdentifier__"
        #paste(sample(LETTERS), collapse = "")
        if (missing(begin.comment))
            begin.comment = identifier()
        if (missing(end.comment))
            end.comment = identifier()
        
        text.lines = gsub("^[[:space:]]+|[[:space:]]+$", "",
            text.lines)
        while (length(grep(sprintf("%s|%s", begin.comment, end.comment),
            text.lines))) {
            begin.comment = identifier()
            end.comment = identifier()
        }
        line.num.comment = substring(text.lines, 1, 5) == "#line" 
        text.lines = text.lines[!line.num.comment]
          head.comment = substring(text.lines, 1, 1) == "#"
          #grep("^[[:space:]]+|#",text.lines)
          #
        if ( length(head.comment) > 0 ) {
            text.lines[head.comment] = gsub("\"", "'", text.lines[head.comment])
            text.lines[head.comment] = gsub("^#", "  #", text.lines[head.comment])
            text.lines[head.comment] = sprintf("%s=\"%s%s\"",
                begin.comment, text.lines[head.comment], end.comment)
        }
        blank.line = text.lines == ""
        if (any(blank.line) & keep.blank.line)
            text.lines[blank.line] = sprintf("%s=\"%s\"", begin.comment,
                end.comment)
        text.mask = tidy.block(text.lines)
        text.tidy = gsub(sprintf("%s = \"|%s\"", begin.comment,
            end.comment), "", text.mask)
    }
    else {
        text.tidy = text.mask = tidy.block(text.lines)
        begin.comment = end.comment = ""
    }
    if (output)
        cat(paste(text.tidy, collapse = "\n"), "\n", ...)
    invisible(list(text.tidy = text.tidy, text.mask = text.mask,
        begin.comment = begin.comment, end.comment = end.comment))
}

## to replace the default parse()
parse2 <- function(text, ...) {
    zz = tempfile()
    enc = options(encoding = "native.enc")
    writeLines(text, zz)
    tidy.res = tidy.source(zz, out = FALSE, keep.blank.line = 
        ifelse(is.null(getOption('keep.blank.line')), TRUE,
            getOption('keep.blank.line')))
    options(enc)
    unlink(zz)
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