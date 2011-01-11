
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
################################################################################

######################################################################
## Take a 'filehash' database and insert a bunch of key/value pairs

dumpToDB <- function(db, list = character(0), envir = parent.frame()) {
        if(!is(db, "filehash"))
                stop("'db' should be a 'filehash' database")
        for(i in seq(along = list))
                dbInsert(db, list[i], get(list[i], envir, inherits = FALSE))
        invisible(db)
}

copy2env <- function(keys, fromEnv, toEnv) {
        for(key in keys) {
                assign(key, get(key, fromEnv, inherits = FALSE), toEnv)
        }
}

## Take an environment and return a copy.  Not an exact copy because
## we don't get all keys (not sure why, but for some reason I remember
## that getting all the keys caused problems.

copyEnv <- function(from) {
        env <- new.env(parent = parent.env(from))
        keys <- ls(from, all.names = FALSE)

        for(key in keys) 
                assign(key, get(key, from, inherits = FALSE), env)
        env
}

isNewOrModified <- function(specials, e1, e2) {
        sapply(specials, function(s) {
                in1 <- exists(s, e1, inherits = FALSE)
                in2 <- exists(s, e2, inherits = FALSE)
                is.new <- !in1 && in2
                is.deleted <- in1 && !in2
                
                if((!in1 && !in2) || is.deleted)
                        FALSE
                else if(is.new)
                        TRUE
                else 
                        !identical(get(s, e1, inherits = FALSE),
                                   get(s, e2, inherits = FALSE))
        })
}

## Check for new symbols in 'e2' that are not in 'e1'; doesn't check
## for modified symbols.

## If 'source()' was used, there may be new symbols in the global
## environment, unless 'source(local = TRUE)' was used.  Also applies
## for 'set.seed()'.

checkNewSymbols <- function(e1, e2) {
        if(identical(e1, e2))
                return(character(0))
        specials <- c(".Random.seed")

        ## Don't check for names beginning with '.' for now
        sym1 <- ls(e1)
        sym2 <- ls(e2)
        newsym <- setdiff(sym2, sym1)

        use <- isNewOrModified(specials, e1, e2)
        c(newsym, specials[use])
}

## Take an expression, evaluate it in a local environment and dump the
## results to a database.  Associate the names of the dumped objects
## with a digest of the expression.  Return a character vector of keys
## that were dumped

evalAndDumpToDB <- function(db, expr, exprDigest) {
        env <- new.env(parent = globalenv())
        global1 <- copyEnv(globalenv())
        
        eval(expr, env)

        global2 <- copyEnv(globalenv())

        ## Functions like 'source' and 'set.seed' alter the global
        ## environment, so check after evaluation
        new.global <- checkNewSymbols(global1, global2)
        copy2env(new.global, globalenv(), env)

        ## Get newly assigned object names
        keys <- ls(env, all.names = TRUE)

        ## Associate the newly created keys with the digest of
        ## the expression
        dbInsert(db, exprDigest, keys)

        ## Dump the values of the keys to the database
        dumpToDB(db, list = keys, envir = env)

	if(length(keys) > 0)
		copy2env(keys, env, globalenv())
        keys
}

makeChunkDatabaseName <- function(cachedir, options, chunkDigest) {
        file.path(cachedir, paste(options$label, chunkDigest, sep = "_"))
}

mangleDigest <- function(x) {
        paste(".__", x, "__", sep = "")
}

hash <- function(object) {
	digest(object, algo = "sha1")
}

hashExpr <- function(expr) {
	expr <- deparse(expr, width.cutoff = 60)
	hash(expr)
}

cacheSweaveEvalWithOpt <- function (expr, options) {
  chunkDigest <- options$chunkDigest
  
  ## 'expr' is a single expression, so something like 'a <- 1'
  res <- NULL

  if(!options$eval)
          return(res)
  if(options$cache) {
    cachedir <- getCacheDir()

    ## Create database name from chunk label and MD5
    ## digest
    dbName <- makeChunkDatabaseName(cachedir, options, chunkDigest)
    exprDigest <- mangleDigest(hashExpr(expr))

    ## Create 'stashR' database
    db <- new("localDB", dir = dbName, name = basename(dbName))

    ## If the current expression is not cached, then
    ## evaluate the expression and dump the resulting
    ## objects to the database.  Otherwise, just read the
    ## vector of keys from the database

    if(!dbExists(db, exprDigest)) {

      keys <- try({
        evalAndDumpToDB(db, expr, exprDigest)
      }, silent = TRUE)

      ## If there was an error then just return the
      ## condition object and let Sweave deal with it.
      if(inherits(keys, "try-error"))
        return(keys)

    } else {

      keys <- dbFetch(db, exprDigest)
      dbLazyLoad(db, globalenv(), keys)

    }
    keys

  } else {

    ## If caching is turned off, just evaluate the expression
    ## in the global environment
    res <- try(withVisible(eval(expr, .GlobalEnv)),
               silent=TRUE)
    if(inherits(res, "try-error"))
      return(res)
    if(options$print | (options$term & res$visible))
      print(res$value)
  }
  res
}

makeExternalShellScriptName <- function(Rnwfile) {
    shellfile <- sub("\\.[rR]nw$", "\\.sh", Rnwfile)

    ## Don''t clobber
    if(identical(shellfile, Rnwfile))
        shellfile <- paste(Rnwfile, "sh", sep = ".")
    shellfile
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
