######################################################################
## These functions are directly copied from the cacheSweave package.
## The reason they are copied here is because they are internal functions
## and hence are not exported into the global environment.  Some functions 
## are also hacked a little, which is the reason the ::: operator is not 
## used. The  original comments and header are preserved.




######################################################################
## Copyright (C) 2006, Roger D. Peng <rpeng@jhsph.edu>
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
## Taken/adapted from Sweave code by Friedrich Leisch, along the lines
## of 'weaver' from Bioconductor, but more naive and we use 'stashR'
## databases for the backend.  We also don't check dependencies on
## previous chunks.


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

makeMapFileName <- function(Rnwfile) {
        mapfile <- sub("\\.Rnw$", "\\.map", Rnwfile)

        ## Don't clobber
        if(identical(mapfile, Rnwfile))
                mapfile <- paste(Rnwfile, "map", sep = ".")
        mapfile
}

writeChunkMetadata <- function(object, chunk, options) {
        chunkprefix <- utils::RweaveChunkPrefix(options)
        chunkexps <- parse(text = chunk, srcfile = NULL)
        chunkDigest <- digest(chunkexps)
        
        options$chunkDigest <- chunkDigest
        
        ## If there's a data map file then write the chunk name and the
        ## directory of the chunk database to the map file (in DCF format)
        dbName <- if(isTRUE(options$cache))
                makeChunkDatabaseName(getCacheDir(), options, chunkDigest)
        else
                ""
        ## Capture figure filenames; default to PDF, otherwise use EPS.
        ## Filenames are <chunkprefix>.<extenstion>, which could change in
        ## the future depending on Sweave implementation details
        ## [CWB] added pgf and tikz extension.
        figname <- ""
        if(options$fig && options$eval) {
                figname <- if(options$pdf)
                        paste(chunkprefix, "pdf", sep = ".")
                else if(options$eps)
                        paste(chunkprefix, "eps", sep = ".")
                else if(options$pgf)
                        paste(chunkprefix, "pgf", sep = ".")
                else if(options$tikz)
                        paste(chunkprefix, "tikz", sep = ".")
                else
                        ""
        }
        ## Write out map file entry
        mapFile <- object[["mapFile"]]
        mapEntry <- data.frame(chunk = ifelse(is.null(options$label),
                                           chunkprefix,options$label),
                               chunkprefix = chunkprefix,
                               fig = figname,
                               cacheDB = dbName,
                               time = Sys.time())
        write.dcf(mapEntry, file = mapFile, append = TRUE, width = 2000)
        cat("\n", file = mapFile, append = TRUE)
        options
}
