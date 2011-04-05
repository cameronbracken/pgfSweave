
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

removeLineJunk <- function(chunk){
  lines <- grep("#line", chunk)
  srclines <- attr(chunk, "srclines")
  chunk <- chunk[-lines]
  attr(chunk, "srclines") <- srclines[-lines]
  chunk
}

hasChunkChanged <- function(chunk,chunkprefix,options){
  
     # caching may not be used but use the caching directory anyway
    cachedir <- getCacheDir()
    chunkDigest <- digest(list(chunk[1:length(chunk)],options$width,options$height))
    dbName <- makeChunkDatabaseName(cachedir, options, chunkDigest)
    
      # Create 'stashR' database if it does not exist
    db <- new("localDB", dir = dbName, name = basename(dbName))
    mangledPrefix <- paste('__',chunkprefix,'__',sep='')

      # If the digest of the chunk does not exist, 
      # then stick it in the db otherwise, get it out and 
      # check it agains the current one

    if( !dbExists(db, mangledPrefix) ) {

      dbInsert(db,mangledPrefix,chunkDigest)
      chunkChanged <- TRUE

    } else {

      oldChunkDigest <- dbFetch(db, mangledPrefix)
      chunkChanged <- ifelse( oldChunkDigest == chunkDigest, FALSE, TRUE)
    }
    chunkChanged
}