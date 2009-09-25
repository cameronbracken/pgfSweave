.onLoad <- function(lib, pkg) {
        pkgList <- c("filehash", "stashR", "cacheSweave", "tikzDevice")
        
        for(pkg in pkgList) {
                status <- suppressMessages({
                        require(pkg, quietly = TRUE, character.only = TRUE)
                })
                if(!status)
                        stop(gettextf("'%s' package required", pkg))
        }
}

.onAttach <- function(lib, pkg) {
        if(!require(utils))
                stop("'utils' package required to use 'Sweave'")
}
