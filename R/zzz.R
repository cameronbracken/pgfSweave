.onLoad <- function(lib, pkg) {
        pkgList <- c("filehash", "stashR", "cacheSweave", 'rJava')
        
        for(pkg in pkgList) {
                status <- suppressMessages({
                        require(pkg, quietly = TRUE, character.only = TRUE)
                })
                if(!status)
                        stop(gettextf("'%s' package required", pkg))
        }
        .jpackage(pkg, morePaths = 'eps2pgf')
}

.onAttach <- function(lib, pkg) {
        if(!require(utils))
                stop("'utils' package required to use 'Sweave'")
}
