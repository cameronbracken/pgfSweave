\name{pgfSweave}
\alias{pgfSweave}
\alias{pgfSweave-package}

\title{Extending the functionality of cacheSweave to include quality speedy graphics compilation}

\description{
The package \pkg{cacheSweave} provides a convenient way for speeding up compilation of \code{Sweave} documents which include computationally intensive code chunks.  \pkg{pgfSweave} provides a new driver for \pkg{Sweave}, \code{pgfSweaveDriver()} and new chunk options \code{pgf} and \code{external} on top of the \code{cache} option provided by \pkg{cacheSweave}.  The \code{pgfSweave()} function is provided to simplify the extra compliation steps needed in the \pkg{pgfSweave} process.
}

\usage{
pgfSweave(file, compile.tex = TRUE, syntax = getOption("SweaveSyntax"), ...)
}

\arguments{ 
    \item{file}{A connection or a character string giving the name of the file to load.} 
    \item{compile.tex}{If \code{TRUE} run \code{LaTeX}/\code{pdfLaTeX} on the resulting tex file. If \code{FALSE} only run \code{Sweave}.}
    \item{syntax}{An object of class SweaveSyntax or a character string with its name. The default installation provides SweaveSyntaxNoweb and SweaveSyntaxLatex (passed to Sweave).}
    \item{\dots}{Other options to be passed to \code{\link{texi2dvi}}} 
}

\details{
This package provides new features beyond \code{\link[cacheSweave]{cacheSweave}}:  
	\enumerate{
		\item better recognition of code chunk changes 
		\item the 'caching' of code chunks which generate graphics and 
		\item the generation of graphics in which the font matches that of the main document. This functionality is achieved using the eps2pgf utility and the external graphics feature in the TeX package pgf.
}

The process carried out by \code{pgfSweave} involves:
\itemize{
	\item Running \code{Sweave} on the .Rnw file using \code{pgfSweaveDriver} function.
	\item Generating a pgf file from each newly created or changed graphic using the eps2pgf utility.  
	\item Running the pgf externalization commands for each newly generated graphic.
	\item Finally running \code{LaTeX} with on the .tex file with using the real job name.
}

For more details see the \pkg{pgfSweave} package vignette.
}

\value{
A pdf file is generated if \code{compile.tex=TRUE} and a tex file only is generated otherwise. 
}

\references{
Sweave manual: \url{http://www.statistik.lmu.de/~leisch/Sweave/Sweave-manual.pdf}  

cacheSweave vignette: \url{http://cran.r-project.org/web/packages/cacheSweave/vignettes/cacheSweave.pdf}

pgf manual externalization section: \url{http://sourceforge.net/projects/pgf/}
}


\note{ 
	\itemize{
		\item Assuming \code{compile.tex=TRUE} the function will issue any system commands needed to ``externalize'' the pgf graphics.  To take advantage of the speedup, set \code{external=TRUE} on every code chunk that creates a plot.  
		\item the command \code{\pgfrealjobname{myfile}} MUST in the header of your \code{LaTeX} file for the external graphics capablities.
}
}

\section{Known issues}{
	\enumerate{
		\item Adding white space at the end of a code chunk causes all of the following code chunks to be registered as changed
		\item changing the width and height of a figure only does not register as a change
		\item A stray Rplots.pdf gets generated when using caching.
	}
}
\author{Cameron Bracken \email{cameron.bracken@gmail.com} and Charlie Sharpsteen }

\seealso{
\code{\link{Sweave}}, 
\code{\link[cacheSweave]{cacheSweave}}
}

\examples{
\dontrun{vignette("pgfSweave")}

library(pgfSweave)
oldcwd <- getwd()
dir <- tempdir()
setwd(dir)
file.copy(system.file("example","pgfSweave-example.Rnw", package = "pgfSweave"),dir)
file <- file.path(dir,"pgfSweave-example.Rnw")
pgfSweave(file,compile.tex=FALSE)
setwd(oldcwd)

\dontrun{
## Normally to compile to PDF by  
#    pgfSweave("pgfSweave-example.Rnw",pdf=T) 
## which calls texi2dvi 
#ifdef unix
## NOTE: LaTeX may not be included in your path by default, please see the FAQ in the pgfSweave manual if you latex or pdflatex are not found when using the R GUI.
#endif
#ifdef windows
## NOTE: does not work currently in R.exe 
#endif
}

## Use outside of R with the script provided in the pgfSweave exec/ directory
}
\keyword{utilities}
\concept{Reproducible research}
\concept{Tangle and Weave}
\concept{Literate programming}
