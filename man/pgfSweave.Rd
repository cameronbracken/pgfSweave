\name{pgfSweave}
\alias{pgfSweave}
\alias{pgfSweave-package}

\title{Extending the functionality of cacheSweave to include quality speedy graphics compilation.}

\description{
The package \pkg{cacheSweave} provides a convenient way for speeding up compilation of \code{Sweave} documents which include computationally intensive code chunks.  \pkg{pgfSweave} provides a new driver for \pkg{Sweave}, \code{pgfSweaveDriver()} and new chunk options \code{pgf} and \code{external} on top of the \code{cache} option provided by \pkg{cacheSweave}.

This package provides new features beyond \link{cacheSweave}:  
\item better recognition of code chunk changes 
\item the 'caching' of code chunks which generate graphics and 
\item the generation of graphics in which the font matches that of the main document. This functionality is achieved using the eps2pgf utility and the external graphics feature in the TeX package pgf.

The \code{pgfSweave()} function is provided to simplify the extra compliation steps needed in the \pkg{pgfSweave} process.
}
\usage{
pgfSweave(file, compile.tex=TRUE, syntax = getOption("SweaveSyntax"), ...)
}

\arguments{ 
    \item{file}{A connection or a character string giving the name of the file to load.} 
    \item{compile.tex}{If \code{TRUE} run \code{LaTeX}/\code{pdfLaTeX} on the resulting tex file. If \code{FALSE} only run \code{Sweave}.}
    \item{syntax}{An object of class SweaveSyntax or a character string with its name. The default installation provides SweaveSyntaxNoweb and SweaveSyntaxLatex (passed to Sweave).}
    \item{...}{Other options to be passed to \code{\link{texi2dvi}}} 
}
 
\details{
The process carried out by \code{pgfSweave} involves:
\item Running \code{Sweave} on the .Rnw file using \code{pgfSweaveDriver} function.
\item Generating a pgf file from each newly created or changed graphic using the eps2pgf utility.  
\item Running the pgf externalization commands for each newly generated graphic.
\item Finally running \code{LaTeX} with on the .tex file with using the real job name.

For more details see the \code{pgfSweave} package vignette.
}

\value{
A pdf file is generated if \code{compile.tex=TRUE} and a tex file only is generated otherwise. 
}

\references{ Yo mamma}

\author{Cameron Bracken and Charlie Sharpsteen}

\note{ 
\item Assuming \code{compile.tex=TRUE} the function will issue any system commands needed to ``externalize'' the pgf graphics.  To take advantage of the speedup, set \code{external=TRUE} on every code chunk that creates a plot.  
\item the command \code{pgfrealjobname} MUST in the header of your \code{LaTeX} file for the external graphics capablities.

Known issues:

\item Adding white space at the end of a code chunk causes all of the following code chunks to be registered as changed
\item changing the width and height of a figure only does not register as a change
\item A stray Rplots.pdf gets generated when \code{external=TRUE}
}

\seealso{\link{Sweave}, \link{cacheSweave}}

\examples{
file <- system.file("example","pgfSweave-example.Rnw", package = "pgfSweave")

## Normally to compile to PDF by  
##    pgfSweave("pgfSweave-example.Rnw",pdf=T) 
## which calls texi2dvi though does not work in R.app
## Use outside of R with the script provided in the pgfSweave vignette or by
## R CMD pgfSweave pgfSweave-example.Rnw
pgfSweave(file,compile.tex=FALSE)
}
