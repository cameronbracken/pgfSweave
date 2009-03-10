\name{pgfSweaveDriver}
\alias{pgfSweaveDriver}
\alias{pgfSweaveSetup}
\title{The Sweave driver for pgfSweave}

\description{
An Sweave driver for caching code chunks and image files making for speedy compilation. 
}
\usage{
pgfSweaveDriver()

pgfSweaveSetup(file, syntax,
               output = NULL, quiet = FALSE, debug = FALSE, echo = TRUE,
               eval = TRUE, split = FALSE, stylepath = TRUE, pdf = FALSE,
               eps = FALSE, cache = FALSE, pgf = TRUE, external=FALSE,
               tex.driver = "pdflatex")
}
\arguments{
    \item{file}{A connection or a character string giving the name of the Sweave file to load.}
    \item{syntax}{See \code{\link{RweaveLatex}}}
    \item{output}{See \code{\link{RweaveLatex}}}
    \item{quiet}{See \code{\link{RweaveLatex}}}
    \item{debug}{See \code{\link{RweaveLatex}}}
    \item{echo}{See \code{\link{RweaveLatex}}}
    \item{eval}{See \code{\link{RweaveLatex}}}
    \item{split}{See \code{\link{RweaveLatex}}}
    \item{stylepath}{See \code{\link{RweaveLatex}}}
    \item{pdf}{See \code{\link{RweaveLatex}}. Note the default is changed to \code{FALSE}}
    \item{eps}{See \code{\link{RweaveLatex}}. Note the default is changed to \code{FALSE}}
    \item{cache}{See \code{\link[cacheSweavecacheSweaveDriver]{cacheSweaveDriver}}}
    \item{pgf}{Set default for option \code{pgf}, see details below.}
    \item{external}{Set default for option \code{external}, see details below.}
    \item{tex.driver}{Set default for option \code{tex.driver}, see details below.}
}
\details{
To only compile an Rnw to a tex file, \code{pgfSweaveDriver} should be used as the \code{driver} option to \code{\link{Sweave}}.  Otherwise use the \code{\link[pgfSweave]{pgfSweave}} function for a simple way to compile to pdf.
}
\section{New options}{
    This function provides new options for use in code chunks 
\describe{
	
	\item{external}{ Use the graphics externalization feature of the TeX package pgf.  Once a graphic is created, a code chunk is not evaluated again until changes are made.  Essentially the graphic is ``cached'' (defaults to \code{FALSE}).}
	
	\item{\code{pgf}}{ Generate an eps file and use the eps2pgf utility to generate a pgf graphic for inclusion with \code{\input{graphic.pgf}} thi takes the place of an \code{\includegraphics} statement. The \code{-m directcopy} option is used so that LaTeX code can be directly used in titles (see the eps2pgf manual, \url{http://sourceforge.net/projects/eps2pgf/}).  This also causes the font in the graphic to match that of the main document. (defaults to \code{TRUE})}
	
	\item{\code{tex.driver}}{ A character string giving the LaTeX driver to use for compiling external graphics (defaults to ``pdflatex'') }
	
}
	All the \code{\link{Sweave}} and \code{\link[cacheSweave]{cacheSweave}} options are available. Note that the defaults for the \code{\link{Sweave}} options \code{pdf} and \code{eps} are set to \code{FALSE} unlike \code{\link{Sweave}} and \code{\link[cacheSweave]{cacheSweave}}.
}
 

\value{
Nothing useful returned.
}

\references{
Sweave manual: \url{http://www.statistik.lmu.de/~leisch/Sweave/Sweave-manual.pdf}  

cacheSweave vignette: \url{http://cran.r-project.org/web/packages/cacheSweave/vignettes/cacheSweave.pdf}
}

\author{ Cameron Bracken \email{cameron.bracken@gmail.com} and Charlie Sharpsteen }

\note{ 
    \itemize{
    	\item For myfile.Rnw, Make sure to call the command \code{\pgfrealjobname{myfile}} in the \LaTeX{} header.
    	\item Calling \code{\link[pgfSweave]{pgfSweave}} with \code{compile.tex=FALSE} is equivalent to directly calling Sweave.  
    	\item To obtain the speedup from using pgf external graphics be sure to set \code{external=TRUE} on all code chunks which generate a plot. The default code options for the driver are \code{pdf=FALSE}, \code{eps=FALSE}, \code{pgf=TRUE}, \code{external=FALSE}.
    }
}


\seealso{
\code{\link[pgfSweave]{pgfSweave}},  
\code{\link[pgfSweaveDriver]{pgfSweaveDriver}}, 
\code{\link[cacheSweave]{cacheSweave}}, 
\code{\link{Sweave}}}
