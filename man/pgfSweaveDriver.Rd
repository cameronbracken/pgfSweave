\name{pgfSweaveDriver}
\alias{pgfSweaveDriver}
\title{The Sweave driver for pgfSweave.}

\description{
An Sweave driver for caching code chunks and image files making for speedy compilation.
}
\usage{
Sweave(file,driver=pgfSweaveDriver)
pgfSweave(file,...)
}
\arguments{
    \item{file}{A connection or a character string giving the name of the Sweave file to load.}
    \item{driver}{The Sweave driver from the pgfSweave package.}
    \item{...}{Other options to be passed to texi2dvi}
}
\details{
To only compile an Rnw to a a tex file, Sweave can be called directly.  Otherwise use the \code{\link{pgfSweave}} function to compile to pdf.
}
\value{
  If Sweave is used a tex file is generated. If pgfSweave is used a pdf file is generated.
}
\references{Sweave manual and cacheSweave vignette}
\author{ Cameron Bracken and Charlie Sharpsteen }

\note{ 
\item For myfile.Rnw, Make sure to call the command \code{\pgfrealjobname{myfile}} in the \LaTeX{} header.
\item Calling \code{\link{pgfSweave}} with \code{compile.tex=FALSE} is equivalent to directly calling Sweave.  
\item To obtain the speedup from using pgf external graphics be sure to set \code{external=TRUE} on all code chunks which generate a plot. The default code options for the driver are \code{pdf=FALSE,eps=FALSE,pgf=TRUE,external=FALSE}.
}

\seealso{\code{\link{pgfSweave}},\code{\link{cacheSweave}},\code{\link{Sweave}}}
