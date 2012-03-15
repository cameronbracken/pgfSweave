\name{pgfSweaveSetup}
\alias{pgfSweaveDriver}
\alias{pgfSweaveSetup}
\title{The Sweave driver for pgfSweave}
\usage{
  pgfSweaveSetup(file, syntax, output = NULL,
    quiet = FALSE, debug = FALSE, echo = TRUE, eval = TRUE,
    split = FALSE, stylepath = TRUE, pdf = FALSE,
    eps = FALSE, cache = FALSE, pgf = FALSE, tikz = TRUE,
    external = FALSE, sanitize = FALSE, highlight = TRUE,
    tidy = FALSE, concordance = FALSE, figs.only = TRUE)
}
\arguments{
  \item{file}{A connection or a character string giving the
  name of the Sweave file to load.}

  \item{syntax}{See \code{\link{RweaveLatex}}}

  \item{output}{See \code{\link{RweaveLatex}}}

  \item{quiet}{See \code{\link{RweaveLatex}}}

  \item{debug}{See \code{\link{RweaveLatex}}}

  \item{echo}{See \code{\link{RweaveLatex}}}

  \item{eval}{See \code{\link{RweaveLatex}}}

  \item{split}{See \code{\link{RweaveLatex}}}

  \item{stylepath}{See \code{\link{RweaveLatex}}}

  \item{pdf}{See \code{\link{RweaveLatex}}. Note the
  default is changed to \code{FALSE}}

  \item{eps}{See \code{\link{RweaveLatex}}. Note the
  default is changed to \code{FALSE}}

  \item{cache}{See
  \code{\link[cacheSweave]{cacheSweaveDriver}}}

  \item{tikz}{Set default for option \code{tikz}, see
  details below.}

  \item{pgf}{Set default for option \code{pgf}, see details
  below.}

  \item{external}{Set default for option \code{external},
  see details below.}

  \item{sanitize}{Set default for option \code{sanitize},
  see details below.}

  \item{highlight}{Should echo'd code be highlighted with
  the highlight package.}

  \item{tidy}{Should echo'd code be cleaned up with the
  \code{\link[formatR]{tidy.source}} function from the
  \pkg{formatR} package.}

  \item{concordance}{\code{\link{RweaveLatex}}}

  \item{figs.only}{\code{\link{RweaveLatex}}}
}
\value{
  Nothing useful returned.
}
\description{
  An Sweave driver for caching code chunks and image files
  making for speedy compilation.
}
\details{
  To only compile an Rnw to a tex file,
  \code{pgfSweaveDriver} should be used as the
  \code{driver} option to \code{\link{Sweave}}.  Otherwise
  use the \code{\link[pgfSweave]{pgfSweave}} function for a
  simple way to compile to pdf.
}
\note{
  \itemize{ \item For myfile.Rnw, Make sure to call the
  command \\code{pgfrealjobname{myfile}} in the LaTeX
  header.  \item Calling \code{\link[pgfSweave]{pgfSweave}}
  with \code{compile.tex=FALSE} is equivalent to directly
  calling Sweave.  \item To obtain the speedup from using
  pgf external graphics be sure to set \code{external=TRUE}
  on all code chunks which generate a plot. The default
  code options for the driver are \code{pdf=FALSE},
  \code{eps=FALSE}, \code{tikz=TRUE}, \code{pgf=FALSE},
  \code{external=FALSE}.  }
}
\author{
  Cameron Bracken \email{cameron.bracken@gmail.com} and
  Charlie Sharpsteen
}
\references{
  Sweave manual:
  \url{http://www.statistik.lmu.de/~leisch/Sweave/Sweave-manual.pdf}

  cacheSweave vignette:
  \url{http://cran.r-project.org/web/packages/cacheSweave/vignettes/cacheSweave.pdf}
}
\seealso{
  \code{\link[pgfSweave]{pgfSweave}},
  \code{\link[pgfSweave]{pgfSweaveDriver}},
  \code{\link[cacheSweave]{cacheSweave}},
  \code{\link{Sweave}},
  \code{\link[tikzDevice]{tikzDevice}}
  \code{\link[highlight]{highlight}}
}

