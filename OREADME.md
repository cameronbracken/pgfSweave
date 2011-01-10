## RESOLVED (resolution in brackets)
* latex/pdflatex is not found when using R.app (Mac OS X) unless path to 
  pdflatex is hardcoded (but pgfSweave <file> works)
  [put path to latex distro in .Rprofile (see vignette faq)]
 
* Package currently generates a bash shell script to compile external 
  graphics, which will not work in windows.
  [This actually does work on windows if Rtools and MiKTeX are installed]
 
* Using the 'pdf' option and 'external' option will conflict suggest using 
  one or the other or possibly have a default when this is the case
  [If pgf is TRUE it supercedes everything else]
 
* output pdf size when included in document is refined by textwidth by 
  default, need a feature to add the 'width' and 'height' and possibly and 
  other option 'scale' the `\includegraphics[width=,height=,scale=]{file}`
  [Use `\usepackage[nogin]{Sweave}`]
 
* Vignette does not compile with R CMD {check,build,install} because file 
  is not normal Sweave (Solution for the final document may just be to 
  save images separately and create a standard Sweave file). For now 
  vignette moved to `inst/doc/src`.
  [Elaborate make file created in `inst/misc/vignette-source`, run make then 
  make cleanforbuild before uploading]


##DONE (old TODO items):

* pgf graphics device.
  [Started tikzDevice on r-forge]

* Figure out how to fill the environment variable $R_HOME in 
  exec/pgfSweave
  [Irrelevant now, see next item]

* Figure out how to call the script exec/pgfSweave when using R CMD 
  pgfSweave
  [Used a Makevars file to create symlink in $RHOME/bin, basically 
  a much simplified version of Charlie's method]
 
* Documentation has a boatload of warnings under R CMD check.

* Figure out how to call eps2pgf.jar with the rJava package 
  [scrapped for now, write a pgf graphics device!]
 
* Determine the limitations R CMD pgfSweave.
  [shell script pgfSweave in exec/ will do the same but using R CMD is not 
  possible withoud a rewrite of the Rcmd script]
 
* Determine if pgf 2.00 contains the external capabilities so that can be 
  stated in the vignette and the documentation.
  [must use at least pgf 2.00]
