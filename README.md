#pgfSweave README
##ABOUT

pgfSweave provides a number of improvements to the improve 
speed and quality of the Sweave output including: (1) capabilities for 
'caching' graphics generated with Sweave on top of the caching 
mechanisms provided by cacheSweave, (2) an interface to the tikzDevice 
package which provides graphics with consistent font style, sizing 
and quality as the main document and (3) Automatic highlighting of 
echo'd source code via the highlight package. pgfSweave provides a new 
driver for Sweave (pgfSweaveDriver) with new chunk options tikz, pgf, 
external, sanitize and highlight on top of the cache option provided by 
cacheSweave.


##TODO
 
* Add options to access the noae and nogin options in Sweave.sty.  The 
  logical thing for pgfSweave is to set nogin by default i.e. 
  `\usepackage[nogin]{Sweave}`.

* check for `\pgfrealjobname`


##BUGS
* If java does not exist the compilation is reported as successful though 
  the pgf file did not properly get created when using eps2pgf. 
 
* A stray Rplots.pdf gets generated using caching. Not sure if this a 
  cacheSweave or Sweave or a pgfSweave problem.  When using a gui 
  interface a plotting window opens up.


##ISSUES:

* `external=T` only works if a single plot is generated with a single code 
  chunk.  This is an issue with Sweave as well. The offered fix in Sweave 
  is to `cat()` your own includegraphics commands.
 
* Not all latex commands work in labels for example `\scshape` works but 
  `\textsc{}` does not
 
* Changing the width and height of a figure ONLY but not the code chunk 
  does not register as a change and thus the code chunk will not 
  recompile.


##POSSIBLE NEW FEATURES

* chunk dependency checking like weaver using codetools. 

* Scrap Sweave.sty completely (since it is so short) just copy the few 
  lines of LaTeX defining the Sweave environments into the tex file. 
 
* Nicer LaTeX number formatting.

* Follow `\input` and `\include` statements. 

* Automatically add the `\pgfrealjobname` command if it is not already in 
  the file like the Sweave style file is added when `stylepath=TRUE`.
 
* When a pgf/TikZ graphic is included manually (not in a code chunk) add 
  the commands to externalize into the graphics into the shell script that 
  is created. This may need to be a separate function that gives the tex 
  file a once over and checks if the pdf file exists corresponding to the 
  name in `\beginpgfgraphicnamed{name}` or the corresponding TikZ 
  externalization command. There are already scripts available on example 
  to do stuff like this so maybe integration of one of those may be 
  possible. 
 
* Add support for TikZ externalization.  TikZ externalization library is
  more elegant and could provide more advanced options.  For example the 
  option to create a make file would be really awesome since it would take 
  care of most of the checking for graphics changes on the part of 
  pgfSweave.  This would be a substantial change, requring alot of 
  rewriting of internal code. 


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
