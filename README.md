#pgfSweave README
##ABOUT

pgfSweave provides a number of improvements to the
speed and quality of Sweave output including: (1) capabilities for 
'caching' graphics generated with Sweave on top of the caching 
mechanisms provided by cacheSweave, (2) an interface to the tikzDevice 
package which provides graphics with consistent font style, sizing 
and quality as the main document and (3) Automatic highlighting of 
echo'd source code via the highlight package. pgfSweave provides a new 
driver for Sweave (pgfSweaveDriver) with new chunk options tikz, 
external, sanitize and highlight on top of the cache option provided by 
cacheSweave.

Main development of pgfSweave takes place on the Github site [https://github.com/cameronbracken/pgfSweave]().  Precompiled packages can be found on the R-Forge site, [http://r-forge.r-project.org/projects/pgfsweave/]()

__Please read the package vignette `inst/doc/pgfSweave.pdf` for more information on installation and usage.__

Also see [http://code.cjb.net/pgfSweave.html](http://code.cjb.net/pgfSweave.html) for a great example.


##BUGS AND FEATURE REQUESTS
* See [https://github.com/cameronbracken/pgfSweave/issues]()