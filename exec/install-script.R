#!/usr/bin/env Rscript

options(warn=-1)
R_HOME <- Sys.getenv('R_HOME')
R_HOME <- ifelse(length(R_HOME) == 0, R.home(), R_HOME)
bindir <- R.home("bin")
bin_script <- file.path(bindir,'pgfsweave')
x <- file.remove(bin_script)

rel_script <- "../exec/pgfsweave-script.R"
abs_script <- file.path(Sys.getenv('R_PACKAGE_DIR'),
	'exec','pgfsweave-script.R')

script <- readLines(rel_script)
win_shebang <- paste("#!",file.path(bindir,'Rscript'),sep='')
win_script <- c( win_shebang, script)

success <- FALSE

if( .Platform$OS.type == 'windows' ){
	
	tf <- tempfile()
	writeLines( win_script, tf )
	if(file.copy(tf,bin_script))
		success <- TRUE
	
}else{
	if(file.symlink(abs_script,bin_script))
		success <- TRUE
		
}

if(success){
	
	cat('\n***********************\n')
	cat( 'Installing custom script in:\n\n')
	cat( R.home(),'/bin/\n\n',sep='')
	cat( 'For usage instructions:\n\n')
	cat( 'R CMD pgfsweave --help\n')
	if(.Platform$OS.type == 'windows'){
		cat( '\nNote for windows users:\n')
		cat( '  You must have Rtools installed and have\n')
		cat( '  ',R.home(),'/bin in your PATH.\n',sep='')
	}
	cat('***********************\n\n')
	
}else{
	
	cat('\n***********************\n')
	cat('Failed to install custom pgfsweave script:\n')
	cat('  Thats ok! You can manually install it later!\n')
	cat('***********************\n')
	
}
	

#R_HOME=`Rscript -e "cat(R.home())"`