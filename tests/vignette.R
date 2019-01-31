## Re-build the vignette if an environmental variable containing the path to the data is set:

# eg:  
# export DONKEY_DOSING_DATA="/Users/matthewdenwood/Documents/Research/Projects/Donkey Sanctuary 2/Final meeting/Deliverables/"
# Sys.setenv(DONKEY_DOSING_DATA="/Users/matthewdenwood/Documents/Research/Projects/Donkey Sanctuary 2/Final meeting/Deliverables/")
wd <- Sys.getenv('DONKEY_DOSING_DATA')
if(!identical(wd, '')){
	if(!all(c("data_2013_2017.Rdata", "hello") %in% list.files(wd)))
		stop('One or more data file required to build the vignette are missing from the provided DONKEY_DOSING_DATA directory (', wd, ')')

	# Re-build the vignette from ../inst/vignette_source and copy the .html to the DONKEY_DOSING_DATA directory

}

