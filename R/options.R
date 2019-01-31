#' @title Options for the donkey dosing model
#' @name dosing_options
#' @aliases dosing_options dosing_getOption dosing_setOption

#' @description
#' These functions allow global options to be get and set

#' @param ... a named list of options to set
#' @param name the name of the option to retrieve

#' @export
#' @rdname dosing_options
dosing_setOption <- function(...){
	opts <- list(...)

	if(length(opts)>0){
		options <- dosingprivate$options
		recognised <- pmatch(names(opts), names(options))
		if(any(is.na(recognised))){
			warning(paste("Igoring unmatched or ambiguous option(s): ", paste(names(opts)[is.na(recognised)],collapse=", ")))
      opts <- opts[!is.na(recognised)]
		}
		optnames <- names(options)[recognised[!is.na(recognised)]]
		if(length(optnames)>0) for(i in 1:length(optnames)){
			options[optnames[i]] <- opts[[i]]
		}
		assign("options",options,envir=dosingprivate)
	}
	
	invisible(dosingprivate$options)
}

#' @export
#' @rdname dosing_options
dosing_getOption <- function(name){
	if(length(name)!=1) stop("Only 1 option can be retrieved at a time")
	opt <- pmatch(name,names(dosingprivate$options))
	if(is.na(opt)) stop(paste("Unmatched or ambiguous option '", name, "'", sep=""))
	# Use eval as some defaults are put in using 'expression' to avoid evaluating at load time:
	return(eval(dosingprivate$options[[opt]]))
}

#' @export
#' @rdname dosing_options
dosing_options <- function(){
	return(dosingprivate$options)
}



dosingprivate <- new.env()

# Use 'expression' for functions to avoid having to evaluate before the package is fully loaded:
assign("defaultoptions", list(
	allsexes = c(DKF='DKF',DKM='DKM',DKG='DKM',DG='DKM',DKS='DKM','DK(RIG)'='DKM',PYF='MUPY',MUF='MUPY',PYG='MUPY',MUG='MUPY',MUS='MUPY',
				POG='MUPY',POF='MUPY',HOF='MUPY',HYF='MUPY',HYG='MUPY',HYS='MUPY',HOG='MUPY',MUPY='MUPY'),
	anthelmintics = c('MOX','FBZ','IVM','PYR'),
	min_N=10, expected_efficacy=0.95, min_week=12, max_week=48	
	), envir=dosingprivate)

assign("options",dosingprivate$defaultoptions,envir=dosingprivate)

