format_coefficients <- function(gpdata, fecdata, models_allyears, models_nolastyear, comparison, expected_efficacy, pmcoefs, threshold){

	## Some checks:
	stopifnot(is.numeric(threshold) && length(threshold)==1 && threshold > 0 && threshold < 1)
	stopifnot(is.data.frame(pmcoefs))
	
	stopifnot(all(c('Farm','Hygiene') %in% names(gpdata)))
	stopifnot(all(c('Sex','AgeCat','LastDoseCat') %in% names(fecdata)))


	if(FALSE){
		summary(models_allyears$group)
		summary(models_allyears$common)
		summary(models_allyears$common_mixed)
		summary(models_allyears$residual_4)
		summary(models_allyears$residual_8)
		summary(models_allyears$residual_12)
		summary(models_allyears$ind_0)
		summary(models_allyears$ind_4)
		summary(models_allyears$ind_8)
		summary(models_allyears$ind_12)
		models_allyears$residual_sd
		models_allyears$all_coefs

		models_allyears$convergence

		models_allyears$plots$combinedplot1
		models_allyears$plots$combinedplot2

		models_allyears$plots$exclusionplot3


		summary(models_nolastyear$group)
		summary(models_nolastyear$common)
		summary(models_nolastyear$common_mixed)
		summary(models_nolastyear$residual_4)
		summary(models_nolastyear$residual_8)
		summary(models_nolastyear$residual_12)
		summary(models_nolastyear$ind_0)
		summary(models_nolastyear$ind_4)
		summary(models_nolastyear$ind_8)
		summary(models_nolastyear$ind_12)
		models_nolastyear$residual_sd
		models_nolastyear$all_coefs

		models_nolastyear$plots$combinedplot1
		models_nolastyear$plots$combinedplot2

		cbind(models_allyears$all_coefs, models_nolastyear$all_coefs)


	#	pdf('prediction_comparisons.pdf', width=12, height=8)
		print(models_allyears$plots$combinedplot1)
		print(models_nolastyear$plots$combinedplot1)
		print(models_allyears$plots$combinedplot2)
		print(models_nolastyear$plots$combinedplot2)
	#	dev.off()
	}


	# Write coefficients for Excel:
	models <- models_allyears
	
	weathernames <- unlist(lapply(c(4,8,12,16,20), function(wk) gsub('WK',wk,c('Temp_frost_WK', 'I(Temp_frost_WK^2/10)', 'Temp_avg_WK', 'I(Temp_avg_WK^2/10)', 'AbsHumid_WK', 'I(AbsHumid_WK^2/10)', 'RelHumid_WK', 'I(RelHumid_WK^2/100)'))))
	groupcoefnames <- c('(Intercept)', 'sdoy', 'cdoy', paste0('Farm', levels(gpdata$Farm)), paste0('Hygiene', levels(gpdata$Hygiene)), 'MeanAge', 'log(TotalAnimals)', paste0('DoseProp',c(4,8,12)), weathernames)
	groupcoefs <- rep(0,length(groupcoefnames))
	names(groupcoefs) <- groupcoefnames
	groupcoefs[names(fixef(models$group))] <- fixef(models$group)
	stopifnot(all(names(groupcoefs)==groupcoefnames))
	groupcoefnames[1] <- "GroupIntercept"

	commoncoefnames <- c('(Intercept)', paste0('Sex', levels(fecdata$Sex)), paste0('AgeCat', levels(fecdata$AgeCat)), paste0('LastDoseCat', levels(fecdata$LastDoseCat)))
	commoncoefs <- rep(0,length(commoncoefnames))
	names(commoncoefs) <- commoncoefnames
	commoncoefs[names(fixef(models$common))] <- fixef(models$common)
	stopifnot(all(names(commoncoefs)== commoncoefnames))
	commoncoefnames[1] <- "CommonIntercept"

	residualcoefs <- c(coef(models$residual_4), coef(models$residual_8), coef(models$residual_12))
	indcoefs <- c(coef(models$ind_0), coef(models$ind_4), coef(models$ind_8), coef(models$ind_12))
	names(indcoefs)[2:7] <- c("fec4wk", "lastYearMeanFEC_4wk", "fec8wk", "lastYearMeanFEC_8wk", "fec12wk", "lastYearMeanFEC_12wk")

	# Constant values:
	constants <- c(expected_efficacy=expected_efficacy, models$centering)

	output <- cbind('Group Coefficients:', '')
	output <- rbind(output, cbind('',''))
	output <- rbind(output, cbind(groupcoefnames,groupcoefs))
	output <- rbind(output, cbind('',''))
	output <- rbind(output, cbind('Common Coefficients:', ''))
	output <- rbind(output, cbind('',''))
	output <- rbind(output, cbind(commoncoefnames, commoncoefs))
	output <- rbind(output, cbind("ResidualSD", models$residual_sd))
	output <- rbind(output, cbind('',''))
	output <- rbind(output, cbind('Residual Coefficients:', ''))
	output <- rbind(output, cbind('',''))
	output <- rbind(output, cbind(names(residualcoefs), residualcoefs))
	output <- rbind(output, cbind('',''))
	output <- rbind(output, cbind('Individual Coefficients:', ''))
	output <- rbind(output, cbind('',''))
	output <- rbind(output, cbind(names(indcoefs), indcoefs))
	output <- rbind(output, cbind('',''))
	output <- rbind(output, cbind('Constants:', ''))
	output <- rbind(output, cbind('',''))
	output <- rbind(output, cbind(names(constants), constants))
	output <- rbind(output, cbind('',''))
	output <- rbind(output, cbind('PM-based risk:', ''))
	output <- rbind(output, cbind('',''))
	output <- rbind(output, cbind(pmcoefs$term, pmcoefs$estimate))
	output <- rbind(output, cbind('PM_threshold',threshold))
	output <- rbind(output, cbind('',''))


	rownames(output) <- NULL
	output <- as.data.frame(output)
	output[,2] <- as.numeric(output[,2])

	return(output)
	# TODO:
	# copy and clean up these two models removing unnecessary code, wrap as 'train' function - DONE
	# write 'predict' function that takes an excel spreadsheet and week #, reads it, makes predictions + dose sheet
	# use this to optimise (threshold is always 2000 as that is the group threshold)
	# copy to Excel and test to make sure it's the same
	# look at predictions per animal over time for a real summer

}
