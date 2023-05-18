#' @title Data import, model running, and data/dosing sheet export for the Donkey Dosing software
#' @name DonkeyDosing
#' @aliases DonkeyDosing donkeydosing

#' @description
#' This function is used to import one or more FEC and weather data files, process the data into the necessary format, and then run the training model to generated updated coefficients for the predictive model used by the dosing support tool.  Facilities to save the processed data and updated coefficients etc are also provided.

#' @details
#' This class is written using an OOP-style of programming, so that the data can be accessed in different formats at any point using a single R object as the point of reference.  This minimises the possibilities for programming mistakes by the end user, but does result in code that may look a little strange to users who may be more used to the standard procedural style of R programming.  The best way to understand the code is using an example - see vignette('DonkeyDosing', package='DonkeyDosing') for an overview.

#' @return
#' An object of class DonkeyDosing.

#' @examples
#' \dontrun{
#' vignette('DonkeyDosing', package='DonkeyDosing')
#' }

#' @param year the year relevant to the locations or FEC data to be read (length 1 numeric)
#' @param excel_file path to an excel from which to read data (either locations, FEC or weather data, depending on the method).  Should have a .xlsx or .xls file extension (length 1 character).
#' @param excel_sheet the relevant sheet number/name within the excel file
#' @param year the farm relevant to the FEC data to be read (length 1 character)
#' @param cl the number of cores on which to run parallel computation (or a cluster created by makeCluster or similar)

#' @importFrom methods new
#' @importFrom pbapply pblapply
#' @importFrom readxl read_excel excel_sheets
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom tibble as_tibble
#' @importFrom stringr str_c str_replace_all
#' @importFrom grDevices dev.off pdf
#' @import tidyverse


#' @export DonkeyDosing
DonkeyDosing <- setRefClass('DonkeyDosing',
	fields = list(Locations='data.frame', Animal='data.frame', FEC='data.frame', Weather='data.frame', GroupModelData='data.frame', IndividualModelData='data.frame', RollingWeather='data.frame', prepared='list', run='logical', predictions='list', probabilities='list'),

	methods = list(

	initialize = function(reset=FALSE){
		"Set up a DonkeyDosing object before importing files"

		.self$Locations <- data.frame(Year=numeric(0), Farm=character(0), Field=character(0), Location=character(0), Hygiene=factor(character(0), levels=c('None','Automated','Manual')), Sensitivity=numeric(0))
		.self$Animal <- data.frame(Year=numeric(0), Farm=character(0), Location=character(0), Field=character(0), AnimalID=character(0), Tag=character(0), Sex=factor(character(0), levels=c('DKF','DKM','MUPY')), Age=numeric(0), AgeCat=factor(character(0), levels=c("Age_0-5", "Age_6-10", "Age_11-20", "Age_21+")), AvFEC=numeric(0), ClinicalRisk = factor(character(0), levels=c('Normal','High','VeryHigh')))
		.self$FEC <- data.frame(AnimalID=character(0), Location=character(0), Year=numeric(0), Week=numeric(0), FEC=numeric(0), Count=numeric(0), Dose=logical(0), DoseText=character(0), Monday=as.Date(character(0)), LastDose=numeric(0))
		.self$Weather <- data.frame(Date=as.Date(character(0)), Year=numeric(0), Month=numeric(0), Day=numeric(0), Temp_low=numeric(0), Temp_avg=numeric(0), Humidity_avg=numeric(0))

		.self$prepared <- list(ready=FALSE)
		.self$run <- FALSE
		.self$predictions <- list()
		.self$probabilities <- list(done=FALSE)

		if(reset) cat("Successfully reset the model container\n") else cat("Successfully created a model container\n")

	},

	Reset = function(){
		"Reset the DonkeyDosing object (clear all data)"

		.self$initialize(reset=TRUE)

	},

	AddLocations = function(year, excel_file){
		"Add farm location information for a given year"

		if(!is.numeric(year) || length(year)!=1 || year < 2000 || year > 3000)
			stop('The specified year argument must be numeric of length 1 (in the range 2000-3000)')

		if(!is.character(excel_file) || length(excel_file)!=1 || !(grepl('\\.xlsx$', excel_file) || grepl('\\.xls$', excel_file) ))
			stop('The specified excel_file argument must be a length 1 character string specifying a file with a .xlsx or .xls file extension')

	  excel_sheet <- "Locations"
	  if(!excel_sheet %in% excel_sheets(excel_file)) stop('The required sheet "Locations" is missing from the specified excel file')
		locations <- read_excel(excel_file, excel_sheet, .name_repair = "unique")
		if(! all(c('Farm','Location','Hygiene','Sensitivity') %in% names(locations)))
			stop('One or more of the required columns Farm, Location, Hygiene and Sensitivity was missing from the specified excel file/sheet')

		if(any(! locations$Hygiene %in% c('None','Automated','Manual')))
			stop('One or more unrecognised Hygiene entry was found - all cells must be either None, Automated or Manual')

		if(any(is.na(as.numeric(locations$Sensitivity))))
			stop('One or more entries under Sensitivity were not interpretable as a number')

		locations$Year <- as.character(year)
		locations$Hygiene <- factor(locations$Hygiene, levels=c('None','Automated','Manual'))

		locations$Field <- locations$Location
		locations$Location <- paste(locations$Farm,locations$Location,sep='_')

		newloc <- locations %>% select(.data$Year, .data$Farm, .data$Field, .data$Location, .data$Hygiene, .data$Sensitivity)
		stopifnot(all(names(newloc)==names(.self$Locations)))
		newloc <- rbind(.self$Locations, newloc)
		test <- newloc %>% group_by(.data$Farm, .data$Location, .data$Year) %>% tally %>% filter(n > 1)
		if(any(nrow(test) > 0)){
			stop('Duplicated farm - location - year combination: ', test$Farm[1], ' - ', test$Location[1], ' - ', test$Year[1])
		}

		newloc$Location <- gsub('[[:space:]]','',newloc$Location)
		.self$Locations <- as_tibble(newloc)

		cat("Succesfully added the following number of locations for ", year, ":\n", sep="")
		for(farm in unique(locations$Farm)){
			cat("\t", farm, ": ", sum(locations$Farm==farm), "\n", sep="")
		}

		# Adding locations does not break an existing run and is needed for predictions:
		# .self$prepared$ready <- FALSE
		# .self$run <- FALSE

	},

	AddDosingSheet = function(year, farm, excel_file, excel_sheet=farm){
		"Add animal-level FEC and dosing information for a given farm and year from an existing dosing spreadsheet"

		cat('Processing data for ', farm, ' (', year, ')...\n', sep='')

		all_locations <- .self$Locations
		rvals <- dosing_sheet(year=year, farm=farm, excel_file=excel_file, excel_sheet=excel_sheet, all_locations=all_locations)

		allanimals <- rvals$allanimals
		allfec <- rvals$allfec

		stopifnot(all(names(allanimals)==names(.self$Animal)))
		newani <- rbind(.self$Animal, allanimals)
		test <- newani %>% group_by(.data$Farm, .data$Location, .data$AnimalID, .data$Year) %>% tally %>% filter(n > 1)
		if(any(nrow(test) > 0)){
			stop('Duplicated farm - location - animalID - year combination: ', test$Farm[1], ' - ', test$Location[1], ' - ', test$AnimalID[1], ' - ', test$Year[1])
		}

		stopifnot(all(names(allfec)==names(.self$FEC)))
		newfec <- rbind(.self$FEC, allfec)
		test <- newfec %>% group_by(.data$Location, .data$AnimalID, .data$Monday) %>% tally %>% filter(n > 1)
		if(any(nrow(test) > 0)){
			stop('Duplicated location - animalID - week date combination: ', test$Location[1], ' - ', test$AnimalID[1], ' - ', strftime(test$Monday[1]))
		}

		cat('\tFirst FEC are from week ', min(allfec$Week), ' - i.e. week starting ', strftime(min(allfec$Monday)), '\n', sep='')

		dosings <- allfec %>% filter(!is.na(.data$DoseText), .data$DoseText!='') %>% group_by(.data$DoseText) %>% tally
		if(nrow(dosings)>0){
			cat('\tThe following text entries were interpreted as representing anthelmintic treatment:\n', sep='')
			for(r in seq_len(nrow(dosings))){
				cat('\t\t', dosings$DoseText[r], ' (N=', dosings$n[r], ')\n', sep='')
			}
		}

		cat('\tFinished processing ', nrow(allfec), ' FEC and/or dosing observations for ', length(unique(allfec$AnimalID)), ' animals from ', length(unique(allfec$Location)), ' locations\n', sep='')

		newani$AnimalID <- gsub('[[:space:]]','', newani$AnimalID)
		newani$Location <- gsub('[[:space:]]','', newani$Location)
		newfec$AnimalID <- gsub('[[:space:]]','', newfec$AnimalID)
		newfec$Location <- gsub('[[:space:]]','', newfec$Location)

		.self$Animal <- as_tibble(newani)
		.self$FEC <- as_tibble(newfec)

		.self$prepared$ready <- FALSE
		.self$run <- FALSE

	},

	AddWeatherSheet = function(weather_file, years='all', replace=TRUE, ...){
		"Add weather data for one or more years from either an Excel or CSV file"

	  if(is.data.frame(weather_file)){

	    weather <- weather_file

	  }else{

	    if(!is.character(weather_file) || length(weather_file)!=1 || !( grepl('\\.xlsx$', weather_file) || grepl('\\.xls$', weather_file) || grepl('\\.csv$', weather_file) ))
	      stop('The specified weather_file argument must be a length 1 character string specifying a file with a .csv, .xlsx or .xls file extension')

	    if(is.character(years)){
	      if( length(years)!=1 || !tolower(years)%in%c('all') )
	        stop('The years argument must either be the word "all" or a numeric vector specifying the years to import')
	    }else{
	      if(!is.numeric(years) || any(years < 2000) || any(years > 2050))
	        stop('The years argument must either be the words "all" or a numeric vector specifying one or more year in the range 2000-2050')
	    }

	    if(grepl('\\.csv$', weather_file)){
	      weather <- read.csv(weather_file, header=TRUE, stringsAsFactors=FALSE, ...)
	    }else{
	      weather <- read_excel(weather_file, col_types='text', col_names=TRUE, .name_repair= "unique", ...)
	    }

	  }


		if(! all(c('Year','Week','Month','Day','Temp_low','Temp_avg','Temp_high','Rel_Humidity_avg','Abs_Humidity_avg') %in% names(weather)) )
			stop('One or more required column Year, Week, Month, Day, Temp_low, Temp_avg, Temp_high, Rel_Humidity_avg & Abs_Humidity_avg was missing')

		for(col in c('Week','Month','Year')){
			weather[[col]] <- as.numeric(weather[[col]])
			if(any(is.na(weather[[col]]))){
				stop('One or more missing value found (in the ', col, ' column)')
			}
		}

		# Weeks outside the range -40 : 53 are not supported:
		if(min(weather$Week) < -40 || max(weather$Week) > 53){
			stop('Weeks before -40 or after 53 are not allowed in the weather recordings')
		}
		if(min(weather$Month) < 1 || max(weather$Month) > 12){
			stop('Months before 1 or after 12 are not allowed in the weather recordings')
		}
		# Any negative weeks or weeks of 0 when month is 12 are from the previous year:
		weather$Year[weather$Week < 0] <- weather$Year[weather$Week < 0] -1
		weather$Year[weather$Week == 0 & weather$Month == 12] <- weather$Year[weather$Week == 0 & weather$Month == 12] -1

		weather <- weather %>%
			select(.data$Year, .data$Month, .data$Day, .data$Temp_low, .data$Temp_avg, .data$Temp_high, .data$Rel_Humidity_avg, .data$Abs_Humidity_avg) %>%
			filter(!is.na(.data$Day))

		weather$Date <- with(weather, as.Date(paste(Year,Month,Day,sep='-')))
		if(any(is.na(weather$Date))){
			with(weather, stop(paste0('Error processing date specified for Year-Month-Day: ', Year[1], '-', Month[1], '-', Day[1])))
		}

		for(col in names(weather %>% select(-.data$Date))){
			weather[[col]] <- as.numeric(weather[[col]])
			if(any(is.na(weather[[col]]))){
				stop('One or more missing value found (in the ', col, ' column)')
			}
		}

		if(is.numeric(years)){
			weather <- weather %>% filter(.data$Year %in% years)
			if(!all(years %in% weather$Year)){
				stop(paste0('Specified year not present in the data: ', years[which(! years %in% weather$Year)][1]))
			}
		}

		years <- unique(weather$Year)
		dmiss <- sapply(years, function(y){
			de <- seq(min((weather %>% filter(.data$Year == y))$Date), max((weather %>% filter(.data$Year == y))$Date), by="days")
			md <- de[which(!de %in% (weather %>% filter(.data$Year == y))$Date)]
			if(length(md)==0) return('ok') else return(strftime(md[1]))
		})
		if(!all(dmiss=='ok')){
			stop(paste0('Missing date in the contiguous (within-year) weather recordings: ', dmiss[which(dmiss!='ok')][1]))
		}

		repnote <- ''
		if(replace){
			newdates <- inner_join(weather, .self$Weather, by='Date')
			if(nrow(newdates) > 1){
				repnote <- paste0('\t[Note: Replaced ', nrow(newdates), ' existing weather observations]\n')
			}
			.self$Weather <- .self$Weather %>% filter(!.data$Date %in% newdates$Date)
		}

		new <- rbind(.self$Weather, weather %>% select(.data$Date, everything()))

		dups <- new %>% group_by(.data$Date) %>% tally %>% filter(.data$n > 1)
		if(nrow(dups)>0){
			stop(paste0('Multiple entries found for ', dups$Date[1], ' - set replace=TRUE to over-write existing data'))
		}

		cat('Imported weather data for ', length(years), ' years\n', repnote, sep='')

		.self$Weather <- as_tibble(new)
		.self$prepared$ready <- FALSE
		.self$run <- FALSE

	},

	SaveDataArchive = function(archive_file, years='all', overwrite=FALSE, compress = "gzip", compression_level=6){
		"Save the currently loaded location/animal/FEC/weather data into an archive file"

		if(is.character(years)){
			if( length(years)!=1 || tolower(years)!='all' )
				stop('The years argument must either be the word "all" or a numeric vector specifying the years to import')
		}else{
			if(!is.numeric(years) || any(years < 2000) || any(years > 2050))
				stop('The years argument must either be the word "all" or a numeric vector specifying one or more year in the range 2000-2050')
		}

		outlist <- list()
		if(is.numeric(years)){
			if(!all(years %in% .self$FEC$Year)){
				stop(paste0('Specified year not present in the FEC data: ', years[which(! years %in% .self$FEC$Year)][1]))
			}
			if(any(! years %in% .self$Weather$Year)){
				stop(paste0('Required year missing from the weather data: ', years[! years %in% .self$Weather$Year][1]))
			}

			outlist$Locations <- .self$Locations %>% filter(.data$Year %in% years)
			outlist$Animal <- .self$Animal %>% filter(.data$Year %in% years)
			outlist$FEC <- .self$FEC %>% filter(.data$Year %in% years)
			# Also export the year before for the weather data (rolling averages go back 6 months):
			outlist$Weather <- .self$Weather %>% filter(.data$Year %in% c(years,years-1))

		}else{

			outlist$Locations <- .self$Locations
			outlist$Animal <- .self$Animal
			outlist$FEC <- .self$FEC
			outlist$Weather <- .self$Weather

		}

		outlist$Info <- list(Version=packageVersion('DonkeyDosing'), sessionInfo=sessionInfo(), SaveDate=Sys.Date(), OriginalFilename=archive_file)

		if(file.exists(archive_file) && !overwrite){
			stop('Unable to create archive: "', archive_file, '" already exists and overwrite argument is set to FALSE')
		}

		save(list=names(outlist), file=archive_file, envir=as.environment(outlist), compress=compress, compression_level=compression_level)
		cat('Data archive saved successfully\n')
	},


	LoadDataArchive = function(archive_file, years='all'){
		"Load location/animal/FEC/weather data from an archive file"

		if(is.character(years)){
			if( length(years)!=1 || tolower(years)!='all' )
				stop('The years argument must either be the word "all" or a numeric vector specifying the years to import')
		}else{
			if(!is.numeric(years) || any(years < 2000) || any(years > 2050))
				stop('The years argument must either be the word "all" or a numeric vector specifying one or more year in the range 2000-2050')
		}

		container <- environment()
		names <- load(archive_file, envir=container)
		if(!all(c("Locations", "Animal", "FEC", "Weather", "Info") %in% names))
			stop('There was a problem loading the archive - perhaps it was not saved using SaveDataArchive() or it has become corrupted?')

		# Check location data:
		if(is.character(years)){
			newdat <- rbind(.self$Locations, container$Locations)
		}else{
			if(any(! years %in% container$Locations$Year))
				stop('Specified year missing from the location data: ', years[which(! years %in% container$Locations$Year)][1])

			newdat <- rbind(.self$Locations, container$Locations %>% filter(.data$Year %in% years))
		}
		test <- newdat %>% group_by(.data$Farm, .data$Location, .data$Year) %>% tally %>% filter(n > 1)
		if(any(nrow(test) > 0)){
			stop('Duplicated farm - location - year combination: ', test$Farm[1], ' - ', test$Location[1], ' - ', test$Year[1])
		}
		newloc <- newdat

		# Check animal data:
		if(is.character(years)){
			newdat <- rbind(.self$Animal, container$Animal)
		}else{
			if(any(! years %in% container$Animal$Year))
				stop('Specified year missing from the animal data: ', years[which(! years %in% container$Animal$Year)][1])

			newdat <- rbind(.self$Animal, container$Animal %>% filter(.data$Year %in% years))
		}
		test <- newdat %>% group_by(.data$Farm, .data$Location, .data$AnimalID, .data$Year) %>% tally %>% filter(n > 1)
		if(any(nrow(test) > 0)){
			stop('Duplicated farm - location - animalID - year combination: ', test$Farm[1], ' - ', test$Location[1], ' - ', test$AnimalID[1], ' - ', test$Year[1])
		}
		newani <- newdat

		# Check FEC data:
		if(is.character(years)){
			newdat <- rbind(.self$FEC, container$FEC)
		}else{
			if(any(! years %in% container$FEC$Year))
				stop('Specified year missing from the FEC data: ', years[which(! years %in% container$FEC$Year)][1])

			newdat <- rbind(.self$FEC, container$FEC %>% filter(.data$Year %in% years))
		}
		test <- newdat %>% group_by(.data$Location, .data$AnimalID, .data$Monday) %>% tally %>% filter(n > 1)
		if(any(nrow(test) > 0)){
			stop('Duplicated location - animalID - week date combination: ', test$Location[1], ' - ', test$AnimalID[1], ' - ', strftime(test$Monday[1]))
		}
		newfec <- newdat

		# Check Weather data:
		if(is.character(years)){
			newdat <- rbind(.self$Weather, container$Weather)
		}else{
			if(any(! years %in% container$Weather$Year))
				stop('Specified year missing from the weather data: ', years[which(! years %in% container$Weather$Year)][1])

			# Also export the year before for the weather data (rolling averages go back 6 months):
			newdat <- rbind(.self$Weather, container$Weather %>% filter(.data$Year %in% c(years,years-1)))
		}
		dups <- newdat %>% group_by(.data$Date) %>% tally %>% filter(.data$n > 1)
		if(nrow(dups)>0){
			stop(paste0('Duplicate date found in the weather data: ', dups$Date[1]))
		}

		.self$Weather <- as_tibble(newdat)
		.self$Locations <- as_tibble(newloc)
		.self$Animal <- as_tibble(newani)
		.self$FEC <- as_tibble(newfec)

		.self$prepared$ready <- FALSE

		cat('Data archive loaded successfully\n')

	},

	FitPredictionModel = function(formula = NA, years='last10', comparison=FALSE){
		"Fit the prediction model"

		if(identical(formula, NA)){

			# Gives the best balance of interpretability and consistency with vs without 2018 data:
			formula <- meanLogFEC ~ (1 | Year) + (1 | LocationYear) +
			    Farm + Hygiene + log(TotalAnimals) +
				DoseProp4 + DoseProp8 + DoseProp12 +
				Temp_avg_16 + I(Temp_avg_16^2/10) + Temp_avg_20 + I(Temp_avg_20^2/10) +
				AbsHumid_16 + I(AbsHumid_16^2/10) + AbsHumid_20 + I(AbsHumid_20^2/10) +
			    Temp_frost_8 + I(Temp_frost_8^2/10) + Temp_frost_12 + I(Temp_frost_12^2/10) +
			    Temp_frost_16 + I(Temp_frost_16^2/10) +
				sdoy + cdoy

				# Give problems with singular fit when excluding location/years, and also not really supported:
			    # + Hygiene:sdoy + Hygiene:cdoy + (1 | FarmHygiene)
				# NB: random effect of farmhygiene would also be problematic if a farm without e.g. manual had it next year
		}

	  # By default include only last 10 years of data:
	  if(length(years)==1 && years=="last10"){
	    current_year <- as.numeric(format(Sys.Date(), "%Y"))
	    years <- seq(current_year-11, current_year-1, by=1)
	    years_available <- .self$FEC %>% distinct(.data$Year) %>% pull(.data$Year) %>% as.numeric()
	    years <- years[years %in% years_available]
	  }

		# Need to run the function to reset the data if needed:
		gpdata <- .self$GetGroupModelData(years=years)
		min_N <- .self$prepared$min_N
		expected_efficacy <- .self$prepared$expected_efficacy
		min_week <- .self$prepared$min_week
		max_week <- .self$prepared$max_week
		indat <- .self$IndividualModelData %>% filter(.data$Year %in% unique(gpdata$Year))

		# All years:
		ww <- capture_warnings({
		predmods <- get_prediction_models(gpdata, indat, .self$RollingWeather, formula, expected_efficacy=expected_efficacy, min_N=min_N, min_week=min_week, max_week=max_week, maxiters=1000, tol=10^-3, txtime=0, txrm=FALSE, lastyear=TRUE, individual_intercept=FALSE, resid_doy_interaction=FALSE, common_re=TRUE)
			# See inside the function for meanings of these additional arguments
		})

		tosave <- list()
		tosave <- list(allyears=predmods, nolastyear=list(), comparison=comparison, expected_efficacy=expected_efficacy, min_week=min_week, max_week=max_week)
		.self$predictions <- tosave
		.self$run <- TRUE

		# Omitting the last year:
		if(comparison){
			useyear <- sort(as.numeric(as.character(unique(gpdata$Year))))
			useyear <- useyear[-(length(useyear))]  # Drop the last year
			useyear <- factor(useyear, levels=levels(gpdata$Year))
			stopifnot(all(!is.na(useyear)))
			if(length(useyear)<2){
				stop('The model comparison option requires 3 years of data')
			}
			ww <- c(ww, capture_warnings({
			 predmods_comp <- get_prediction_models(gpdata %>% filter(.data$Year %in% useyear), .self$IndividualModelData %>% filter(.data$Year %in% useyear), .self$RollingWeather, formula, expected_efficacy=expected_efficacy, min_N=min_N, min_week=min_week, max_week=max_week, maxiters=1000, tol=10^-3, txtime=0, txrm=FALSE, lastyear=TRUE, individual_intercept=FALSE, resid_doy_interaction=FALSE, common_re=TRUE)
		  }))

			tosave$nolastyear <- predmods_comp
			.self$predictions <- tosave
		}

		if(length(ww)>0){
		  if(length(ww)==1){
		    msg <- str_c("The following warning was obtained when fitting the models:\n", ww)
		  }else{
		    msg <- str_c("The following warnings were obtained when fitting the models:\n", str_c(ww, collapse="\n"))
		  }
		  warning(msg)
		}

		invisible(tosave)

	},

	LoadPredictionModel = function(saved){
	  "This function is only needed for testing purposes"

		.self$predictions <- saved
		.self$run <- TRUE

	},

	GetPlots = function(farm, year="latest", file="", ...){
	  "Extract plots showing model fit/predictions for each farm and year requested"

	  if(!.self$run){
	    stop('No coefficients available for writing: run FitPredictionModel() first')
	  }

	  pd <- .self$predictions$allyears$group_data_output
	  comb <- pd$pred %>%
	    select(.data$Year, .data$Farm, .data$Week, .data$Hygiene, Predicted = .data$PredExclusion) %>%
	    filter(!is.na(.data$Predicted)) %>%
	    right_join(
	      pd$obs %>% distinct(.data$Year, .data$Farm, .data$Hygiene, .data$Location),
	      by=c("Year","Farm","Hygiene"),
	      relationship = "many-to-many"
	    ) %>%
	    full_join(
	      pd$obs %>%
	        mutate(Observed = exp(.data$meanLogFEC)-1L) %>%
	        select(.data$Year, .data$Farm, .data$Week, .data$Hygiene, .data$Location, .data$Observed, .data$TotalFEC),
	      by = c("Year","Farm","Week","Hygiene","Location")
	    )

	  ## Calculate average mean sizes:
	  sizes <- comb %>%
	    filter(!is.na(Observed)) %>%
	    group_by(Year, Farm, Location) %>%
	    summarise(MeanTotalFEC = round(mean(TotalFEC),1), .groups="drop")

	  comb <- comb %>%
	    left_join(sizes, by=c("Year","Farm","Location"))

	  if("all" %in% farm) farm <- unique(comb$Farm)
	  farm <- unique(farm)
	  if(any(!farm %in% unique(comb$Farm))) stop("Unrecognised farm:  specify farms by name, or use 'all'")

	  if("all" %in% year) year <- unique(comb$Year)
	  year[year=="latest"] <- as.character(max(as.numeric(as.character(comb$Year))))
	  year <- unique(year)
	  if(any(!year %in% unique(comb$Year))) stop("Invalid year:  specify years manually, or use 'all' or 'latest'")

	  plotfun <- function(f, y){
	    pd <- comb %>%
	      filter(Farm==f, Year==y)

	    pd$Location <- paste0(gsub(paste0(f, "_"), "", pd$Location), " (", pd$MeanTotalFEC, ")")

	    pt <- ggplot(pd, aes(x=.data[["Week"]])) +
	      geom_line(aes(y=.data[["Predicted"]])) +
	      geom_point(aes(y=.data[["Observed"]]), pd %>% filter(!is.na(.data$Observed))) +
	      facet_wrap(~Location) +
	      theme_light() +
	      ylab("FEC") +
	      ggtitle(paste0(f, " - ", y))

	    pt
	  }

	  if(file==""){
	    if(length(farm)>1 || length(year)>1) stop("Specify a filename to generate plots for multiple farms and/or years")

	    return(plotfun(farm, year))
	  }

	  if(!grepl("\\.pdf$", file)) stop("Invalid file - must end with .pdf")
	  pdf(file, ...)
	  expand_grid(Farm=farm, Year=year) %>%
	    group_split(Farm, Year) %>%
	    pblapply(function(x){
	      print(plotfun(x$Farm, x$Year))
	    })
    dev.off()

    invisible(
      comb %>%
        filter(.data$Farm %in% farm, .data$Year %in% year) %>%
        mutate(Location = str_replace(.data$Location, paste0(.data$Farm, "_"), "")) %>%
        select("Year", "Farm", "Location", "Hygiene", "Week", "Predicted", "Observed", N="TotalFEC") %>%
        arrange(.data$Year, .data$Farm, .data$Location, .data$Week)
    )
	},

	GetCoefficients = function(write=TRUE, coefs_file='effect_etimates.csv', sexcodes_file='sex_codes.csv', PMcoefs=system.file('extdata/PMcoefs.Rdata', package='DonkeyDosing')){
		"Extract coefficient estimates for entering into the dosing tool"

		if(!.self$run){
			stop('No coefficients available for writing: run FitPredictionModel() first')
		}

		# Load and check the PM data coefficients:
		if(is.character(PMcoefs)){
			stopifnot(length(PMcoefs)==1)
			env <- new.env()
			load(PMcoefs, envir=env)
			PMcoefs <- as.list(env)
		}else{
			if(!is.list(PMcoefs))
				stop('The argument supplied to PMcoefs must either be a named of a file to be load()ed or a list')
		}

		if(!all(c("pmcoefs", "threshold") %in% names(PMcoefs)))
			stop('The PMcoefs file/list must contain both pmcoefs and threshold in the same format as given by (load("', system.file('extdata/PMcoefs.Rdata', package='DonkeyDosing'), '"))')

		if(!is.numeric(PMcoefs$threshold) || length(PMcoefs$threshold)!=1)
			stop('The PMcoefs file/list must contain threshold in the same format as given by (load("', system.file('extdata/PMcoefs.Rdata', package='DonkeyDosing'), '"))')

		if(!is.data.frame(PMcoefs$pmcoefs) || !all(c('term','estimate') %in% names(PMcoefs$pmcoefs)) || !all(c("PM_Intercept","PM_AgeCatAge_0-5","PM_AgeCatAge_11-20","PM_AgeCatAge_21+","PM_AgeCatAge_6-10","PM_cdoy","PM_sdoy","PM_I(meanFEC/1000)","PM_SexDKF","PM_SexDKM","PM_SexMUPY") == (PMcoefs$pmcoefs$term)))
			stop('The PMcoefs file/list must contain pmcoefs in the same format as given by (load("', system.file('extdata/PMcoefs.Rdata', package='DonkeyDosing'), '"))')

		output <- format_coefficients(.self$GroupModelData, .self$IndividualModelData, .self$predictions$allyears, .self$predictions$nolastyear, .self$predictions$comparison, .self$predictions$expected_efficacy, PMcoefs$pmcoefs, PMcoefs$threshold)

		if(write){
			write.table(output, file=coefs_file, col.names=FALSE, row.names=FALSE, sep=',', na='')

			# Get sex codes:
			sexcodes <- dosing_getOption('allsexes')
			sexcodes <- rbind(data.frame(Sex='', SexCategory=''), data.frame(Sex=names(sexcodes), SexCategory=paste0('Sex',sexcodes)) %>% arrange(SexCategory, Sex))
			write.table(sexcodes, file=sexcodes_file, col.names=TRUE, row.names=FALSE, sep=',', na='')
		}

		return(output)
	},

	GetPredictions = function(year, farm, excel_file, excel_sheet=farm, weeks='all', dosing_thresholds=c(600,1000,2000), prob_threshold=0.35, PMcoefs=system.file('extdata/PMcoefs.Rdata', package='DonkeyDosing'), cl=getOption("mc.cores", 2L)){
		"Extract predictions for checking model estimates"

		if(!.self$run){
			stop('Unable to make predictions as the model has not yet been run')
		}

		# TODO: check year is present in weather data, then re-calculate rolling weather - addweathersheet should not set run=FALSE but maybe note if run=TRUE and weather added
		# TODO: Check that the weather data extends as far back and forward as necessary for the fec data
		# TODO: process weeks argument and pass to underlying function

		stopifnot(length(dosing_thresholds)==3 && all(dosing_thresholds>0))
		stopifnot(all(dosing_thresholds==sort(dosing_thresholds)))
		names(dosing_thresholds) <- c('VeryHigh','High','Normal')
		stopifnot(length(prob_threshold)==1 && prob_threshold>0 && prob_threshold<1)

		cat('Processing data to make predictions for ', farm, ' (', year, ')...\n', sep='')

		all_locations <- .self$Locations
		rvals <- dosing_sheet(year=year, farm=farm, excel_file=excel_file, excel_sheet=excel_sheet, all_locations=all_locations)

		newani <- rvals$allanimals
		test <- newani %>% group_by(.data$Farm, .data$Location, .data$AnimalID, .data$Year) %>% tally %>% filter(n > 1)
		if(any(nrow(test) > 0)){
			stop('Duplicated farm - location - animalID - year combination: ', test$Farm[1], ' - ', test$Location[1], ' - ', test$AnimalID[1], ' - ', test$Year[1])
		}

		newfec <- rvals$allfec
		test <- newfec %>% group_by(.data$Location, .data$AnimalID, .data$Monday) %>% tally %>% filter(n > 1)
		if(any(nrow(test) > 0)){
			stop('Duplicated location - animalID - week date combination: ', test$Location[1], ' - ', test$AnimalID[1], ' - ', strftime(test$Monday[1]))
		}

		cat('\tFirst FEC are from week ', min(newfec$Week), ' - i.e. week starting ', strftime(min(newfec$Monday)), '\n', sep='')

		dosings <- newfec %>% filter(!is.na(.data$DoseText), .data$DoseText!='') %>% group_by(.data$DoseText) %>% tally
		if(nrow(dosings)>0){
			cat('\tThe following text entries were interpreted as representing anthelmintic treatment:\n', sep='')
			for(r in seq_len(nrow(dosings))){
				cat('\t\t', dosings$DoseText[r], ' (N=', dosings$n[r], ')\n', sep='')
			}
		}

		cat('\tFinished processing ', nrow(newfec), ' FEC and/or dosing observations for ', length(unique(newfec$AnimalID)), ' animals from ', length(unique(newfec$Location)), ' locations\n', sep='')

		newani$AnimalID <- gsub('[[:space:]]','', newani$AnimalID)
		newani$Location <- gsub('[[:space:]]','', newani$Location)
		newfec$AnimalID <- gsub('[[:space:]]','', newfec$AnimalID)
		newfec$Location <- gsub('[[:space:]]','', newfec$Location)

		# Merge fec, location and ani data, then remove non-locations:
		fec_data <- newfec %>%
			left_join(newani, by = c("AnimalID", "Location", "Year")) %>%
			left_join(.self$Locations, by = c("Location", "Year", "Farm", "Field")) %>%
			filter(!is.na(.data$Location))

		#TODO: adapt rollingweather so that it generates up to its max week + 4 but doesn't give values_0 (with argument for min historic??)
		rollingweather <- .self$RollingWeather

		# TODO: change once week argument is implemented:
		if(max(rollingweather$Week[rollingweather$Year==year]) < max(fec_data$Week+4)){
			warning(paste0('Limiting the predictions to <= week ', max(rollingweather$Week[rollingweather$Year==year]), ' due to limited availability of weather data'))
			fec_data <- fec_data %>% filter((Week+4) <= max(rollingweather$Week[rollingweather$Year==as.character(year)]))
		}

		predmods <- .self$predictions
		min_week <- predmods$min_week
		max_week <- predmods$max_week

		numani <- newani %>%
		  filter(!is.na(Location)) %>%
		  group_by(Farm, Location, Year) %>%
		  summarise(TotalAnimals=n(), MeanAge=mean(Age,na.rm=TRUE))

		samplings <- fec_data %>%
			filter(!is.na(Hygiene), !is.na(FEC)) %>%
			filter(Week >= min_week, Week <= max_week) %>%
			group_by(Farm, Location, Year, Hygiene, Week) %>%
			tally() %>%
		  left_join(numani, by = c("Farm", "Location", "Year")) %>%
			ungroup()

		predout <- forward_predict(samplings=samplings, fec_data=fec_data, rollingweather=rollingweather, models=predmods$allyears, expected_efficacy=predmods$expected_efficacy, threshold=max(dosing_thresholds), min_week=min_week, max_week=max_week, year=year, cl=cl)

		# Then call getcoefs so we can extract the PM model:
		coefs <- .self$GetCoefficients(write=FALSE, PMcoefs=PMcoefs)
		avfec <- exp(coefs[coefs[,1]=='feclastyearmean',2])-1
		coefn <- coefs[,1]
		coefs <- coefs[grepl('PM_',coefn,fixed=TRUE),2]
		names(coefs) <- coefn[grepl('PM_',coefn,fixed=TRUE)]

		# Then get the current observed FEC, clinical risk and demographic risk corresponding to 3-5 weeks before the prediction weeks:
		anidat <- samplings %>%
			# This is all relevant to the current week, not the prediction week:
			mutate(Week=.data$Week-4) %>%
			left_join(.self$Animal, by = c("Farm", "Location", "Year")) %>% left_join(.self$FEC, by = c("Location", "Year", "Week", "AnimalID")) %>%
			mutate(AvFEC = ifelse(is.na(.data$AvFEC), avfec, .data$AvFEC)) %>%
			mutate(DemographicProb = coefs['PM_Intercept'] + coefs[paste0('PM_AgeCat',as.character(.data$AgeCat))] + coefs['PM_cdoy']*cos(2*pi*.data$Week/52) + coefs['PM_sdoy']*sin(2*pi*.data$Week/52) + coefs['PM_I(meanFEC/1000)']*.data$AvFEC/1000 + coefs[paste0('PM_Sex',as.character(.data$Sex))]) %>%
			mutate(ClinicalRisk=as.character(ClinicalRisk), DemographicRisk=ifelse(plogis(DemographicProb)>coefs['PM_threshold'], 'Elevated', ''), OverallRisk=ifelse(ClinicalRisk=='Very High', ClinicalRisk, ifelse(DemographicRisk=='Elevated', 'High', ClinicalRisk))) %>%
			select(.data$Farm, .data$Location, .data$Hygiene, .data$TotalAnimals, .data$MeanAge, .data$Week, .data$AnimalID, .data$Tag, .data$DemographicProb, .data$ClinicalRisk, .data$DemographicRisk, .data$OverallRisk)

		stopifnot(all(!is.na(anidat$OverallRisk)))

		allpreds <- predout %>%
			# Turn the week back into the current week, not the prediction week:
			mutate(Week = .data$Week-4) %>%
			inner_join(anidat, by = c("Farm", "Location", "Week", "AnimalID")) %>%
			mutate(DemographicProb = round(.data$DemographicProb,2)) %>%
			mutate(LastDose = ifelse(.data$LastDose > 52, "None", paste0('Week ', .data$Week+4-.data$LastDose))) %>%
			mutate(ModelDose = ifelse(.data$ThresholdProbability >= prob_threshold, TRUE, FALSE)) %>%
			mutate(IndividDose = as.logical(ifelse(!is.na(.data$LastFEC) & .data$LastFEC >= dosing_thresholds[.data$OverallRisk], TRUE, FALSE))) %>%
		  ungroup

		formatpreds <- allpreds %>%
			select(.data$Farm, .data$Location, .data$Week, Donkey=.data$AnimalID, .data$Tag, .data$DemographicProb, .data$ClinicalRisk, .data$DemographicRisk, .data$OverallRisk, .data$LastFEC, .data$LastDose, PredMean=.data$PredictedAnimalMean, Pthresh=.data$ThresholdProbability)

		stopifnot(all(!is.na(allpreds$ModelDose)))
		stopifnot(all(!is.na(allpreds$IndividDose)))

		formatpreds$Donkey <- gsub('.*\\_','',formatpreds$Donkey)
		.self$probabilities <- list(done=TRUE, raw=predout, processed=allpreds, formatted=formatpreds, dosing_thresholds=dosing_thresholds, prob_threshold=prob_threshold)

		return(.self$probabilities)
	},

	GetGroupModelData = function(years='all', force_reprocess=FALSE){
		"Prepare and retrieve the currently loaded location/animal/FEC/weather data for modelling"

		if(is.character(years)){
			if( length(years)!=1 || tolower(years)!='all' )
				stop('The years argument must either be the word "all" or a numeric vector specifying the years to import')
		}else{
			if(!is.numeric(years) || any(years < 2000) || any(years > 2050))
				stop('The years argument must either be the word "all" or a numeric vector specifying one or more year in the range 2000-2050')
		}

		min_N <- dosing_getOption('min_N')
		stopifnot(length(min_N)==1 && is.numeric(min_N) && min_N > 0)
		expected_efficacy <- dosing_getOption('expected_efficacy')
		stopifnot(length(expected_efficacy)==1 && is.numeric(expected_efficacy) && expected_efficacy > 0 && expected_efficacy < 1)
		min_week <- dosing_getOption('min_week')
		stopifnot(length(min_week)==1 && is.numeric(min_week) && min_week > 0)
		max_week <- dosing_getOption('max_week')
		stopifnot(length(max_week)==1 && is.numeric(max_week) && max_week > 0)

		if(nrow(.self$Locations)==1)
			stop('Unable to prepare model data:  no location information has been imported')
		if(nrow(.self$Animal)==1 || nrow(.self$FEC)==1)
			stop('Unable to prepare model data:  no animal/FEC information has been imported')
		if(nrow(.self$Weather)==1)
			stop('Unable to prepare model data:  no weather information has been imported')

		fecyrs <- unique(.self$FEC$Year)
		if(is.numeric(years)){
			if(!all(years %in% fecyrs)){
				stop(paste0('Specified year not present in the FEC data: ', years[which(! years %in% fecyrs)][1]))
			}
			years <- sort(unique(years))
		}else{
			years <- fecyrs
		}

		if(any(! years %in% .self$Weather$Year)){
			stop(paste0('Required year missing from the weather data: ', years[! years %in% .self$Weather$Year][1]))
		}

		if(length(years)<2){
		  stop('A minimum of 2 years must be specified')
		}
		if(!all(years %in% min(as.numeric(years)):max(as.numeric(years)))){
			stop('Non-consecutive years are not supported')
		}

		# See if the data is already ready:
		preptest <- list(ready=TRUE, years=years, expected_efficacy=expected_efficacy, min_N=min_N, min_week=min_week, max_week=max_week)
		if(!force_reprocess && identical(preptest, .self$prepared)){
			cat('Retrieving previously prepared data consisting of a total of ', nrow(.self$GroupModelData), ' average FEC observations\n', sep='')
			return(.self$GroupModelData)
		}

		# Process the data into the format necessary for the models:
		cat('Processing model data for ', length(years), ' years...\n', sep='')

		rollingweather <- process_weather(.self$Weather, min_year=min(years))

		meanfec <- .self$FEC %>%
			filter(!is.na(.data$Location)) %>%
			## Only run the model for March to Nov as Mox treatment in Dec/Jan may not be universally recorded, and dosing tool is not to be used Jan/Feb anyway:
			filter(.data$Week >= min_week, .data$Week <= max_week, !is.na(.data$Location)) %>%
			group_by(.data$Location, .data$Year, .data$Week, .data$Monday) %>%
			filter(!is.na(.data$FEC)) %>%
			summarise(meanLogFEC = mean(log(.data$FEC + 1)), DosedWithin4 = sum(.data$LastDose <= 4), DosedWithin8 = sum(.data$LastDose <= 8)-.data$DosedWithin4, DosedWithin12 = sum(.data$LastDose <= 12)-(.data$DosedWithin4+.data$DosedWithin8), TotalFEC = n())
		stopifnot(all(meanfec$DosedWithin4 >= 0))
		stopifnot(all(meanfec$DosedWithin8 >= 0))
		stopifnot(all(meanfec$DosedWithin12 >= 0))

		# Merge with cleaning etc data:
		locations <- .self$Animal %>%
			filter(!is.na(.data$Location)) %>%
			left_join(.self$Locations, by = c("Year", "Farm", "Location")) %>%
		 	group_by(.data$Farm, .data$Location, .data$Year, .data$Hygiene, .data$Sensitivity) %>%
			summarise(MeanAge = mean(.data$Age, na.rm=TRUE), TotalAnimals = n())

		meanfec <- meanfec %>%
			left_join(locations, by = c("Location", "Year")) %>%
			filter(.data$Week >= min_week, .data$Week <= max_week) %>%
			mutate(sdoy = sin(2*pi*Week/52), cdoy = cos(2*pi*Week/52))
		stopifnot(nrow(meanfec %>% group_by(Farm,Year))>1)
		stopifnot(all(meanfec$TotalAnimals >= meanfec$TotalFEC))
		stopifnot(all(meanfec$TotalAnimals >= (meanfec$DosedWithin4 + meanfec$DosedWithin8 + meanfec$DosedWithin12)))

			# Don't filter by minN here - let the group model do it:
			# %>% filter(.data$TotalFEC >= min_N)

		# This reflects the expected reduction in mean count due to dosing proportion of animals (NOT TotalFEC):
		red <- 1-expected_efficacy
		meanfec <- meanfec %>%
			mutate(DoseProp4 = log(1 * (.data$TotalAnimals-.data$DosedWithin4)/.data$TotalAnimals + red * .data$DosedWithin4/.data$TotalAnimals)) %>%
			mutate(DoseProp8 = log(1 * (.data$TotalAnimals-.data$DosedWithin8)/.data$TotalAnimals + red * .data$DosedWithin8/.data$TotalAnimals)) %>%
			mutate(DoseProp12 = log(1 * (.data$TotalAnimals-.data$DosedWithin12)/.data$TotalAnimals + red * .data$DosedWithin12/.data$TotalAnimals)) %>%
			left_join(rollingweather, by = c("Year", "Week", "Monday"))

		allmodeldata <- meanfec %>%
			filter(.data$Year %in% years)

		allmodeldata$FarmHygiene <- with(allmodeldata, interaction(Farm, Hygiene, sep='_'))
		allmodeldata$LocationYear <- with(allmodeldata, interaction(Location, Year, sep='_'))

		allmodeldata <- allmodeldata %>%
			ungroup %>%
			select(.data$meanLogFEC, .data$TotalFEC, .data$Year, .data$LocationYear, .data$Location, .data$FarmHygiene, .data$Monday, .data$Week, .data$sdoy, .data$cdoy, .data$Farm, .data$Hygiene, .data$MeanAge, .data$TotalAnimals, starts_with('DoseProp'), starts_with('Temp'), starts_with('RelHumid'), starts_with('AbsHumid'))

		# Formats for the model:
		allyears <- sort(unique(c(years, rollingweather$Year))) # RollingWeather may have more years of data than the model data - used for later prediction
		allmodeldata$Year <- factor(allmodeldata$Year, levels=as.character(allyears), labels=as.character(allyears))  # Note: the labels must be able to be converted back to year using as.numeric(as.character(levels()))
		stopifnot(is.factor(allmodeldata$LocationYear))
		allmodeldata$Location <- factor(allmodeldata$Location)
		allmodeldata$FarmHygiene <- factor(allmodeldata$FarmHygiene)

		allmodeldata$Farm <- factor(allmodeldata$Farm)
		maxobs <- (allmodeldata %>%
			group_by(Farm) %>%
			tally %>%
			arrange(desc(n)) %>%
			slice(1))$Farm
		allmodeldata$Farm <- relevel(allmodeldata$Farm, as.character(maxobs))
		stopifnot(is.factor(allmodeldata$Hygiene))

		# Also prepare the individual model data here as we have easy access to rollingweather etc:
		## Add necessary information to the FEC data to get model predictions
		allanimals <- .self$Animal %>%
			filter(!is.na(.data$Location)) %>%
			left_join(locations, by = c("Year", "Farm", "Location")) %>%
			ungroup()

		indmod <- .self$FEC %>%
			filter(.data$Week >= min_week, .data$Week <= max_week, !is.na(.data$Location)) %>%
			left_join(allanimals, by = c("AnimalID", "Location", "Year")) %>%
			mutate(LocationYear=NA, FarmHygiene=NA, sdoy=NA, cdoy=NA) %>%
			left_join(rollingweather, by = c("Year", "Week", "Monday")) %>%
			filter(!is.na(.data$FEC)) %>%
			ungroup()

		indmod$LocationYear <- paste(indmod$Location, indmod$Year, sep='_')
		indmod$FarmHygiene <- paste(indmod$Farm, indmod$Hygiene, sep='_')
		indmod$sdoy <- sin(2*pi*indmod$Week/52)
		indmod$cdoy <- cos(2*pi*indmod$Week/52)

		# Get dose proportion based on if the animal was dosed:
		indmod$DoseProp4 <- 0
		indmod$DoseProp8 <- 0
		indmod$DoseProp12 <- 0
		indmod$GroupPredictionNoTx <- NA
		dosed4 <- indmod$LastDose <= 4
		indmod$DoseProp4[dosed4] <- log(red)
		dosed8 <- indmod$LastDose <= 8 & !dosed4
		indmod$DoseProp8[dosed8] <- log(red)
		dosed12 <- indmod$LastDose <= 12 & !dosed8
		indmod$DoseProp12[dosed12] <- log(red)
		indmod$GroupPredictionIncTx <- NA

		# Categorise the last dosing:
		indmod$LastDoseCat <- relevel(cut(indmod$LastDose, breaks=c(0.5, 4.5, 8.5, 12.5, Inf), labels=c('1-4','5-8','9-12','>12')), '>12')
		stopifnot(all(!is.na(indmod$LastDoseCat)))  # LastDose should never be 0

		# Factorise things:
		indmod$Year <- factor(indmod$Year, levels=as.character(allyears), labels=as.character(allyears))  # Note: the labels must be able to be converted back to year using as.numeric(as.character(levels()))
		indmod$Farm <- factor(indmod$Farm, levels=levels(allmodeldata$Farm))
		indmod$FarmHygiene <- factor(indmod$FarmHygiene, levels=levels(allmodeldata$FarmHygiene))
		indmod$Location <- factor(indmod$Location, levels=levels(allmodeldata$Location))
		indmod$LocationYear <- factor(indmod$LocationYear, levels=levels(allmodeldata$LocationYear))
		indmod$Hygiene <- factor(indmod$Hygiene, levels=levels(allmodeldata$Hygiene))
		rollingweather$Year <- factor(rollingweather$Year, levels=as.character(allyears), labels=as.character(allyears))  # Note: the labels must be able to be converted back to year using as.numeric(as.character(levels()))
		stopifnot(identical(levels(indmod$Year), levels(allmodeldata$Year)))
		stopifnot(identical(levels(indmod$Year), levels(rollingweather$Year)))

		cat('The data consists of a total of ', nrow(allmodeldata), ' average FEC observations\n', sep='')

		.self$prepared <- preptest
		.self$GroupModelData <- as_tibble(allmodeldata)
		.self$IndividualModelData <- as_tibble(indmod)
		.self$RollingWeather <- as_tibble(rollingweather)

		stopifnot(all(!is.na(allmodeldata$Farm)))
		stopifnot(all(!is.na(indmod$Farm)))
		stopifnot(all(!is.na(allmodeldata$Location)))
		stopifnot(all(!is.na(allmodeldata$Hygiene)))

		return(.self$GroupModelData)

	}

))
